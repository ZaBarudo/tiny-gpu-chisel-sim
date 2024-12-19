package controller

import chisel3._
import chisel3.util._

object ControlState extends ChiselEnum {
  val IDLE           = Value("b000".U)
  val READ_WAITING   = Value("b010".U)
  val WRITE_WAITING  = Value("b011".U)
  val READ_RELAYING  = Value("b100".U)
  val WRITE_RELAYING = Value("b101".U)
}

class Controller(
  AddrBits:     Int = 8,
  DataBits:     Int = 16,
  NumConsumers: Int = 4,
  NumChannels:  Int = 1,
  WriteEnable:  Int = 1)
    extends Module {
  // TODO: Use decoupledIO interface for consumer and memory
  val io = IO(new Bundle {
    // Consumer Interface (Fetchers / LSUs)
    val consumer_read_valid    = Input(UInt(NumConsumers.W))
    val consumer_read_address  = Input(UInt(AddrBits.W))
    val consumer_read_ready    = Output(Vec(NumConsumers, Bool()))
    val consumer_read_data     = Output(Vec(NumConsumers, UInt(DataBits.W)))
    val consumer_write_valid   = Input(UInt(NumConsumers.W))
    val consumer_write_address = Input(Vec(NumConsumers, UInt(AddrBits.W)))
    val consumer_write_data    = Input(Vec(NumConsumers, UInt(DataBits.W)))
    val consumer_write_ready   = Output(Vec(NumConsumers, Bool()))

    // Memory Interface (Data / Program)
    val mem_read_valid    = Output(Vec(NumChannels, Bool()))
    val mem_read_address  = Output(Vec(NumChannels, UInt(AddrBits.W)))
    val mem_read_ready    = Input(UInt(NumChannels.W))
    val mem_read_data     = Input(Vec(NumChannels, UInt(DataBits.W)))
    val mem_write_valid   = Output(Vec(NumChannels, Bool()))
    val mem_write_address = Output(Vec(NumChannels, UInt(AddrBits.W)))
    val mem_write_data    = Output(Vec(NumChannels, UInt(DataBits.W)))
    val mem_write_ready   = Input(UInt(NumChannels.W))
  })

  val mem_read_valid    = RegInit(false.B)
  val mem_read_address  = RegInit(0.U(AddrBits.W))
  val mem_write_valid   = RegInit(false.B)
  val mem_write_address = RegInit(0.U(AddrBits.W))
  val mem_write_data    = RegInit(0.U(DataBits.W))

  val consumer_read_ready  = RegInit(false.B)
  val consumer_read_data   = RegInit(0.U(DataBits.W))
  val consumer_write_ready = RegInit(false.B)

  val current_consumer = RegInit(VecInit(Seq.fill(log2Ceil(NumConsumers))(0.U(NumChannels.W))))
  val current_state    = RegInit(VecInit(Seq.fill(3)(ControlState.IDLE)))

  val channel_serving_consumer = RegInit(0.U(NumConsumers.W))

  when(!reset.asBool) {
    // For each channel, we handle processing concurrently
    for (i <- 0 until NumChannels) {
      switch(current_state(i)) {
        is(ControlState.IDLE) {
          for (j <- 0 until NumConsumers) {
            // While this channel is idle, cycle through consumers looking for one with a pending request
            when(io.consumer_read_valid(j) && channel_serving_consumer(j)) {
              channel_serving_consumer(j) := true.B
              current_consumer(i)         := j.U

              mem_read_valid(i)   := true.B
              mem_read_address(i) := io.consumer_read_address(j)
              current_state(i)    := ControlState.READ_WAITING
              // Once we find a pending request, pick it up with this channel and stop looking for requests
              break
            }.elsewhen((io.consumer_write_valid(j) && channel_serving_consumer(j)) {
              channel_serving_consumer(j) := true.B
              current_consumer(i)         := j.U

              mem_write_valid(i)   := true.B
              mem_write_address(i) := io.consumer_write_address(j)
              mem_write_data(i)    := io.consumer_write_data(j)
              current_state(i)     := ControlState.WRITE_WAITING
              // Once we find a pending request, pick it up with this channel and stop looking for requests
              break
            })
          }
        }
        is(ControlState.READ_WAITING) {
          // Wait for response from memory for pending read request
          when(mem_read_ready(i)) {
            mem_read_valid(i)                        := false.B
            consumer_read_ready(current_consumer(i)) := true.B
            consumer_read_data(current_consumer(i))  := mem_read_data(i)
            current_state(i)                         := ControlState.READ_RELAYING
          }
        }
        is(ControlState.READ_RELAYING) {
          // Wait for response from memory for pending write request
          when(mem_write_ready(i)) {
            mem_write_valid(i)                        := false.B
            consumer_write_ready(current_consumer(i)) := true.B
            current_state(i)                          := ControlState.WRITE_RELAYING
          }
        }
        is(ControlState.WRITE_WAITING) {
          when(!consumer_read_valid(i)) {
            channel_serving_consumer(current_consumer(i)) := false.B
            consumer_read_ready(i)                        := false.B
            current_state(i)                              := ControlState.IDLE
          }
        }
        is(ControlState.WRITE_RELAYING) {
          when(!consumer_write_valid(i)) {
            channel_serving_consumer(current_consumer(i)) := false.B
            consumer_write_ready(i)                       := false.B
            current_state(i)                              := ControlState.IDLE
          }
        }
      }
    }
  }

  io.consumer_read_ready  := consumer_read_ready
  io.consumer_read_data   := consumer_read_data
  io.consumer_write_ready := consumer_write_ready
  io.mem_read_valid       := mem_read_valid
  io.mem_read_address     := mem_read_address
  io.mem_write_valid      := mem_write_valid
  io.mem_write_address    := mem_write_address
  io.mem_write_data       := mem_write_data
}
