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

// MEMORY CONTROLLER
// > Receives memory requests from all cores
// > Throttles requests based on limited external memory bandwidth
// > Waits for responses from external memory and distributes them back to cores
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
    val consumer_read_addr_receiver = Vec(NumConsumers, Flipped(new DecoupledIO(UInt(AddrBits.W))))
    val consumer_read_data          = Output(Vec(NumConsumers, UInt(DataBits.W)))

    val consumer_write_receiver = Vec(
      NumConsumers,
      Flipped(DecoupledIO(new Bundle {
        val address = UInt(AddrBits.W)
        val data    = UInt(DataBits.W)
      }))
    )

    // Memory Interface (Data / Program)
    val mem_read_sender = Vec(NumChannels, DecoupledIO(UInt(AddrBits.W)))
    val mem_read_data   = Input(Vec(NumChannels, UInt(DataBits.W)))

    val mem_write_sender = Vec(
      NumChannels,
      DecoupledIO(new Bundle {
        val address = UInt(AddrBits.W)
        val data    = UInt(DataBits.W)
      })
    )
  })

  val mem_read_valid   = RegInit(VecInit(Seq.fill(NumChannels)(false.B)))
  val mem_read_address = RegInit(VecInit(Seq.fill(NumChannels)(0.U(AddrBits.W))))

  val mem_write_valid   = RegInit(VecInit(Seq.fill(NumChannels)(false.B)))
  val mem_write_address = RegInit(VecInit(Seq.fill(NumChannels)(0.U(AddrBits.W))))
  val mem_write_data    = RegInit(VecInit(Seq.fill(NumChannels)(0.U(DataBits.W))))

  val consumer_read_ready  = RegInit(VecInit(Seq.fill(NumConsumers)(false.B)))
  val consumer_read_data   = RegInit(VecInit(Seq.fill(NumConsumers)(0.U(DataBits.W))))
  val consumer_write_ready = RegInit(VecInit(Seq.fill(NumConsumers)(false.B)))

  val current_consumer = RegInit(VecInit(Seq.fill(log2Ceil(NumConsumers))(0.U(NumChannels.W))))
  val controller_state = RegInit(VecInit(Seq.fill(3)(ControlState.IDLE)))

  val channel_serving_consumer = RegInit(VecInit(Seq.fill(NumConsumers)(false.B)))

  when(!reset.asBool) {
    // For each channel, we handle processing concurrently
    for (i <- 0 until NumChannels) {
      switch(controller_state(i)) {
        is(ControlState.IDLE) {
          // While this channel is idle, cycle through consumers looking for one with a pending request
          // Once we find a pending request, pick it up with this channel and stop looking for requests
          val read_signals  = Vec(NumConsumers, Bool())
          val write_signals = Vec(NumConsumers, Bool())
          for (j <- 0 until NumConsumers) {
            read_signals(j)  := io.consumer_read_addr_receiver(j).valid && !channel_serving_consumer(j)
            write_signals(j) := io.consumer_write_receiver(j).valid && !channel_serving_consumer(j)
          }

          when(read_signals.asUInt > 0.U) {
            val channel_index = PriorityEncoder(read_signals.reverse)
            val read_address  = io.consumer_read_addr_receiver(channel_index).bits

            channel_serving_consumer(channel_index) := true.B
            current_consumer(i)                     := channel_index

            mem_read_valid(i)   := true.B
            mem_read_address(i) := read_address

            controller_state(i) := ControlState.READ_WAITING
          }.elsewhen(write_signals.asUInt > 0.U) {
            val channel_index = PriorityEncoder(write_signals.reverse)
            val write_address = io.consumer_write_receiver(channel_index).bits.address
            val write_data    = io.consumer_write_receiver(channel_index).bits.data

            channel_serving_consumer(channel_index) := true.B
            current_consumer(i)                     := channel_index

            mem_write_valid(i)   := true.B
            mem_write_address(i) := write_address
            mem_write_data(i)    := write_data

            controller_state(i) := ControlState.WRITE_WAITING
          }
        }
        is(ControlState.READ_WAITING) {
          // Wait for response from memory for pending read request
          when(io.mem_read_sender(i).ready) {
            mem_read_valid                           := false.B
            consumer_read_ready(current_consumer(i)) := true.B
            consumer_read_data(current_consumer(i))  := io.mem_read_data(i)
            controller_state(i)                      := ControlState.READ_RELAYING
          }
        }
        is(ControlState.WRITE_WAITING) {
          // Wait for response from memory for pending write request
          when(io.mem_write_sender(i).ready) {
            mem_write_valid(i)                        := false.B
            consumer_write_ready(current_consumer(i)) := true.B
            controller_state(i)                       := ControlState.WRITE_RELAYING
          }
        }
        is(ControlState.READ_RELAYING) {
          when(!io.consumer_read_addr_receiver(i).valid) {
            channel_serving_consumer(current_consumer(i)) := false.B
            consumer_read_ready(i)                        := false.B
            controller_state(i)                           := ControlState.IDLE
          }
        }
        is(ControlState.WRITE_RELAYING) {
          when(!io.consumer_write_receiver(i).valid) {
            channel_serving_consumer(current_consumer(i)) := false.B
            consumer_write_ready(i)                       := false.B
            controller_state(i)                           := ControlState.IDLE
          }
        }
      }
    }
  }

  for (i <- 0 until NumConsumers) {
    io.mem_read_sender(i).valid := mem_read_valid(i)
    io.mem_read_sender(i).bits  := mem_read_address(i)

    io.consumer_read_addr_receiver(i).ready := consumer_read_ready(i)
    io.consumer_read_data(i)                := consumer_read_data(i)
    io.consumer_write_receiver(i).ready     := consumer_write_ready(i)
  }

  for (i <- 0 until NumChannels) {
    io.mem_write_sender(i).valid        := mem_write_valid(i)
    io.mem_write_sender(i).bits.address := mem_write_address(i)
    io.mem_write_sender(i).bits.data    := mem_write_data(i)
  }
}
