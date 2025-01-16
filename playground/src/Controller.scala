package controller

import chisel3._
import chisel3.util._

import statecode.ControlState

// MEMORY CONTROLLER
// > Receives memory requests from all cores
// > Throttles requests based on limited external memory bandwidth
// > Waits for responses from external memory and distributes them back to cores
class Controller(
  AddrBits:     Int = 8,
  DataBits:     Int = 16,
  NumConsumers: Int = 4,
  NumChannels:  Int = 1,
  WriteEnable:  Boolean = true)
    extends Module {
  // TODO: Use decoupledIO interface for consumer and memory
  val io = IO(new Bundle {
    // Consumer Interface (Fetchers / LSUs)
    val consumer_read_addr_receiver = Vec(NumConsumers, Flipped(new DecoupledIO(UInt(AddrBits.W))))
    val consumer_read_data          = Output(Vec(NumConsumers, UInt(DataBits.W)))

    val consumer_write_receiver =
      if (WriteEnable)
        Some(
          Vec(
            NumConsumers,
            Flipped(DecoupledIO(new Bundle {
              val address = UInt(AddrBits.W)
              val data    = UInt(DataBits.W)
            }))
          )
        )
      else None

    // Memory Interface (Data / Program)
    val mem_read_sender = Vec(NumChannels, new DecoupledIO(UInt(AddrBits.W)))
    val mem_read_data   = Input(Vec(NumChannels, UInt(DataBits.W)))

    val mem_write_sender =
      if (WriteEnable)
        Some(
          Vec(
            NumChannels,
            DecoupledIO(new Bundle {
              val address = UInt(AddrBits.W)
              val data    = UInt(DataBits.W)
            })
          )
        )
      else None
  })

  val mem_read_valid   = RegInit(VecInit(Seq.fill(NumChannels)(false.B)))
  val mem_read_address = RegInit(VecInit(Seq.fill(NumChannels)(0.U(AddrBits.W))))

  val mem_write_valid   = RegInit(VecInit(Seq.fill(NumChannels)(false.B)))
  val mem_write_address = RegInit(VecInit(Seq.fill(NumChannels)(0.U(AddrBits.W))))
  val mem_write_data    = RegInit(VecInit(Seq.fill(NumChannels)(0.U(DataBits.W))))

  val consumer_read_ready  = RegInit(VecInit(Seq.fill(NumConsumers)(false.B)))
  val consumer_read_data   = RegInit(VecInit(Seq.fill(NumConsumers)(0.U(DataBits.W))))
  val consumer_write_ready = RegInit(VecInit(Seq.fill(NumConsumers)(false.B)))

  val current_consumer = RegInit(VecInit(Seq.fill(NumChannels)(0.U(log2Ceil(NumConsumers).W))))
  val controller_state = RegInit(VecInit(Seq.fill(NumChannels)(ControlState.IDLE)))

  val channel_serving_consumer = RegInit(VecInit(Seq.fill(NumConsumers)(false.B)))

  // // Add debug printing
  // printf("#### Module Internal State ####\n")
  // printf(cf"#controller_state: ${controller_state}\n")
  // printf(cf"#mem_read_valid: ${mem_read_valid}\n")
  // printf(cf"#mem_read_address: ${mem_read_address}\n")
  // printf(cf"#mem_write_valid: ${mem_write_valid}\n")
  // printf(cf"#mem_write_address: ${mem_write_address}\n")
  // printf(cf"#mem_write_data: ${mem_write_data}\n")
  // printf(cf"#consumer_read_ready: ${consumer_read_ready}\n")
  // printf(cf"#consumer_read_data: ${consumer_read_data}\n")
  // printf(cf"#consumer_write_ready: ${consumer_write_ready}\n")
  // printf(cf"#channel_serving_consumer: ${channel_serving_consumer}\n")
  // printf(cf"#current_consumer: ${current_consumer}\n")

  when(!reset.asBool) {
    // For each channel, we handle processing concurrently
    for (i <- 0 until NumChannels) {
      switch(controller_state(i)) {
        is(ControlState.IDLE) {
          // While this channel is idle, cycle through consumers looking for one with a pending request
          // Once we find a pending request, pick it up with this channel and stop looking for requests
          val read_signals  = Wire(Vec(NumConsumers, Bool()))
          // val write_signals = Wire(Vec(NumConsumers, Bool()))
          val write_signals = VecInit(Seq.fill(NumConsumers)(false.B))
          for (j <- 0 until NumConsumers) {
            read_signals(j) := io.consumer_read_addr_receiver(j).valid && !channel_serving_consumer(j)
            // write_signals(j) := io.consumer_write_receiver(j).valid && !channel_serving_consumer(j)
            io.consumer_write_receiver.map(write_receiver =>
              write_signals(j) := write_receiver(j).valid && !channel_serving_consumer(j)
            )
          }

          val first_read_idx  = PriorityEncoder(read_signals)
          val first_write_idx = PriorityEncoder(write_signals)
          // printf("#first_read_idx: %b, first_write_idx: %b\n", first_read_idx, first_write_idx)

          when(read_signals.asUInt > 0.U && first_read_idx <= first_write_idx) {
            val read_address = io.consumer_read_addr_receiver(first_read_idx).bits

            channel_serving_consumer(first_read_idx) := true.B
            current_consumer(i)                      := first_read_idx

            mem_read_valid(i)   := true.B
            mem_read_address(i) := read_address

            controller_state(i) := ControlState.READ_WAITING
          }.elsewhen(write_signals.asUInt > 0.U) {
            // val write_address = io.consumer_write_receiver(first_write_idx).bits.address
            // val write_data    = io.consumer_write_receiver(first_write_idx).bits.data
            val write_address = io.consumer_write_receiver.map(_.apply(first_write_idx).bits.address).getOrElse(0.U)
            val write_data    = io.consumer_write_receiver.map(_.apply(first_write_idx).bits.data).getOrElse(0.U)

            channel_serving_consumer(first_write_idx) := true.B
            current_consumer(i)                       := first_write_idx

            mem_write_valid(i)   := true.B
            mem_write_address(i) := write_address
            mem_write_data(i)    := write_data

            controller_state(i) := ControlState.WRITE_WAITING
          }
        }
        is(ControlState.READ_WAITING) {
          // Wait for response from memory for pending read request
          when(io.mem_read_sender(i).ready) {
            mem_read_valid(i)                        := false.B
            consumer_read_ready(current_consumer(i)) := true.B
            consumer_read_data(current_consumer(i))  := io.mem_read_data(i)
            controller_state(i)                      := ControlState.READ_RELAYING
          }
        }
        is(ControlState.WRITE_WAITING) {
          // Wait for response from memory for pending write request
          val write_ready = io.mem_write_sender.map(_.apply(i).ready).getOrElse(false.B)
          // when(io.mem_write_sender(i).ready) {
          when(write_ready) {
            mem_write_valid(i)                        := false.B
            consumer_write_ready(current_consumer(i)) := true.B
            controller_state(i)                       := ControlState.WRITE_RELAYING
          }
        }
        is(ControlState.READ_RELAYING) {
          when(!io.consumer_read_addr_receiver(current_consumer(i)).valid) {
            channel_serving_consumer(current_consumer(i)) := false.B
            consumer_read_ready(current_consumer(i))      := false.B
            controller_state(i)                           := ControlState.IDLE
          }
        }
        is(ControlState.WRITE_RELAYING) {
          val write_valid = io.consumer_write_receiver.map(!_.apply(current_consumer(i)).valid).getOrElse(false.B)
          // when(!io.consumer_write_receiver(current_consumer(i)).valid) {
          when(write_valid) {
            channel_serving_consumer(current_consumer(i)) := false.B
            consumer_write_ready(current_consumer(i))     := false.B
            controller_state(i)                           := ControlState.IDLE
          }
        }
      }
    }
  }

  for (i <- 0 until NumConsumers) {
    io.consumer_read_addr_receiver(i).ready := consumer_read_ready(i)
    io.consumer_read_data(i)                := consumer_read_data(i)
    // io.consumer_write_receiver(i).ready     := consumer_write_ready(i)
    io.consumer_write_receiver.map(_.apply(i).ready := consumer_write_ready(i))
  }

  for (i <- 0 until NumChannels) {
    io.mem_read_sender(i).valid := mem_read_valid(i)
    io.mem_read_sender(i).bits  := mem_read_address(i)
    // io.mem_write_sender(i).valid        := mem_write_valid(i)
    // io.mem_write_sender(i).bits.address := mem_write_address(i)
    // io.mem_write_sender(i).bits.data    := mem_write_data(i)
    io.mem_write_sender.map(sender => {
      sender(i).valid        := mem_write_valid(i)
      sender(i).bits.address := mem_write_address(i)
      sender(i).bits.data    := mem_write_data(i)
    })
  }
}
