package lsu

import chisel3._
import chisel3.util._
import statecode.CoreState

object LSUState extends ChiselEnum {
  val IDLE, REQUESTING, WAITING, DONE = Value
}

class MemLoadStoreUnit extends Module {
  val io = IO(new Bundle {
    val enable = Input(Bool())

    val core_state = Input(CoreState())

    val decoded_mem_read_enable  = Input(Bool())
    val decoded_mem_write_enable = Input(Bool())

    // Registers
    val rs = Input(UInt(8.W))
    val rt = Input(UInt(8.W))

    val mem_read_valid   = Output(Bool())
    val mem_read_address = Output(UInt(8.W))
    val mem_read_ready   = Input(Bool())
    val mem_read_data    = Input(UInt(8.W))

    val mem_write_valid   = Output(Bool())
    val mem_write_address = Output(UInt(8.W))
    val mem_write_data    = Output(UInt(8.W))
    val mem_write_ready   = Input(Bool())

    // LSU Outputs
    val lsu_state = Output(LSUState())
    val lsu_out   = Output(UInt(8.W))
  })

  val lsu_state         = RegInit(LSUState.IDLE)
  val lsu_out           = RegInit(0.U(8.W))
  val mem_read_valid    = RegInit(false.B)
  val mem_read_address  = RegInit(0.U(8.W))
  val mem_write_valid   = RegInit(false.B)
  val mem_write_address = RegInit(0.U(8.W))
  val mem_write_data    = RegInit(0.U(8.W))

  when(io.enable) {
    // If memory read enable is triggered (LDR instruction)
    when(io.decoded_mem_read_enable) {
      switch(io.lsu_state) {
        is(LSUState.IDLE) {
          when(io.core_state === CoreState.REQUEST) {
            lsu_state := LSUState.REQUESTING
          }
        }
        is(LSUState.REQUESTING) {
          mem_read_valid   := true.B
          mem_read_address := io.rs
          lsu_state        := LSUState.WAITING
        }
        is(LSUState.WAITING) {
          when(io.mem_read_ready) {
            mem_read_valid := false.B
            lsu_out        := io.mem_read_data
            lsu_state      := LSUState.DONE
          }
        }
        is(LSUState.DONE) {
          when(io.core_state === CoreState.UPDATE) {
            lsu_state := LSUState.IDLE
          }
        }
      }
    }

    // If memory write enable is triggered (STR instruction)
    when(io.decoded_mem_write_enable) {
      switch(io.lsu_state) {
        is(LSUState.IDLE) {
          when(io.core_state === CoreState.REQUEST) {
            lsu_state := LSUState.REQUESTING
          }
        }
        is(LSUState.REQUESTING) {
          mem_write_valid   := true.B
          mem_write_address := io.rs
          mem_write_data    := io.rt
          lsu_state         := LSUState.WAITING
        }
        is(LSUState.WAITING) {
          when(io.mem_write_ready) {
            mem_write_valid := false.B
            lsu_state       := LSUState.DONE
          }
        }
        is(LSUState.DONE) {
          when(io.core_state === CoreState.UPDATE) {
            lsu_state := LSUState.IDLE
          }
        }
      }
    }
  }

  io.lsu_state         := lsu_state
  io.lsu_out           := lsu_out
  io.mem_read_valid    := mem_read_valid
  io.mem_read_address  := mem_read_address
  io.mem_write_valid   := mem_write_valid
  io.mem_write_address := mem_write_address
  io.mem_write_data    := mem_write_data
}
