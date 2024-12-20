package registers

import chisel3._
import chisel3.util._
import statecode.CoreState
import statecode.RegInputOp

class RegisterFiles(ThreadsPerBlk: Int = 4, ThreadId: Int = 0, DataBits: Int = 8) extends Module {
  val io = IO(new Bundle {
    val enable = Input(Bool())

    // Kernel Execution
    val block_id = Input(UInt(8.W))

    // State
    val core_state = Input(CoreState())

    // Instruction Signals
    val decoded_rd_address = Input(UInt(4.W))
    val decoded_rs_address = Input(UInt(4.W))
    val decoded_rt_address = Input(UInt(4.W))

    // Control Signals
    val decoded_reg_write_enable = Input(Bool())
    val decoded_reg_input_mux    = Input(RegInputOp())
    val decoded_immediate        = Input(UInt(DataBits.W))

    // Thread Unit Outputs
    val alu_out = Input(UInt(DataBits.W))
    val lsu_out = Input(UInt(DataBits.W))

    // Register File Outputs
    val reg_out = new Bundle {
      val rs = Output(UInt(DataBits.W))
      val rt = Output(UInt(DataBits.W))
    }
  })

  def InitValByIndex(index: Int): UInt = {
    index match {
      case 14 => ThreadsPerBlk.U
      case 15 => ThreadId.U
      case _  => 0.U(8.W)
    }
  }

  val registers = RegInit(VecInit(Seq.tabulate(16)(i => InitValByIndex(i))))

  val rs = RegInit(0.U(DataBits.W))
  val rt = RegInit(0.U(DataBits.W))

  when(!reset.asBool && io.enable) {
    // [Bad Solution] Shouldn't need to set this every cycle
    // Update the block_id when a new block is issued from dispatcher
    registers(13) := io.block_id

    // Fill rs/rt when core_state = REQUEST
    when(io.core_state === CoreState.REQUEST) {
      rs := registers(io.decoded_rs_address)
      rt := registers(io.decoded_rt_address)
    }.elsewhen(io.core_state === CoreState.UPDATE) { // Store rd when core_state = UPDATE
      // Only allow writing to R0 - R12
      when(io.decoded_reg_write_enable && io.decoded_rd_address < 13.U) {
        switch(io.decoded_reg_input_mux) {
          is(RegInputOp.ARITHMETIC) {
            registers(io.decoded_rd_address) := io.alu_out
          }
          is(RegInputOp.MEMORY) {
            registers(io.decoded_rd_address) := io.lsu_out
          }
          is(RegInputOp.CONSTANT) {
            registers(io.decoded_rd_address) := io.decoded_immediate
          }
        }
      }
    }
  }

  io.reg_out.rs := rs
  io.reg_out.rt := rt
}
