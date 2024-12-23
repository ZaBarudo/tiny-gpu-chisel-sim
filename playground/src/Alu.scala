package alu

import chisel3._
import chisel3.util._
import statecode.CoreState
import statecode.AluOpCode

class Alu extends Module {
  val io = IO(new Bundle {
    val enable = Input(Bool())

    val core_state = Input(CoreState())

    val decoded_alu_op = new Bundle {
      val arithmetic_mux = Input(AluOpCode()) // Select arithmetic operation
      val output_mux     = Input(Bool())      // Select operation in ALU
    }

    val reg_in = new Bundle {
      val rs = Input(UInt(8.W))
      val rt = Input(UInt(8.W))
    }

    val alu_out = Output(UInt(8.W))
  })

  val alu_out_reg = RegInit(0.U(8.W))

  when(io.enable) {
    when(io.core_state === CoreState.EXECUTE) {
      when(io.decoded_alu_op.output_mux) {
        // Set values to compare with NZP register in alu_out[2:0]
        val gt = io.reg_in.rs > io.reg_in.rt
        val eq = io.reg_in.rs === io.reg_in.rt
        val lt = io.reg_in.rs < io.reg_in.rt
        alu_out_reg := Cat(0.U(5.W), gt, eq, lt)
      }.otherwise {
        switch(io.decoded_alu_op.arithmetic_mux) {
          is(AluOpCode.ADD) {
            alu_out_reg := io.reg_in.rs + io.reg_in.rt
          }
          is(AluOpCode.SUB) {
            alu_out_reg := io.reg_in.rs - io.reg_in.rt
          }
          is(AluOpCode.MUL) {
            alu_out_reg := io.reg_in.rs * io.reg_in.rt
          }
          is(AluOpCode.DIV) {
            alu_out_reg := io.reg_in.rs / io.reg_in.rt
          }
        }
      }
    }
  }

  when(reset.asBool) {
    io.alu_out := 0.U
  }.otherwise {
    io.alu_out := alu_out_reg
  }
}
