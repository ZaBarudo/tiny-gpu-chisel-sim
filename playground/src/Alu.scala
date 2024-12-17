package alu

import chisel3._
import chisel3.util._
import statecode.CoreState

object AluOpCode extends ChiselEnum {
  val ADD, SUB, MUL, DIV = Value
}

class Alu extends Module {
  val io = IO(new Bundle {
    val enable = Input(Bool())

    val core_state = Input(CoreState())

    val decoded_alu_arithmetic_mux = Input(AluOpCode())
    val decoded_alu_output_mux     = Input(Bool())

    val rs      = Input(UInt(8.W))
    val rt      = Input(UInt(8.W))
    val alu_out = Output(UInt(8.W))
  })

  val alu_out_reg = RegInit(0.U(8.W))

  when(io.enable) {
    when(io.core_state === CoreState.EXECUTE) {
      when(io.decoded_alu_output_mux) {
        // Set values to compare with NZP register in alu_out[2:0]
        val gt = io.rs > io.rt
        val eq = io.rs === io.rt
        val lt = io.rs < io.rt
        alu_out_reg := Cat(0.U(5.W), gt, eq, lt)
      }.otherwise {
        switch(io.decoded_alu_arithmetic_mux) {
          is(AluOpCode.ADD) {
            alu_out_reg := io.rs + io.rt
          }
          is(AluOpCode.SUB) {
            alu_out_reg := io.rs - io.rt
          }
          is(AluOpCode.MUL) {
            alu_out_reg := io.rs * io.rt
          }
          is(AluOpCode.DIV) {
            alu_out_reg := io.rs / io.rt
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
