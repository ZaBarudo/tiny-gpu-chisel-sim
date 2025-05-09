package alu

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import statecode.AluOpCode
import statecode.CoreState

class AluModel {
  var alu_out = 0

  def update(
    enable:         Boolean,
    core_state:     CoreState.Type,
    rs:             Int,
    rt:             Int,
    arithmetic_mux: AluOpCode.Type,
    output_mux:     Boolean
  ): Unit = {
    if (enable && core_state == CoreState.EXECUTE) {
      if (output_mux) {
        // Compare operation
        val gt = if (rs > rt) 1 else 0
        val eq = if (rs == rt) 1 else 0
        val lt = if (rs < rt) 1 else 0
        alu_out = (gt << 2) | (eq << 1) | lt
      } else {
        // Arithmetic operation
        alu_out = arithmetic_mux match {
          case AluOpCode.ADD => (rs + rt) & 0xff
          case AluOpCode.SUB => ((rs - rt) + 256) & 0xff
          case AluOpCode.MUL => (rs * rt) & 0xff
          case AluOpCode.DIV => if (rt == 0) 0 else rs / rt
        }
      }
    }
  }

  def reset(): Unit = {
    alu_out = 0
  }
}

class AluSpec extends AnyFreeSpec with Matchers {
  "Test ALU for all supported operations" in {
    simulate(new Alu) { dut =>
      def cmpResult(rs: Int, rt: Int): Int = {
        val gt = if (rs > rt) 1 else 0
        val eq = if (rs == rt) 1 else 0
        val lt = if (rs < rt) 1 else 0
        (gt << 2) | (eq << 1) | lt
      }

      // Calculate expected result based on operation
      def arithmeticResult(rs: Int, rt: Int, op: AluOpCode.Type): Int = {
        op match {
          case AluOpCode.ADD => (rs + rt) & 0xff
          case AluOpCode.SUB => ((rs - rt) + 256) & 0xff
          case AluOpCode.MUL => (rs * rt) & 0xff
          case AluOpCode.DIV => if (rt == 0) 0 else rs / rt
        }
      }

      val testValues = for {
        rs         <- 0 to 255
        rt         <- 0 to 255
        op         <- List(AluOpCode.ADD, AluOpCode.SUB, AluOpCode.MUL, AluOpCode.DIV)
        core_state <- List(CoreState.IDLE, CoreState.EXECUTE)
        is_cmp     <- List(true, false)
      } yield (rs, rt, op, core_state, is_cmp)

      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()

      var aluModel = new AluModel()
      // Test each combination
      for ((rs, rt, op, core_state, is_cmp) <- testValues) {
        // Set inputs
        dut.io.enable.poke(true.B)
        dut.io.core_state.poke(core_state)
        dut.io.decoded_alu_op.arithmetic_mux.poke(op)
        dut.io.decoded_alu_op.output_mux.poke(is_cmp)
        dut.io.reg_in.rs.poke(rs.U)
        dut.io.reg_in.rt.poke(rt.U)

        dut.clock.step()

        aluModel.update(
          enable = true,
          core_state = core_state,
          rs = rs,
          rt = rt,
          arithmetic_mux = op,
          output_mux = is_cmp
        )
        // Verify output
        dut.io.alu_out.expect(aluModel.alu_out.U)
      }
    }
  }
}
