package alu

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import statecode.AluOpCode
import statecode.CoreState

class AluSpec extends AnyFreeSpec with Matchers {
  "Test ALU for all supported operations" in {
    simulate(new Alu) { dut =>
      val testValues = for {
        rs         <- 0 to 10
        rt         <- 0 to 10
        op         <- List(AluOpCode.ADD, AluOpCode.SUB, AluOpCode.MUL, AluOpCode.DIV)
        core_state <- List(CoreState.IDLE, CoreState.EXECUTE, CoreState.DONE)
        is_cmp     <- List(true, false)
      } yield (rs, rt, op, core_state, is_cmp)

      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()

      var last_reg_val = 0
      // Test each combination
      for ((rs, rt, op, core_state, is_cmp) <- testValues) {
        // Set inputs
        dut.io.enable.poke(true.B)
        dut.io.core_state.poke(core_state)
        dut.io.decoded_alu_op.arithmetic_mux.poke(op)
        dut.io.decoded_alu_op.output_mux.poke(is_cmp)
        dut.io.reg_in.rs.poke(rs.U)
        dut.io.reg_in.rt.poke(rt.U)

        // Verify output
        dut.clock.step()

        // Calculate expected result based on operation
        if (is_cmp) {
          val gt = rs > rt
          val eq = rs == rt
          val lt = rs < rt
          dut.io.alu_out.expect(Cat(0.U(5.W), gt.B, eq.B, lt.B))
        } else if (core_state != CoreState.EXECUTE) {
          dut.io.alu_out.expect(last_reg_val)
        } else {
          val result = op match {
            case AluOpCode.ADD => rs + rt
            case AluOpCode.SUB => rs - rt
            case AluOpCode.MUL => rs * rt
            case AluOpCode.DIV => rs / rt
          }
          dut.io.alu_out.expect(result.U)
        }

        last_reg_val = dut.io.alu_out.peek().litValue.toInt
      }
    }
  }
}
