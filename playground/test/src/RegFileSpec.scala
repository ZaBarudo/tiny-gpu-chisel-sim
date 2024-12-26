package registers

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import statecode.CoreState

class RegFileSpec extends AnyFreeSpec with Matchers {
  "Test load/store for RegisterFiles" in {
    simulate(new RegisterFiles) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()

      dut.io.enable.poke(true.B)
      dut.io.core_state.poke(CoreState.IDLE)
      dut.io.decoded_reg_address.rs.poke(0.U)
      dut.io.decoded_reg_address.rt.poke(1.U)
      dut.io.decoded_reg_write_enable.poke(true.B)
      dut.io.decoded_reg_input_mux.poke(RegInputOp.ALU)
      dut.io.decoded_immediate.poke(10.U)
      dut.io.alu_out.poke(10.U)
      dut.io.lsu_out.poke(10.U)
      dut.clock.step()
    }
  }
}
