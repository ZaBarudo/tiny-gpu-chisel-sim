package decoder

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import statecode.CoreState
import statecode.AluOpCode
import statecode.RegInputOp

class DecoderModel {
  var decoded_rd_address         = 0
  var decoded_rs_address         = 0
  var decoded_rt_address         = 0
  var decoded_nzp                = 0
  var decoded_immediate          = 0
  var decoded_reg_write_enable   = false
  var decoded_mem_read_enable    = false
  var decoded_mem_write_enable   = false
  var decoded_nzp_write_enable   = false
  var decoded_reg_input_mux      = RegInputOp.ARITHMETIC
  var decoded_alu_arithmetic_mux = AluOpCode.ADD
  var decoded_alu_output_mux     = false
  var decoded_pc_mux             = false
  var decoded_ret                = false

  def update(
    core_state:  CoreState.Type,
    instruction: Int
  ): Unit = {
    if (core_state == CoreState.DECODE) {
      decoded_rd_address = (instruction & 0xf00) >> 8
      decoded_rs_address = (instruction & 0xf0) >> 4
      decoded_rt_address = instruction & 0x000f
      decoded_immediate = instruction & 0x00ff
      decoded_nzp = (instruction & 0xe00) >> 9
    }
  }
}

class DecoderSpec extends AnyFreeSpec with Matchers {
  "Test decoder" - {
    "should match model behavior with random inputs" in {
      val ProgramMemAddrBits: Int = 8
      val ProgramMemDataBits: Int = 16
      simulate(new Decoder) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step()
        dut.reset.poke(false.B)
        dut.clock.step()

        def randomCoreState(rng: scala.util.Random): CoreState.Type = {
          val v = rng.nextInt(3)
          v match {
            case 0 => CoreState.FETCH
            case 1 => CoreState.DECODE
            case 2 => CoreState.EXECUTE
          }
        }

        var cnt = 0
        val rng = new scala.util.Random(42) // 42 is the seed for reproducibility
      }
    }
  }
}
