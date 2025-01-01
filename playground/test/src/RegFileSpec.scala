package registers

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import statecode.RegInputOp
import statecode.CoreState

import scala.collection.mutable.ArrayBuffer

class RegModel {
  var registers = ArrayBuffer.fill(16)(0)
  var rs_in     = 0
  var rt_in     = 0

  def rs_out(): Int = {
    registers(rs_in)
  }

  def rt_out(): Int = {
    registers(rt_in)
  }

  def update(
    enable:                   Boolean,
    block_id:                 Int,
    core_state:               CoreState.Type,
    rd:                       Int,
    rs:                       Int,
    rt:                       Int,
    decoded_reg_write_enable: Boolean,
    decoded_reg_input_mux:    RegInputOp.Type,
    decoded_immediate:        Int,
    alu_out:                  Int,
    lsu_out:                  Int
  ): Unit = {
    if (enable) {
      registers(13) = block_id
      if (core_state == CoreState.REQUEST) {
        rs_in = rs
        rt_in = rt
      } else if (decoded_reg_write_enable && rd < 13) {
        val data = decoded_reg_input_mux match {
          case RegInputOp.ARITHMETIC => alu_out
          case RegInputOp.MEMORY     => lsu_out
          case RegInputOp.CONSTANT   => decoded_immediate
        }
        registers(rd) = data
      }
    }
  }
}

class RegFileSpec extends AnyFreeSpec with Matchers {
  "Test load/store for RegisterFiles" in {
    val threadsPerBlk = 4
    val threadId      = 0
    val dataBits      = 8
    simulate(new RegisterFiles(threadsPerBlk, threadId, dataBits)) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()

      dut.io.enable.poke(true.B)

      // test reg14 and reg15 defualt values
      dut.io.core_state.poke(CoreState.REQUEST)
      dut.io.decoded_reg_address.rs.poke(14.U)
      dut.io.decoded_reg_address.rt.poke(15.U)
      dut.clock.step()
      dut.io.reg_out.rs.expect(threadsPerBlk.U)
      dut.io.reg_out.rt.expect(threadId.U)

      // helper functions
      def randomRegOp(rng: scala.util.Random): RegInputOp.Type = {
        val v = rng.nextInt(3)
        v match {
          case 0 => RegInputOp.ARITHMETIC
          case 1 => RegInputOp.MEMORY
          case 2 => RegInputOp.CONSTANT
        }
      }

      def randomCoreState(rng: scala.util.Random): CoreState.Type = {
        val v = rng.nextInt(2)
        v match {
          case 0 => CoreState.REQUEST
          case 1 => CoreState.UPDATE
        }
      }

      // Initialize register model
      val regModel = new RegModel()

      val times = 100
      val rng   = new scala.util.Random(42) // 42 is the seed for reproducibility
      for (i <- 0 until times) {
        // Generate test sequence
        val block_id   = rng.nextInt(256)
        val rd_in      = rng.nextInt(12)
        val rs_in      = rng.nextInt(12)
        val rt_in      = rng.nextInt(12)
        val op         = randomRegOp(rng)
        val core_state = randomCoreState(rng)
        val alu_out    = rng.nextInt(256)
        val lsu_out    = rng.nextInt(256)
        val immediate  = rng.nextInt(256)

        dut.io.core_state.poke(core_state)
        val is_write = core_state == CoreState.UPDATE
        dut.io.decoded_reg_write_enable.poke(is_write.B)

        dut.io.decoded_reg_address.rd.poke(rd_in.U)
        dut.io.decoded_reg_address.rs.poke(rs_in.U)
        dut.io.decoded_reg_address.rt.poke(rt_in.U)

        dut.io.decoded_reg_input_mux.poke(op)
        dut.io.decoded_immediate.poke(immediate.U)
        dut.io.alu_out.poke(alu_out.U)
        dut.io.lsu_out.poke(lsu_out.U)

        // Update model
        regModel.update(
          enable = true,
          block_id = block_id,
          core_state = core_state,
          rd = rd_in,
          rs = rs_in,
          rt = rt_in,
          decoded_reg_write_enable = is_write,
          decoded_reg_input_mux = op,
          decoded_immediate = immediate,
          alu_out = alu_out,
          lsu_out = lsu_out
        )

        dut.clock.step()
        // println(
        //   s"rd_in: $rd_in, rs_in: $rs_in, rt_in: $rt_in, op: $op, core_state: $core_state, immediate: $immediate, alu_out: $alu_out, lsu_out: $lsu_out"
        // )
        // println(s"regModel after 1 cycle: ${regModel.registers}")

        if (core_state == CoreState.REQUEST) {
          dut.io.reg_out.rs.expect(regModel.rs_out().U)
          dut.io.reg_out.rt.expect(regModel.rt_out().U)
        }
      }
    }
  }
}
