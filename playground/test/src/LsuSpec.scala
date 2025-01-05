package lsu

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import statecode.CoreState

class LsuModel {
  var lsu_state     = LSUState.IDLE
  var output_data   = 0
  var read_valid    = false
  var read_address  = 0
  var write_valid   = false
  var write_address = 0
  var write_data    = 0

  def update(
    enable:          Boolean,
    core_state:      CoreState.Type,
    read_enable:     Boolean,
    write_enable:    Boolean,
    rs:              Int,
    rt:              Int,
    mem_read_data:   Int,
    mem_read_ready:  Boolean,
    mem_write_ready: Boolean
  ) = {
    if (enable) {
      if (read_enable) {
        lsu_state match {
          case LSUState.IDLE       =>
            if (core_state == CoreState.REQUEST) {
              lsu_state = LSUState.REQUESTING
            }
          case LSUState.REQUESTING =>
            read_valid = true
            read_address = rs
            lsu_state = LSUState.WAITING
          case LSUState.WAITING    =>
            if (mem_read_ready) {
              read_valid = false
              output_data = mem_read_data
              lsu_state = LSUState.DONE
            }
          case LSUState.DONE       =>
            if (core_state == CoreState.UPDATE) {
              lsu_state = LSUState.IDLE
            }
        }
      }

      if (write_enable) {
        lsu_state match {
          case LSUState.IDLE       =>
            if (core_state == CoreState.REQUEST) {
              lsu_state = LSUState.REQUESTING
            }
          case LSUState.REQUESTING =>
            write_valid = true
            write_address = rs
            write_data = rt
            lsu_state = LSUState.WAITING
          case LSUState.WAITING    =>
            if (mem_write_ready) {
              write_valid = false
              lsu_state = LSUState.DONE
            }
          case LSUState.DONE       =>
            if (core_state == CoreState.UPDATE) {
              lsu_state = LSUState.IDLE
            }
        }
      }
    }
  }
}

class LsuSpec extends AnyFreeSpec with Matchers {
  "Test Load/Store Unit" - {
    "should match model behavior with random inputs" in {
      simulate(new MemLoadStoreUnit) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step()
        dut.reset.poke(false.B)
        dut.clock.step()

        def randomCoreState(rng: scala.util.Random): CoreState.Type = {
          val v = rng.nextInt(2)
          v match {
            case 0 => CoreState.REQUEST
            case 1 => CoreState.UPDATE
          }
        }

        var cnt      = 0
        val rng      = new scala.util.Random(42) // 42 is the seed for reproducibility
        val lsuModel = new LsuModel()

        while (cnt < 10000) {
          val enable          = rng.nextBoolean()
          val core_state      = randomCoreState(rng)
          val read_enable     = rng.nextBoolean()
          val write_enable    = if (read_enable) false else rng.nextBoolean() // Ensure not both enabled
          val rs              = rng.nextInt(256)
          val rt              = rng.nextInt(256)
          val mem_read_data   = rng.nextInt(256)
          val mem_read_ready  = rng.nextBoolean()
          val mem_write_ready = rng.nextBoolean()

          dut.io.enable.poke(enable.B)
          dut.io.core_state.poke(core_state)
          dut.io.mem_rw_enable.read_enable.poke(read_enable.B)
          dut.io.mem_rw_enable.write_enable.poke(write_enable.B)
          dut.io.reg_in.rs.poke(rs.U)
          dut.io.reg_in.rt.poke(rt.U)
          dut.io.mem_read_data.poke(mem_read_data.U)
          dut.io.mem_read_address_sender.ready.poke(mem_read_ready.B)
          dut.io.mem_write_sender.ready.poke(mem_write_ready.B)

          dut.clock.step()

          lsuModel.update(
            enable = enable,
            core_state = core_state,
            read_enable = read_enable,
            write_enable = write_enable,
            rs = rs,
            rt = rt,
            mem_read_data = mem_read_data,
            mem_read_ready = mem_read_ready,
            mem_write_ready = mem_write_ready
          )

          // dut.io.lsu_state.expect(LSUState.IDLE)
          assert(dut.io.lsu_state.peekValue().asBigInt == lsuModel.lsu_state.litValue)
          dut.io.lsu_out.expect(lsuModel.output_data.U)
          dut.io.mem_read_address_sender.valid.expect(lsuModel.read_valid.B)
          if (lsuModel.read_valid) {
            dut.io.mem_read_address_sender.bits.expect(lsuModel.read_address.U)
          }
          dut.io.mem_write_sender.valid.expect(lsuModel.write_valid.B)
          if (lsuModel.write_valid) {
            dut.io.mem_write_sender.bits.address.expect(lsuModel.write_address.U)
            dut.io.mem_write_sender.bits.data.expect(lsuModel.write_data.U)
          }

          cnt += 1
        }
      }
    }
  }
}
