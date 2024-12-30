package pc

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import statecode.CoreState

class PCModel(DataMemWidth: Int = 8, MemAddrWidth: Int = 8) {
  private var nzp     = 0
  private var next_pc = 0

  def update(
    enable:                   Boolean,
    core_state:               CoreState.Type,
    decoded_nzp:              Int,
    decoded_immediate:        Int,
    decoded_nzp_write_enable: Boolean,
    decoded_pc_mux:           Boolean,
    alu_out:                  Int,
    current_pc:               Int
  ): Int = {
    if (enable) {
      // Execute state logic
      if (core_state == CoreState.EXECUTE) {
        if (decoded_pc_mux) {
          if ((nzp & decoded_nzp) != 0) {
            // Branch taken
            next_pc = decoded_immediate
          } else {
            // Branch not taken
            next_pc = current_pc + 1
          }
        } else {
          // Normal PC increment
          next_pc = current_pc + 1
        }
      }

      // Update state logic
      if (core_state == CoreState.UPDATE) {
        if (decoded_nzp_write_enable) {
          // Update NZP register from ALU output
          nzp = alu_out & 0x7 // Take only lowest 3 bits
        }
      }
    }

    next_pc
  }

  def reset(): Unit = {
    nzp = 0
    next_pc = 0
  }

  // Getter for testing
  def getNzp: Int = nzp
}

class PCSpec extends AnyFreeSpec with Matchers {
  "Test Program Counter" in {
    val DataMemWidth: Int = 8
    val MemAddrWidth: Int = 8
    simulate(new ProgramCounter(DataMemWidth, MemAddrWidth)) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()

      def randomCoreState(rng: scala.util.Random): CoreState.Type = {
        val v = rng.nextInt(2)
        v match {
          case 0 => CoreState.EXECUTE
          case 1 => CoreState.UPDATE
        }
      }

      dut.io.enable.poke(true.B)

      var cnt     = 0
      val rng     = new scala.util.Random(42) // 42 is the seed for reproducibility
      val pcModel = new PCModel()
      while (cnt < 1000) {
        val core_state               = randomCoreState(rng)
        val decoded_nzp              = rng.nextInt(4)
        val decoded_immediate        = rng.nextInt(256)
        val decoded_nzp_write_enable = rng.nextInt(2) == 1
        val decoded_pc_mux           = rng.nextInt(2) == 1
        val alu_out                  = rng.nextInt(256)
        val current_pc               = rng.nextInt(256)

        dut.io.core_state.poke(core_state)
        dut.io.decoded_nzp.poke(decoded_nzp.U)
        dut.io.decoded_immediate.poke(decoded_immediate.U)
        dut.io.decoded_nzp_write_enable.poke(decoded_nzp_write_enable.B)
        dut.io.decoded_pc_mux.poke(decoded_pc_mux.B)
        dut.io.alu_out.poke(alu_out.U)
        dut.io.current_pc.poke(current_pc.U)

        dut.clock.step()

        dut.io.next_pc.expect(
          pcModel
            .update(
              enable = true,
              core_state = core_state,
              decoded_nzp = decoded_nzp,
              decoded_immediate = decoded_immediate,
              decoded_nzp_write_enable = decoded_nzp_write_enable,
              decoded_pc_mux = decoded_pc_mux,
              alu_out = alu_out,
              current_pc = current_pc
            )
            .U
        )

        cnt += 1
      }
    }
  }
}
