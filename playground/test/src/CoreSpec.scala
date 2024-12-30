package core

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import statecode.CoreState

class CoreSpec extends AnyFreeSpec with Matchers {
  "Test Core" - {
    "should match model behavior with random inputs" in {
      val DataMemAddrBits:    Int = 8
      val DataMemDataBits:    Int = 8
      val ProgramMemAddrBits: Int = 8
      val ProgramMemDataBits: Int = 16
      val ThreadsPerBlock:    Int = 4
      simulate(new Core(DataMemAddrBits, DataMemDataBits, ProgramMemAddrBits, ProgramMemDataBits, ThreadsPerBlock)) {
        dut => 0
      }
    }
  }
}
