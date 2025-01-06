package dispatch

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import statecode.CoreState

class DispatchModel(NumCores: Int = 2, ThreadsPerCore: Int = 4) {
  val ThreadCountWidth = log2Ceil(ThreadsPerCore) + 1

  var core_start        = Array.fill(NumCores)(false)
  var core_reset        = Array.fill(NumCores)(false)
  var core_block_id     = Array.fill(NumCores)(0)
  var core_thread_count = Array.fill(NumCores)(0)
  var done              = false

  // interal states
  var start_execution   = false
  var blocks_done       = 0
  var blocks_dispatched = 0

  def update(
    start:        Boolean,
    thread_count: Int,
    core_done:    Array[Boolean]
  ): Unit = {
    if (!start) {
      return
    }

    if (!start_execution) {
      start_execution = true
      core_reset.map(_ => true)
    }

    val total_blocks = (thread_count + ThreadsPerCore - 1) / ThreadsPerCore
    if (blocks_done == total_blocks) {
      done = true
    }

    for (i <- 0 until NumCores) {
      if (core_reset(i)) {
        core_reset(i) = false

        if (blocks_dispatched < total_blocks) {
          core_start(i) = true
          core_block_id(i) = blocks_dispatched
          core_thread_count(i) = if (blocks_dispatched == total_blocks - 1) {
            thread_count - (blocks_dispatched * ThreadsPerCore)
          } else { ThreadsPerCore }
          blocks_dispatched += 1
        }
      }

      if (core_start(i) && core_done(i)) {
        core_reset(i) = true
        core_start(i) = false
        blocks_done += 1
      }
    }
  }
}

class DispatchSpec extends AnyFreeSpec with Matchers {
  "Test dispatch" - {
    "should match model behavior with random inputs" in {
      val NumCores:       Int = 2
      val ThreadsPerCore: Int = 4

      simulate(new Dispatch(NumCores, ThreadsPerCore)) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step()
        dut.reset.poke(false.B)
        dut.clock.step()

        var cnt           = 0
        val rng           = new scala.util.Random(42) // 42 is the seed for reproducibility
        val dispatchModel = new DispatchModel()

        while (cnt < 10000) {
          val core_done    = Array.fill(NumCores)(rng.nextBoolean())
          val thread_count = rng.nextInt(ThreadsPerCore)

          dut.io.start.poke(true.B)
          dut.io.thread_count.poke(thread_count.U)
          for (i <- 0 until NumCores) {
            dut.io.core_done(i).poke(core_done(i).B)
          }

          dut.clock.step()

          // Update model
          dispatchModel.update(
            start = true,
            thread_count = thread_count,
            core_done = core_done
          )

          // Compare model with DUT
          dut.io.done.expect(dispatchModel.done.B)
          for (i <- 0 until NumCores) {
            dut.io.core_start(i).expect(dispatchModel.core_start(i).B)
            dut.io.core_reset(i).expect(dispatchModel.core_reset(i).B)
            dut.io.core_block_id(i).expect(dispatchModel.core_block_id(i).U)
            dut.io.core_thread_count(i).expect(dispatchModel.core_thread_count(i).U)
          }

          cnt += 1
        }
      }
    }
  }
}
