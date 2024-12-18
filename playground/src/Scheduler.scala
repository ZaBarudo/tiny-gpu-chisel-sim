package scheduler

import chisel3._
import chisel3.util._
import statecode.CoreState
import lsu.LSUState
import fetcher.FetcherState

class Scheduler(ThreadsPerBlock: Int = 4) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())

    // Control signals
    val decoded_mem_read_enable  = Input(Bool())
    val decoded_mem_write_enable = Input(Bool())
    val decoded_ret              = Input(Bool())

    // Memory access state
    val fetcher_state = Input(FetcherState())
    val lsu_state     = Input(Vec(ThreadsPerBlock, LSUState()))

    // Current & Next PC
    val current_pc = Output(UInt(8.W))
    val next_pc    = Input(Vec(ThreadsPerBlock, UInt(8.W)))

    // Execution state
    val core_state = Output(CoreState())
    val done       = Output(Bool())
  })

  val current_pc = RegInit(0.U(8.W))
  val core_state = RegInit(CoreState.IDLE)
  val done       = RegInit(false.B)

  when(!reset.asBool) {
    switch(core_state) {
      is(CoreState.IDLE) {
        // Here after reset (before kernel is launched, or after previous block has been processed)
        when(io.start) {
          core_state := CoreState.FETCH
        }
      }

      is(CoreState.FETCH) {
        // Move on once fetcher_state = FETCHED
        when(io.fetcher_state === FetcherState.FETCHED) {
          core_state := CoreState.DECODE
        }
      }

      is(CoreState.DECODE) {
        // Decode is synchronous so we move on after one cycle
        core_state := CoreState.REQUEST
      }

      is(CoreState.REQUEST) {
        // Request is synchronous so we move on after one cycle
        core_state := CoreState.WAIT
      }

      is(CoreState.WAIT) {
        // Wait for all LSUs to finish their request before continuing
        // Make sure no lsu_state = REQUESTING or WAITING
        val any_lsu_waiting =
          io.lsu_state.exists(lsu_state => lsu_state === LSUState.WAITING || lsu_state === LSUState.REQUESTING)

        // If no LSU is waiting for a response, move onto the next stage
        when(!any_lsu_waiting) {
          core_state := CoreState.EXECUTE
        }
      }

      is(CoreState.EXECUTE) {
        // Execute is synchronous so we move on after one cycle
        core_state := CoreState.UPDATE
      }

      is(CoreState.UPDATE) {
        when(io.decoded_ret) {
          // If we reach a RET instruction, this block is done executing
          done       := true.B
          core_state := CoreState.DONE
        }.otherwise {
          // TODO: Branch divergence. For now assume all next_pc converge
          current_pc := io.next_pc((ThreadsPerBlock - 1).U)
          core_state := CoreState.FETCH
        }
      }

      is(CoreState.DONE) {
        // no-op
      }
    }
  }

  io.current_pc := current_pc
  io.core_state := core_state
  io.done       := done
}
