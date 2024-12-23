package scheduler

import chisel3._
import chisel3.util._
import statecode.CoreState
import lsu.LSUState
import fetcher.FetcherState

// SCHEDULER
// > Manages the entire control flow of a single compute core processing 1 block
// 1. FETCH - Retrieve instruction at current program counter (PC) from program memory
// 2. DECODE - Decode the instruction into the relevant control signals
// 3. REQUEST - If we have an instruction that accesses memory, trigger the async memory requests from LSUs
// 4. WAIT - Wait for all async memory requests to resolve (if applicable)
// 5. EXECUTE - Execute computations on retrieved data from registers / memory
// 6. UPDATE - Update register values (including NZP register) and program counter
// > Each core has it's own scheduler where multiple threads can be processed with
//   the same control flow at once.
// > Technically, different instructions can branch to different PCs, requiring "branch divergence." In
//   this minimal implementation, we assume no branch divergence (naive approach for simplicity)
class Scheduler(ThreadsPerBlock: Int = 4) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())

    // Control signals
    val mem_rw_enable = new Bundle {
      val read_enable  = Input(Bool()) // Enable reading from memory
      val write_enable = Input(Bool()) // Enable writing to memory
    }
    val decoded_ret   = Input(Bool())

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
