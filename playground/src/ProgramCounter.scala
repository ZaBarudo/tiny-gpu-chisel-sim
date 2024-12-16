package pc

import chisel3._
import chisel3.util._
import statecode.StateCode
import scala.reflect.internal.Reporter.INFO
// PROGRAM COUNTER
// > Calculates the next PC for each thread to update to (but currently we assume all threads
//   update to the same PC and don't support branch divergence)
// > Currently, each thread in each core has it's own calculation for next PC
// > The NZP register value is set by the CMP instruction (based on >/=/< comparison) to
//   initiate the BRnzp instruction for branching
class ProgramCounter(DataMemWidth: Int = 8, MemAddrWidth: Int = 8) extends Module {
  val io = IO(new Bundle {
    val enable = Input(Bool())

    val core_state = Input(StateCode())

    val decoded_nzp              = Input(UInt(2.W))
    val decoded_immediate        = Input(UInt(DataMemWidth.W))
    val decoded_nzp_write_enable = Input(Bool())
    val decoded_pc_mux           = Input(Bool())

    val alu_out = Input(UInt(DataMemWidth.W))

    val current_pc = Input(UInt(MemAddrWidth.W))
    val next_pc    = Output(UInt(MemAddrWidth.W))
  })

  val nzp     = RegInit(0.U(2.W))
  val next_pc = RegInit(0.U(MemAddrWidth.W))

  when(io.enable) {
    when(io.core_state === StateCode.EXECUTE) {
      when(io.decoded_pc_mux) {
        when((nzp & io.decoded_nzp) =/= 0.U) {
          // On BRnzp instruction, branch to immediate if NZP case matches previous CMP
          next_pc := io.decoded_immediate
        }.otherwise {
          // Otherwise, just update to PC + 1 (next line)
          next_pc := io.current_pc + 1.U
        }
      }.otherwise {
        // By default update to PC + 1 (next line)
        next_pc := io.current_pc + 1.U
      }
    }

    // Store NZP when core_state = UPDATE
    when(io.core_state === StateCode.UPDATE) {
      when(io.decoded_nzp_write_enable) {
        nzp := io.alu_out(2, 0)
      }
    }
  }

  when(reset.asBool) {
    io.next_pc := 0.U
  }.otherwise {
    io.next_pc := next_pc
  }
}
