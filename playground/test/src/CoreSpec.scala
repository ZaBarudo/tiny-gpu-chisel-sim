package core

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import statecode.CoreState
import fetcher.FetcherState
import lsu.LSUState

class CoreModel(
  DataMemAddrBits:    Int = 8,
  DataMemDataBits:    Int = 8,
  ProgramMemAddrBits: Int = 8,
  ProgramMemDataBits: Int = 16,
  ThreadsPerBlock: Int = 4) {
  // State variables matching the Core module's submodules
  var fetcherModel   = new fetcher.FetcherModel()
  var decoderModel   = new decoder.DecoderModel()
  var schedulerModel = new scheduler.SchedulerModel(ThreadsPerBlock)

  // Arrays for per-thread components
  var aluModels     = Array.fill(ThreadsPerBlock)(new alu.AluModel())
  var lsuModels     = Array.fill(ThreadsPerBlock)(new lsu.LsuModel())
  var regfileModels = Array.fill(ThreadsPerBlock)(new registers.RegModel())
  var pcModels      = Array.fill(ThreadsPerBlock)(new pc.PCModel())

  class PrevCycleData {

    // Scheduler outputs
    var current_pc = 0
    var core_state = CoreState.IDLE
    var done       = false
    // Scheduler end

    // PC output
    var next_pc = Array.fill(ThreadsPerBlock)(0)
    // PC end

    // LSU outputs
    var lsu_read_valid    = Array.fill(ThreadsPerBlock)(false)
    var lsu_read_address  = Array.fill(ThreadsPerBlock)(0)
    var lsu_write_valid   = Array.fill(ThreadsPerBlock)(false)
    var lsu_write_address = Array.fill(ThreadsPerBlock)(0)
    var lsu_write_data    = Array.fill(ThreadsPerBlock)(0)
    var lsu_state         = Array.fill(ThreadsPerBlock)(LSUState.IDLE)
    var lsu_out           = Array.fill(ThreadsPerBlock)(0)
    // LSU end

    def save(module: scheduler.SchedulerModel) = {
      core_state = module.core_state
      current_pc = module.current_pc
      done = module.done
    }

    def save(module: pc.PCModel, idx: Int) = {
      next_pc(idx) = module.next_pc
    }

    def save(module: lsu.LsuModel, idx: Int) = {
      lsu_read_valid(idx) = module.read_valid
      lsu_read_address(idx) = module.read_address
      lsu_write_valid(idx) = module.write_valid
      lsu_write_address(idx) = module.write_address
      lsu_write_data(idx) = module.write_data
      lsu_state(idx) = module.lsu_state
      lsu_out(idx) = module.output_data
    }
  }

  var prevCycleData = new PrevCycleData

  def update(
    // Kernel execution inputs
    start:        Boolean,
    block_id:     Int,
    thread_count: Int,

    // Memory inputs
    program_mem_read_ready: Boolean,
    program_mem_read_data:  Int,

    // Data Memory signals
    data_mem_read_data:   Array[Int],
    data_mem_read_ready:  Array[Boolean],
    data_mem_write_ready: Array[Boolean]
  ): Unit = {
    // Update fetcher
    fetcherModel.update(
      core_state = prevCycleData.core_state,
      current_pc = prevCycleData.current_pc,
      mem_read_ready = program_mem_read_ready,
      mem_read_data = program_mem_read_data
    )

    // Update decoder
    decoderModel.update(
      core_state = prevCycleData.core_state,
      instruction = fetcherModel.instruction
    )

    // Update scheduler
    schedulerModel.update(
      start = start,
      mem_read_enable = decoderModel.decoded_mem_read_enable,
      mem_write_enable = decoderModel.decoded_mem_write_enable,
      decoded_ret = decoderModel.decoded_ret,
      fetcher_state = fetcherModel.fetcher_state,
      lsu_state = prevCycleData.lsu_state,
      next_pc = prevCycleData.next_pc
    )
    prevCycleData.save(schedulerModel)

    // Update per-thread components
    for (i <- 0 until ThreadsPerBlock) {
      val enable = i < thread_count

      val rs_in = decoderModel.decoded_rs_address
      val rt_in = decoderModel.decoded_rt_address

      // Update ALU
      aluModels(i).update(
        enable = enable,
        core_state = schedulerModel.core_state,
        rs = regfileModels(i).getReg(rs_in), // TODO: Need to use `prevCycleData`?
        rt = regfileModels(i).getReg(rt_in),
        arithmetic_mux = decoderModel.decoded_alu_arithmetic_mux,
        output_mux = decoderModel.decoded_alu_output_mux
      )

    //   // Update LSU
    //   lsuModels(i).update(
    //     enable = enable,
    //     core_state = schedulerModel.core_state,
    //     read_enable = decoderModel.decoded_mem_read_enable,
    //     write_enable = decoderModel.decoded_mem_write_enable,
    //     rs = data_mem_read_data(i),
    //     rt = data_mem_read_ready(i),
    //     mem_read_data = data_mem_read_data(i),
    //     mem_read_ready = data_mem_read_ready(i),
    //     mem_write_ready = data_mem_write_ready(i)
    //   )

      //   // Update Register Files
      //   regfileModels(i).update(
      //     enable = enable,
      //     block_id = block_id,
      //     core_state = schedulerModel.core_state,
      //     decoded_reg_address = decoderModel.decoded_reg_address,
      //     decoded_reg_write_enable = decoderModel.decoded_reg_write_enable,
      //     decoded_reg_input_mux = decoderModel.decoded_reg_input_mux,
      //     alu_out = aluModels(i).alu_out,
      //     lsu_out = lsuModels(i).lsu_out,
      //     decoded_immediate = decoderModel.decoded_immediate
      //   )

      // Update PC
      pcModels(i).update(
        enable = enable,
        core_state = schedulerModel.core_state,
        decoded_nzp = decoderModel.decoded_nzp,
        decoded_immediate = decoderModel.decoded_immediate,
        decoded_nzp_write_enable = decoderModel.decoded_nzp_write_enable,
        decoded_pc_mux = decoderModel.decoded_pc_mux,
        alu_out = aluModels(i).alu_out,
        current_pc = schedulerModel.current_pc
      )
    }
  }

  def reset(): Unit = {
    fetcherModel.reset()
    decoderModel.reset()
    schedulerModel.reset()
    aluModels.foreach(_.reset())
    lsuModels.foreach(_.reset())
    regfileModels.foreach(_.reset())
    pcModels.foreach(_.reset())
  }

}

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
