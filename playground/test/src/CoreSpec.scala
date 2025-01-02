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

    // Register Files outputs
    var regout_rs = Array.fill(ThreadsPerBlock)(0)
    var regout_rt = Array.fill(ThreadsPerBlock)(0)
    // Register end

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

    def save(module: registers.RegModel, idx: Int) = {
      regout_rs(idx) = module.rs_out()
      regout_rt(idx) = module.rt_out()
    }
  }

  var prevCycleData = new PrevCycleData

  // core's 8 outputs
  var done = false

  var program_mem_read_address_valid = false
  var program_mem_read_address_bits  = 0

  var data_mem_read_address_valid = Array.fill(ThreadsPerBlock)(false)
  var data_mem_read_address_bits  = Array.fill(ThreadsPerBlock)(0)

  var data_mem_write_address_valid = Array.fill(ThreadsPerBlock)(false)
  var data_mem_write_address_bits  = Array.fill(ThreadsPerBlock)(0)
  var data_mem_write_data_bits     = Array.fill(ThreadsPerBlock)(0)

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

      val rd_in = decoderModel.decoded_rd_address
      val rs_in = decoderModel.decoded_rs_address
      val rt_in = decoderModel.decoded_rt_address

      // Update ALU
      aluModels(i).update(
        enable = enable,
        core_state = schedulerModel.core_state,
        rs = prevCycleData.regout_rs(i),
        rt = prevCycleData.regout_rt(i),
        arithmetic_mux = decoderModel.decoded_alu_arithmetic_mux,
        output_mux = decoderModel.decoded_alu_output_mux
      )

      // Update LSU
      lsuModels(i).update(
        enable = enable,
        core_state = schedulerModel.core_state,
        read_enable = decoderModel.decoded_mem_read_enable,
        write_enable = decoderModel.decoded_mem_write_enable,
        rs = prevCycleData.regout_rs(i),
        rt = prevCycleData.regout_rt(i),
        mem_read_data = data_mem_read_data(i),
        mem_read_ready = data_mem_read_ready(i),
        mem_write_ready = data_mem_write_ready(i)
      )

      // Update Register Files
      regfileModels(i).update(
        enable = enable,
        block_id = block_id,
        core_state = schedulerModel.core_state,
        rs = rs_in,
        rd = rd_in,
        rt = rt_in,
        decoded_reg_write_enable = decoderModel.decoded_reg_write_enable,
        decoded_reg_input_mux = decoderModel.decoded_reg_input_mux,
        decoded_immediate = decoderModel.decoded_immediate,
        alu_out = aluModels(i).alu_out,
        lsu_out = lsuModels(i).output_data
      )
      prevCycleData.save(regfileModels(i), i)

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
      prevCycleData.save(pcModels(i), i)
    }

    // update outputs
    done = schedulerModel.done
    program_mem_read_address_valid = fetcherModel.read_valid
    program_mem_read_address_bits = fetcherModel.read_address

    for (i <- 0 until ThreadsPerBlock) {
      data_mem_read_address_valid(i) = lsuModels(i).read_valid
      data_mem_read_address_bits(i) = lsuModels(i).read_address
      data_mem_write_address_valid(i) = lsuModels(i).write_valid
      data_mem_write_address_bits(i) = lsuModels(i).write_address
      data_mem_write_data_bits(i) = lsuModels(i).write_data
    }

  }

  // def reset(): Unit = {
  //   fetcherModel.reset()
  //   decoderModel.reset()
  //   schedulerModel.reset()
  //   aluModels.foreach(_.reset())
  //   lsuModels.foreach(_.reset())
  //   regfileModels.foreach(_.reset())
  //   pcModels.foreach(_.reset())
  // }

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
        dut =>
          dut.reset.poke(true.B)
          dut.clock.step()
          dut.reset.poke(false.B)
          dut.clock.step()

          var cnt       = 0
          val rng       = new scala.util.Random(42) // 42 is the seed for reproducibility
          val coreModel = new CoreModel()
          while (cnt < 1000) {
            val block_id             = rng.nextInt(13)
            val thread_count         = rng.nextInt(ThreadsPerBlock)
            val mem_read_ready       = rng.nextBoolean()
            val mem_read_data        = rng.nextInt(256)
            val data_mem_read_data   = Array.fill(ThreadsPerBlock)(rng.nextInt(256))
            val data_mem_read_ready  = Array.fill(ThreadsPerBlock)(rng.nextBoolean())
            val data_mem_write_ready = Array.fill(ThreadsPerBlock)(rng.nextBoolean())

            dut.io.start.poke(true.B)
            dut.io.block_id.poke(block_id.U)
            dut.io.thread_count.poke(thread_count.U)

            dut.io.program_mem_read_address_sender.ready.poke(mem_read_ready.B)
            dut.io.program_mem_read_data.poke(mem_read_data.U)

            for (i <- 0 until ThreadsPerBlock) {
              dut.io.data_mem_read_data(i).poke(data_mem_read_data(i).U)
              dut.io.data_mem_read_address_sender(i).ready.poke(data_mem_read_ready(i).B)
              dut.io.data_mem_write_sender(i).ready.poke(data_mem_write_ready(i).B)
            }

            println(s"\n=== Random Values for Cycle $cnt ===")
            println(s"Block ID: $block_id")
            println(s"Thread Count: $thread_count")
            println(s"Program Memory Read: [ready=$mem_read_ready, data=$mem_read_data]")
            println(s"Read Data:    [${data_mem_read_data.mkString(", ")}]")
            println(s"Read Ready:   [${data_mem_read_ready.mkString(", ")}]")
            println(s"Write Ready:  [${data_mem_write_ready.mkString(", ")}]")
            println()

            dut.clock.step()

            coreModel.update(
              start = true,
              block_id = block_id,
              thread_count = thread_count,
              // Memory inputs
              program_mem_read_ready = mem_read_ready,
              program_mem_read_data = mem_read_data,
              // Data Memory signals
              data_mem_read_data = data_mem_read_data,
              data_mem_read_ready = data_mem_read_ready,
              data_mem_write_ready = data_mem_write_ready
            )

            // verify all 8 outputs
            dut.io.done.expect(coreModel.done.B)
            dut.io.program_mem_read_address_sender.valid.expect(coreModel.program_mem_read_address_valid.B)
            dut.io.program_mem_read_address_sender.bits.expect(coreModel.program_mem_read_address_bits.U)

            for (i <- 0 until ThreadsPerBlock) {
              dut.io.data_mem_read_address_sender(i).valid.expect(coreModel.data_mem_read_address_valid(i).B)
              dut.io.data_mem_read_address_sender(i).bits.expect(coreModel.data_mem_read_address_bits(i).U)
              dut.io.data_mem_write_sender(i).valid.expect(coreModel.data_mem_write_address_valid(i).B)
              dut.io.data_mem_write_sender(i).bits.address.expect(coreModel.data_mem_write_address_bits(i).U)
              dut.io.data_mem_write_sender(i).bits.data.expect(coreModel.data_mem_write_data_bits(i).U)
            }
          }
      }
    }
  }
}
