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
import statecode.RegInputOp
import statecode.AluOpCode

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
    // Fetcher outputs
    var fetcher_state = FetcherState.IDLE
    var read_valid    = false
    var read_address  = 0
    var instruction   = 0
    // Fetcher end

    // Decoder outputs
    var decoded_rd_address         = 0
    var decoded_rs_address         = 0
    var decoded_rt_address         = 0
    var decoded_nzp                = 0
    var decoded_immediate          = 0
    var decoded_reg_write_enable   = false
    var decoded_mem_read_enable    = false
    var decoded_mem_write_enable   = false
    var decoded_nzp_write_enable   = false
    var decoded_reg_input_mux      = RegInputOp.ARITHMETIC
    var decoded_alu_arithmetic_mux = AluOpCode.ADD
    var decoded_alu_output_mux     = false
    var decoded_pc_mux             = false
    var decoded_ret                = false
    // Decoder end

    // Scheduler outputs
    var current_pc = 0
    var core_state = CoreState.IDLE
    var done       = false
    // Scheduler end

    // ALU outputs
    var alu_out = Array.fill(ThreadsPerBlock)(0)
    // ALU end

    // PC output
    var next_pc = Array.fill(ThreadsPerBlock)(0)
    // PC end

    // LSU outputs
    // var lsu_read_valid    = Array.fill(ThreadsPerBlock)(false)
    // var lsu_read_address  = Array.fill(ThreadsPerBlock)(0)
    // var lsu_write_valid   = Array.fill(ThreadsPerBlock)(false)
    // var lsu_write_address = Array.fill(ThreadsPerBlock)(0)
    // var lsu_write_data    = Array.fill(ThreadsPerBlock)(0)
    var lsu_state = Array.fill(ThreadsPerBlock)(LSUState.IDLE)
    var lsu_out   = Array.fill(ThreadsPerBlock)(0)
    // LSU end

    // Register Files outputs
    var regout_rs = Array.fill(ThreadsPerBlock)(0)
    var regout_rt = Array.fill(ThreadsPerBlock)(0)
    // Register end

    def save(model: fetcher.FetcherModel) = {
      fetcher_state = model.fetcher_state
      read_valid = model.read_valid
      read_address = model.read_address
      instruction = model.instruction
    }

    def save(model: decoder.DecoderModel) = {
      decoded_rd_address = model.decoded_rd_address
      decoded_rs_address = model.decoded_rs_address
      decoded_rt_address = model.decoded_rt_address
      decoded_nzp = model.decoded_nzp
      decoded_immediate = model.decoded_immediate
      decoded_reg_write_enable = model.decoded_reg_write_enable
      decoded_mem_read_enable = model.decoded_mem_read_enable
      decoded_mem_write_enable = model.decoded_mem_write_enable
      decoded_nzp_write_enable = model.decoded_nzp_write_enable
      decoded_reg_input_mux = model.decoded_reg_input_mux
      decoded_alu_arithmetic_mux = model.decoded_alu_arithmetic_mux
      decoded_alu_output_mux = model.decoded_alu_output_mux
      decoded_pc_mux = model.decoded_pc_mux
      decoded_ret = model.decoded_ret
    }

    def save(model: scheduler.SchedulerModel) = {
      core_state = model.core_state
      current_pc = model.current_pc
      done = model.done
    }

    def save(model: alu.AluModel, idx: Int) = {
      alu_out(idx) = model.alu_out
    }

    def save(model: pc.PCModel, idx: Int) = {
      next_pc(idx) = model.next_pc
    }

    def save(model: lsu.LsuModel, idx: Int) = {
      // lsu_read_valid(idx) = model.read_valid
      // lsu_read_address(idx) = model.read_address
      // lsu_write_valid(idx) = model.write_valid
      // lsu_write_address(idx) = model.write_address
      // lsu_write_data(idx) = model.write_data
      lsu_state(idx) = model.lsu_state
      lsu_out(idx) = model.output_data
    }

    def save(model: registers.RegModel, idx: Int) = {
      regout_rs(idx) = model.rs_out()
      regout_rt(idx) = model.rt_out()
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

  // debug var
  var cycle = 0

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
      instruction = prevCycleData.instruction
    )

    // Update scheduler
    schedulerModel.update(
      start = start,
      mem_read_enable = prevCycleData.decoded_mem_read_enable,
      mem_write_enable = prevCycleData.decoded_mem_write_enable,
      decoded_ret = prevCycleData.decoded_ret,
      fetcher_state = prevCycleData.fetcher_state,
      lsu_state = prevCycleData.lsu_state,
      next_pc = prevCycleData.next_pc
    )

    // Update per-thread components
    for (i <- 0 until ThreadsPerBlock) {
      val enable = i < thread_count

      val rd_in = prevCycleData.decoded_rd_address
      val rs_in = prevCycleData.decoded_rs_address
      val rt_in = prevCycleData.decoded_rt_address

      // Update ALU
      aluModels(i).update(
        enable = enable,
        core_state = prevCycleData.core_state,
        rs = prevCycleData.regout_rs(i),
        rt = prevCycleData.regout_rt(i),
        arithmetic_mux = prevCycleData.decoded_alu_arithmetic_mux,
        output_mux = prevCycleData.decoded_alu_output_mux
      )

      // Update LSU
      lsuModels(i).update(
        enable = enable,
        core_state = prevCycleData.core_state,
        read_enable = prevCycleData.decoded_mem_read_enable,
        write_enable = prevCycleData.decoded_mem_write_enable,
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
        core_state = prevCycleData.core_state,
        rs = rs_in,
        rd = rd_in,
        rt = rt_in,
        decoded_reg_write_enable = prevCycleData.decoded_reg_write_enable,
        decoded_reg_input_mux = prevCycleData.decoded_reg_input_mux,
        decoded_immediate = prevCycleData.decoded_immediate,
        alu_out = prevCycleData.alu_out(i),
        lsu_out = prevCycleData.lsu_out(i)
      )

      // Update PC
      pcModels(i).update(
        enable = enable,
        core_state = prevCycleData.core_state,
        decoded_nzp = prevCycleData.decoded_nzp,
        decoded_immediate = prevCycleData.decoded_immediate,
        decoded_nzp_write_enable = prevCycleData.decoded_nzp_write_enable,
        decoded_pc_mux = prevCycleData.decoded_pc_mux,
        alu_out = prevCycleData.alu_out(i),
        current_pc = prevCycleData.current_pc
      )

      prevCycleData.save(aluModels(i), i)
      prevCycleData.save(lsuModels(i), i)
      prevCycleData.save(regfileModels(i), i)
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

    prevCycleData.save(fetcherModel)
    prevCycleData.save(decoderModel)
    prevCycleData.save(schedulerModel)

    // print and update debug info
    // println(
    //   "#Model After Cycle: " + cycle + ", CoreState: " + prevCycleData.core_state + ", PC: " + prevCycleData.current_pc + ", Fetcher State: " + fetcherModel.fetcher_state
    // )
    cycle += 1
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

          // println("########## Start ##########")

          var cnt       = 0
          val rng       = new scala.util.Random(42) // 42 is the seed for reproducibility
          val coreModel = new CoreModel()
          while (cnt < 10000) {
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

            dut.clock.step()

            // println(s"\n=== Random Values for Cycle $cnt ===")
            // println(s"Block ID: $block_id")
            // println(s"Thread Count: $thread_count")
            // println(s"Program Memory Read: [ready=$mem_read_ready, data=$mem_read_data]")
            // println(s"Read Data:    [${data_mem_read_data.mkString(", ")}]")
            // println(s"Read Ready:   [${data_mem_read_ready.mkString(", ")}]")
            // println(s"Write Ready:  [${data_mem_write_ready.mkString(", ")}]")
            // println()
            // println("--Aft Core State: " + dut.io.core_state.peekValue() + ", current_pc: " + dut.io.current_pc.peek())

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

            cnt += 1
          }
      }
    }
  }
}
