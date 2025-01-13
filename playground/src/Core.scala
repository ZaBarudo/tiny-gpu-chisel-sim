package core

import chisel3._
import chisel3.util._
import statecode.CoreState

import fetcher.Fetcher
import decoder.Decoder
import scheduler.Scheduler
import registers.RegisterFiles
import alu.Alu
import lsu.MemLoadStoreUnit
import pc.ProgramCounter

// TODO: Change each submodules' IO Bundle,
//       and then connect them in a more Chisel way

// COMPUTE CORE
// > Handles processing 1 block at a time
// > The core also has it's own scheduler to manage control flow
// > Each core contains 1 fetcher & decoder, and register files, ALUs, LSUs, PC for each thread
class Core(
  DataMemAddrBits:    Int = 8,
  DataMemDataBits:    Int = 8,
  ProgramMemAddrBits: Int = 8,
  ProgramMemDataBits: Int = 16,
  ThreadsPerBlock:    Int = 4)
    extends Module {
  val io = IO(new Bundle {
    // Kernel Execution
    val start = Input(Bool())
    val done  = Output(Bool())

    // Block Metadata
    val block_id     = Input(UInt(8.W))
    val thread_count = Input(UInt(log2Ceil(ThreadsPerBlock).W))

    // Program Memory
    val program_mem_read_address_sender = new DecoupledIO(UInt(ProgramMemAddrBits.W))
    val program_mem_read_data           = Input(UInt(ProgramMemDataBits.W))

    // Data Memory

    val data_mem_read_data           = Input(Vec(ThreadsPerBlock, UInt(DataMemDataBits.W)))
    val data_mem_read_address_sender = Vec(ThreadsPerBlock, new DecoupledIO(UInt(DataMemAddrBits.W)))
    val data_mem_write_sender        = Vec(
      ThreadsPerBlock,
      new DecoupledIO(new Bundle {
        val address = UInt(DataMemAddrBits.W)
        val data    = UInt(DataMemDataBits.W)
      })
    )

    // debug outputs
    val core_state = Output(CoreState())
    val current_pc = Output(UInt(8.W))
  })

  val fetcher   = Module(new Fetcher(ProgramMemAddrBits, ProgramMemDataBits))
  val decoder   = Module(new Decoder())
  val scheduler = Module(new Scheduler(ThreadsPerBlock))

  // val core_state = RegNext(scheduler.io.core_state)
  // val current_pc = RegNext(scheduler.io.current_pc)
  val core_state = Wire(CoreState())
  val current_pc = Wire(UInt(8.W))
  core_state := scheduler.io.core_state
  current_pc := scheduler.io.current_pc

  // printf(cf"--Core State: $core_state, current PC: $current_pc, Fetcher State: ${fetcher.io.fetcher_state}\n")

  // Fetcher inputs connections (3/3)
  fetcher.io.core_state    := core_state
  fetcher.io.current_pc    := current_pc
  fetcher.io.mem_read_data := io.program_mem_read_data

  // Decoder inputs connections (2/2)
  decoder.io.core_state  := scheduler.io.core_state
  decoder.io.instruction := fetcher.io.instruction

  // Scheduler inputs connections (5/7)
  scheduler.io.start         := io.start
  scheduler.io.mem_rw_enable := decoder.io.mem_rw_enable
  scheduler.io.decoded_ret   := decoder.io.decoded_ret
  scheduler.io.fetcher_state := fetcher.io.fetcher_state

  // TODO: just use a for loop
  val compute_units = Seq
    .tabulate(ThreadsPerBlock)(i => {
      val alu     = Module(new Alu())
      val lsu     = Module(new MemLoadStoreUnit())
      val regfile = Module(new RegisterFiles(ThreadsPerBlock, i, DataMemDataBits))
      val pc      = Module(new ProgramCounter(DataMemDataBits, DataMemAddrBits))

      val enable = (i.U < io.thread_count)

      // alu inputs connections (6/6)
      alu.io.enable         := enable
      alu.io.core_state     := scheduler.io.core_state
      alu.io.reg_in         := regfile.io.reg_out
      alu.io.decoded_alu_op := decoder.io.decoded_alu_op

      // lsu inputs connections (7/7)
      lsu.io.enable        := enable
      lsu.io.core_state    := scheduler.io.core_state
      lsu.io.mem_rw_enable := decoder.io.mem_rw_enable
      lsu.io.mem_read_data := io.data_mem_read_data(i)
      lsu.io.reg_in        := regfile.io.reg_out

      // regfile inputs connections (11/11)
      regfile.io.enable                   := enable
      regfile.io.block_id                 := io.block_id
      regfile.io.core_state               := scheduler.io.core_state
      regfile.io.decoded_reg_address      := decoder.io.decoded_reg_address
      regfile.io.decoded_reg_write_enable := decoder.io.decoded_reg_write_enable
      regfile.io.decoded_reg_input_mux    := decoder.io.decoded_reg_input_mux
      regfile.io.decoded_immediate        := decoder.io.decoded_immediate
      regfile.io.alu_out                  := alu.io.alu_out
      regfile.io.lsu_out                  := lsu.io.lsu_out

      // pc inputs connections (8/8)
      pc.io.enable                   := enable
      pc.io.decoded_nzp              := decoder.io.decoded_nzp
      pc.io.decoded_immediate        := decoder.io.decoded_immediate
      pc.io.decoded_nzp_write_enable := decoder.io.decoded_nzp_write_enable
      pc.io.decoded_pc_mux           := decoder.io.decoded_pc_mux
      pc.io.alu_out                  := alu.io.alu_out
      pc.io.current_pc               := scheduler.io.current_pc
      pc.io.core_state               := scheduler.io.core_state

      // Connect to scheduler input (7/7)
      scheduler.io.lsu_state(i) := lsu.io.lsu_state
      scheduler.io.next_pc(i)   := pc.io.next_pc

      // Connect to core module outputs (5/8)
      io.data_mem_read_address_sender(i) <> lsu.io.mem_read_address_sender
      io.data_mem_write_sender(i) <> lsu.io.mem_write_sender

      (alu, lsu, regfile, pc)
    })

  // Connect to module outputs (8/8)
  io.done := scheduler.io.done
  io.program_mem_read_address_sender <> fetcher.io.mem_read_address_sender

  // debug
  io.core_state := core_state
  io.current_pc := current_pc
}
