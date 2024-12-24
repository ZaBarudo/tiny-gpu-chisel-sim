package gpu

import chisel3._
import chisel3.util._

import dcr.DeviceControlRegister
import controller.Controller
import dispatch.Dispatch
import core.Core

class Gpu(
  DataMemAddrBits:       Int = 8,
  DataMemDataBits:       Int = 8,
  DataMemNumChannels:    Int = 4,
  ProgramMemAddrBits:    Int = 8,
  ProgramMemDataBits:    Int = 16,
  ProgramMemNumChannels: Int = 1,
  NumCores:              Int = 2,
  ThreadsPerBlock:       Int = 4)
    extends Module {
  val io = IO(new Bundle {
    // Kernel Execution
    val start = Input(Bool())
    val done  = Output(Bool())

    // Device Control Register
    val device_control_write_enable = Input(Bool())
    val device_control_data         = Input(UInt(8.W))

    // Program Memory
    val program_mem_read_sender = Vec(ProgramMemNumChannels, new DecoupledIO(UInt(ProgramMemAddrBits.W)))
    val program_mem_read_data   = Input(Vec(ProgramMemNumChannels, UInt(ProgramMemDataBits.W)))

    // Data Memory
    val data_mem_read_sender  = Vec(DataMemNumChannels, new DecoupledIO(UInt(DataMemAddrBits.W)))
    val data_mem_read_data    = Input(Vec(DataMemNumChannels, UInt(DataMemDataBits.W)))
    val data_mem_write_sender = Vec(
      DataMemNumChannels,
      DecoupledIO(new Bundle {
        val address = UInt(DataMemAddrBits.W)
        val data    = UInt(DataMemDataBits.W)
      })
    )
  })

// Control
  val thread_count = RegInit(0.U(8.W))

  // Compute Core State
  val core_start        = RegInit(0.U(NumCores.W))
  val core_reset        = RegInit(0.U(NumCores.W))
  val core_done         = RegInit(0.U(NumCores.W))
  val core_block_id     = RegInit(VecInit(Seq.fill(NumCores)(0.U(8.W))))
  val core_thread_count = RegInit(VecInit(Seq.fill(NumCores)(0.U(log2Ceil(ThreadsPerBlock).W))))

  // LSU <> Data Memory Controller Channels
  val NumLSUs = NumCores * ThreadsPerBlock
//   val lsu_mem_read_data           = Reg(VecInit(Seq.fill(NumLSUs)(UInt(DataMemDataBits.W))))
//   val lsu_mem_read_address_sender = Reg(VecInit(Seq.fill(NumLSUs)(new DecoupledIO(UInt(DataMemAddrBits.W)))))
//   val lsu_mem_write_sender        = Reg(VecInit(Seq.fill(NumLSUs)(DecoupledIO(new Bundle {
//     val address = UInt(DataMemAddrBits.W)
//     val data    = UInt(DataMemDataBits.W)
//   }))))

  // Fetcher <> Program Memory Controller Channels
  // Fetcher <> Program Memory Controller Channels
  val NumFetchers = NumCores

  // Device Control Register
  val dcr = Module(new DeviceControlRegister())
//   dcr.io.device_control_write_enable := io.device_control_write_enable
//   dcr.io.device_control_data         := io.device_control_data
//   dcr.io.thread_count                := thread_count

  // Data Memory Controller
  val data_memory_controller = Module(new Controller(DataMemAddrBits, DataMemDataBits, NumLSUs, DataMemNumChannels))

  // Program Memory Controller
  val program_memory_controller = Module(new Controller(ProgramMemAddrBits, ProgramMemDataBits, NumFetchers, ProgramMemNumChannels, 0))


  // Dispatcher
  val dispatcher = Module(new Dispatch(NumCores, ThreadsPerBlock))

  // Compute Cores
  for (i <- 0 until NumCores) {
    val core = Module(
      val core = Module(new Core(DataMemAddrBits, DataMemDataBits, ProgramMemAddrBits, ProgramMemDataBits, ThreadsPerBlock))
      core.io.start := dispatcher.io.core_start(i)
      core.io.block_id := dispatcher.io.core_block_id(i)
      core.io.thread_count := dispatcher.io.core_thread_count(i)

      core.io.program_mem_read_sender <> program_memory_controller.io.consumer_read_addr_receiver(i)
      core.io.program_mem_read_data := program_memory_controller.io.consumer_read_data(i)

      core.io.data_mem_read_data := data_memory_controller.io.consumer_read_data(i)
      core.io.data_mem_read_address_sender <> data_memory_controller.io.consumer_read_addr_receiver(i)
      core.io.data_mem_write_sender <> data_memory_controller.io.consumer_write_receiver(i)

      dispatcher.io.core_done(i) := core.io.done
    )
  }
}
