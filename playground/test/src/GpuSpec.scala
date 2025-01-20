// package gpu

// import chisel3._
// import chisel3.util._
// import chisel3.experimental.BundleLiterals._
// import chisel3.simulator.EphemeralSimulator._
// import org.scalatest.freespec.AnyFreeSpec
// import org.scalatest.matchers.must.Matchers

// import core.CoreModel
// import dcr.DeviceControlRegisterModel
// import controller.ControllerModel
// import dispatch.DispatchModel

// class GpuModel(
//   DataMemAddrBits:       Int = 8,
//   DataMemDataBits:       Int = 8,
//   DataMemNumChannels:    Int = 4,
//   ProgramMemAddrBits:    Int = 8,
//   ProgramMemDataBits:    Int = 16,
//   ProgramMemNumChannels: Int = 1,
//   NumCores:              Int = 2,
//   ThreadsPerBlock:       Int = 4
// ) {
//   // Submodule models
//   private var dcrModel = new DeviceControlRegisterModel()
//   private var dispatchModel = new DispatchModel(NumCores, ThreadsPerBlock)
  
//   // Memory controllers
//   private var dataMemoryController = new ControllerModel(
//     DataMemAddrBits, 
//     DataMemDataBits, 
//     NumCores * ThreadsPerBlock, 
//     DataMemNumChannels
//   )
//   private var programMemoryController = new ControllerModel(
//     ProgramMemAddrBits, 
//     ProgramMemDataBits, 
//     NumCores, 
//     ProgramMemNumChannels
//   )
  
//   // Core models
//   private var coreModels = Array.fill(NumCores)(
//     new CoreModel(
//       DataMemAddrBits,
//       DataMemDataBits,
//       ProgramMemAddrBits,
//       ProgramMemDataBits,
//       ThreadsPerBlock
//     )
//   )

//   // GPU outputs
//   var done = false
//   var program_mem_read_valid = Array.fill(ProgramMemNumChannels)(false)
//   var program_mem_read_bits = Array.fill(ProgramMemNumChannels)(0)
//   var data_mem_read_valid = Array.fill(DataMemNumChannels)(false)
//   var data_mem_read_bits = Array.fill(DataMemNumChannels)(0)
//   var data_mem_write_valid = Array.fill(DataMemNumChannels)(false)
//   var data_mem_write_address = Array.fill(DataMemNumChannels)(0)
//   var data_mem_write_data = Array.fill(DataMemNumChannels)(0)

//   def update(
//     // Control inputs
//     start: Boolean,
//     device_control_write_enable: Boolean,
//     device_control_data: Int,
    
//     // Memory inputs
//     program_mem_read_ready: Array[Boolean],
//     program_mem_read_data: Array[Int],
//     data_mem_read_ready: Array[Boolean],
//     data_mem_read_data: Array[Int],
//     data_mem_write_ready: Array[Boolean]
//   ): Unit = {
//     // Update DCR
//     dcrModel.update(
//       device_control_write_enable,
//       device_control_data
//     )

//     // Update dispatch unit
//     val core_done = coreModels.map(_.done)
//     dispatchModel.update(
//       start = start,
//       thread_count = dcrModel.thread_count,
//       core_done = core_done
//     )

//     // Update memory controllers
//     programMemoryController.update(
//       mem_read_ready = program_mem_read_ready,
//       mem_read_data = program_mem_read_data
//     )

//     dataMemoryController.update(
//       mem_read_ready = data_mem_read_ready,
//       mem_read_data = data_mem_read_data,
//       mem_write_ready = data_mem_write_ready
//     )

//     // Update each core
//     for (i <- 0 until NumCores) {
//       val core_start = dispatchModel.core_start(i)
//       val core_block_id = dispatchModel.core_block_id(i)
//       val core_thread_count = dispatchModel.core_thread_count(i)

//       // Get memory signals for this core
//       val prog_mem_read_data = programMemoryController.consumer_read_data(i)
//       val prog_mem_read_ready = programMemoryController.consumer_read_ready(i)

//       val data_mem_read_data = Array.tabulate(ThreadsPerBlock)(j => 
//         dataMemoryController.consumer_read_data(i * ThreadsPerBlock + j)
//       )
//       val data_mem_read_ready = Array.tabulate(ThreadsPerBlock)(j =>
//         dataMemoryController.consumer_read_ready(i * ThreadsPerBlock + j)
//       )
//       val data_mem_write_ready = Array.tabulate(ThreadsPerBlock)(j =>
//         dataMemoryController.consumer_write_ready(i * ThreadsPerBlock + j)
//       )

//       // Update core model
//       coreModels(i).update(
//         start = core_start,
//         block_id = core_block_id,
//         thread_count = core_thread_count,
//         program_mem_read_ready = prog_mem_read_ready,
//         program_mem_read_data = prog_mem_read_data,
//         data_mem_read_data = data_mem_read_data,
//         data_mem_read_ready = data_mem_read_ready,
//         data_mem_write_ready = data_mem_write_ready
//       )

//       // Update memory controller inputs from core outputs
//       programMemoryController.updateConsumerInput(
//         i,
//         coreModels(i).program_mem_read_address_valid,
//         coreModels(i).program_mem_read_address_bits
//       )

//       for (j <- 0 until ThreadsPerBlock) {
//         val idx = i * ThreadsPerBlock + j
//         dataMemoryController.updateConsumerInput(
//           idx,
//           coreModels(i).data_mem_read_address_valid(j),
//           coreModels(i).data_mem_read_address_bits(j),
//           coreModels(i).data_mem_write_address_valid(j),
//           coreModels(i).data_mem_write_address_bits(j),
//           coreModels(i).data_mem_write_data_bits(j)
//         )
//       }
//     }

//     // Update outputs
//     done = dispatchModel.done

//     // Update memory outputs
//     for (i <- 0 until ProgramMemNumChannels) {
//       program_mem_read_valid(i) = programMemoryController.mem_read_valid(i)
//       program_mem_read_bits(i) = programMemoryController.mem_read_bits(i)
//     }

//     for (i <- 0 until DataMemNumChannels) {
//       data_mem_read_valid(i) = dataMemoryController.mem_read_valid(i)
//       data_mem_read_bits(i) = dataMemoryController.mem_read_bits(i)
//       data_mem_write_valid(i) = dataMemoryController.mem_write_valid(i)
//       data_mem_write_address(i) = dataMemoryController.mem_write_address(i)
//       data_mem_write_data(i) = dataMemoryController.mem_write_data(i)
//     }
//   }
// }

// class GpuSpec extends AnyFreeSpec with Matchers {
//   "Test GPU" - {
//     "should match model behavior with random inputs" in {
//       val DataMemAddrBits = 8
//       val DataMemDataBits = 8
//       val DataMemNumChannels = 4
//       val ProgramMemAddrBits = 8
//       val ProgramMemDataBits = 16
//       val ProgramMemNumChannels = 1
//       val NumCores = 2
//       val ThreadsPerBlock = 4

//       simulate(new Gpu(
//         DataMemAddrBits,
//         DataMemDataBits,
//         DataMemNumChannels,
//         ProgramMemAddrBits,
//         ProgramMemDataBits,
//         ProgramMemNumChannels,
//         NumCores,
//         ThreadsPerBlock
//       )) { dut =>
//         // Reset the DUT
//         dut.reset.poke(true.B)
//         dut.clock.step()
//         dut.reset.poke(false.B)
//         dut.clock.step()

//         var cnt = 0
//         val rng = new scala.util.Random(42) // 42 is the seed for reproducibility
//         val gpuModel = new GpuModel(
//           DataMemAddrBits,
//           DataMemDataBits,
//           DataMemNumChannels,
//           ProgramMemAddrBits,
//           ProgramMemDataBits,
//           ProgramMemNumChannels,
//           NumCores,
//           ThreadsPerBlock
//         )

//         while (cnt < 10000) {
//           // Generate random inputs
//           val start = rng.nextBoolean()
//           val device_control_write_enable = rng.nextBoolean()
//           val device_control_data = rng.nextInt(256)
          
//           val program_mem_read_ready = Array.fill(ProgramMemNumChannels)(rng.nextBoolean())
//           val program_mem_read_data = Array.fill(ProgramMemNumChannels)(rng.nextInt(65536)) // 16-bit data
          
//           val data_mem_read_ready = Array.fill(DataMemNumChannels)(rng.nextBoolean())
//           val data_mem_read_data = Array.fill(DataMemNumChannels)(rng.nextInt(256))
//           val data_mem_write_ready = Array.fill(DataMemNumChannels)(rng.nextBoolean())

//           // Apply inputs to DUT
//           dut.io.start.poke(start.B)
//           dut.io.device_control_write_enable.poke(device_control_write_enable.B)
//           dut.io.device_control_data.poke(device_control_data.U)

//           for (i <- 0 until ProgramMemNumChannels) {
//             dut.io.program_mem_read_sender(i).ready.poke(program_mem_read_ready(i).B)
//             dut.io.program_mem_read_data(i).poke(program_mem_read_data(i).U)
//           }

//           for (i <- 0 until DataMemNumChannels) {
//             dut.io.data_mem_read_sender(i).ready.poke(data_mem_read_ready(i).B)
//             dut.io.data_mem_read_data(i).poke(data_mem_read_data(i).U)
//             dut.io.data_mem_write_sender(i).ready.poke(data_mem_write_ready(i).B)
//           }

//           dut.clock.step()

//           // Update model with same inputs
//           gpuModel.update(
//             start = start,
//             device_control_write_enable = device_control_write_enable,
//             device_control_data = device_control_data,
//             program_mem_read_ready = program_mem_read_ready,
//             program_mem_read_data = program_mem_read_data,
//             data_mem_read_ready = data_mem_read_ready,
//             data_mem_read_data = data_mem_read_data,
//             data_mem_write_ready = data_mem_write_ready
//           )

//           // Verify outputs
//           dut.io.done.expect(gpuModel.done.B)

//           // Verify program memory interface
//           for (i <- 0 until ProgramMemNumChannels) {
//             dut.io.program_mem_read_sender(i).valid.expect(gpuModel.program_mem_read_valid(i).B)
//             dut.io.program_mem_read_sender(i).bits.expect(gpuModel.program_mem_read_bits(i).U)
//           }

//           // Verify data memory interface
//           for (i <- 0 until DataMemNumChannels) {
//             dut.io.data_mem_read_sender(i).valid.expect(gpuModel.data_mem_read_valid(i).B)
//             dut.io.data_mem_read_sender(i).bits.expect(gpuModel.data_mem_read_bits(i).U)
//             dut.io.data_mem_write_sender(i).valid.expect(gpuModel.data_mem_write_valid(i).B)
//             dut.io.data_mem_write_sender(i).bits.address.expect(gpuModel.data_mem_write_address(i).U)
//             dut.io.data_mem_write_sender(i).bits.data.expect(gpuModel.data_mem_write_data(i).U)
//           }

//           cnt += 1
//         }
//       }
//     }
//   }
// }