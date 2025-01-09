package controller

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class ControllerModel {
  var controller_state         = Array.fill(3)(ControlState.IDLE)
  var mem_read_valid           = Array.fill(1)(false)
  var mem_read_address         = Array.fill(1)(0)
  var mem_write_valid          = Array.fill(1)(false)
  var mem_write_address        = Array.fill(1)(0)
  var mem_write_data           = Array.fill(1)(0)
  var consumer_read_ready      = Array.fill(4)(false)
  var consumer_read_data       = Array.fill(4)(0)
  var consumer_write_ready     = Array.fill(4)(false)
  var channel_serving_consumer = Array.fill(4)(false)
  var current_consumer         = Array.fill(1)(0)

  def update(
    consumer_read_valid:  Array[Boolean],
    consumer_read_addr:   Array[Int],
    consumer_write_valid: Array[Boolean],
    consumer_write_addr:  Array[Int],
    consumer_write_data:  Array[Int],
    mem_read_ready:       Array[Boolean],
    mem_read_data:        Array[Int],
    mem_write_ready:      Array[Boolean]
  ): Unit = {
    // Update model state for each channel
    for (i <- 0 until 1) { // Assuming NumChannels = 1
      controller_state(i) match {
        case ControlState.IDLE =>
          // Look for pending read requests
          val readRequests = consumer_read_valid.zipWithIndex.filter { case (valid, idx) =>
            valid && !channel_serving_consumer(idx)
          }
          if (readRequests.nonEmpty) {
            val (_, consumerIdx) = readRequests.last
            channel_serving_consumer(consumerIdx) = true
            current_consumer(i) = consumerIdx
            mem_read_valid(i) = true
            mem_read_address(i) = consumer_read_addr(consumerIdx)
            controller_state(i) = ControlState.READ_WAITING
          } else {
            // Look for pending write requests
            val writeRequests = consumer_write_valid.zipWithIndex.filter { case (valid, idx) =>
              valid && !channel_serving_consumer(idx)
            }
            if (writeRequests.nonEmpty) {
              val (_, consumerIdx) = writeRequests.last
              channel_serving_consumer(consumerIdx) = true
              current_consumer(i) = consumerIdx
              mem_write_valid(i) = true
              mem_write_address(i) = consumer_write_addr(consumerIdx)
              mem_write_data(i) = consumer_write_data(consumerIdx)
              controller_state(i) = ControlState.WRITE_WAITING
            }
          }

        case ControlState.READ_WAITING =>
          if (mem_read_ready(i)) {
            mem_read_valid(i) = false
            consumer_read_ready(current_consumer(i)) = true
            consumer_read_data(current_consumer(i)) = mem_read_data(i)
            controller_state(i) = ControlState.READ_RELAYING
          }

        case ControlState.WRITE_WAITING =>
          if (mem_write_ready(i)) {
            mem_write_valid(i) = false
            consumer_write_ready(current_consumer(i)) = true
            controller_state(i) = ControlState.WRITE_RELAYING
          }

        case ControlState.READ_RELAYING =>
          if (!consumer_read_valid(current_consumer(i))) {
            channel_serving_consumer(current_consumer(i)) = false
            consumer_read_ready(current_consumer(i)) = false
            controller_state(i) = ControlState.IDLE
          }

        case ControlState.WRITE_RELAYING =>
          if (!consumer_write_valid(current_consumer(i))) {
            channel_serving_consumer(current_consumer(i)) = false
            consumer_write_ready(current_consumer(i)) = false
            controller_state(i) = ControlState.IDLE
          }
      }
    }
  }

  def reset(): Unit = {
    controller_state = Array.fill(3)(ControlState.IDLE)
    mem_read_valid = Array.fill(1)(false)
    mem_read_address = Array.fill(1)(0)
    mem_write_valid = Array.fill(1)(false)
    mem_write_address = Array.fill(1)(0)
    mem_write_data = Array.fill(1)(0)
    consumer_read_ready = Array.fill(4)(false)
    consumer_read_data = Array.fill(4)(0)
    consumer_write_ready = Array.fill(4)(false)
    channel_serving_consumer = Array.fill(4)(false)
    current_consumer = Array.fill(1)(0)
  }
}

class ControllerSpec extends AnyFreeSpec with Matchers {
  "Test controller" - {
    "should match model behavior with random inputs" in {
      val AddrBits     = 8
      val DataBits     = 16
      val NumConsumers = 4
      val NumChannels  = 1

      simulate(new Controller(AddrBits, DataBits, NumConsumers, NumChannels)) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step()
        dut.reset.poke(false.B)
        dut.clock.step()

        var cnt             = 0
        val rng             = new scala.util.Random(42)
        val controllerModel = new ControllerModel()

        while (cnt < 10000) {
          // Generate random inputs
          val consumer_read_valid  = Array.fill(NumConsumers)(rng.nextBoolean())
          val consumer_read_addr   = Array.fill(NumConsumers)(rng.nextInt(256))
          val consumer_write_valid = Array.fill(NumConsumers)(rng.nextBoolean())
          val consumer_write_addr  = Array.fill(NumConsumers)(rng.nextInt(256))
          val consumer_write_data  = Array.fill(NumConsumers)(rng.nextInt(65536))
          val mem_read_ready       = Array.fill(NumChannels)(rng.nextBoolean())
          val mem_read_data        = Array.fill(NumChannels)(rng.nextInt(65536))
          val mem_write_ready      = Array.fill(NumChannels)(rng.nextBoolean())

          // Apply inputs to DUT
          for (i <- 0 until NumConsumers) {
            dut.io.consumer_read_addr_receiver(i).valid.poke(consumer_read_valid(i).B)
            dut.io.consumer_read_addr_receiver(i).bits.poke(consumer_read_addr(i).U)
            dut.io.consumer_write_receiver(i).valid.poke(consumer_write_valid(i).B)
            dut.io.consumer_write_receiver(i).bits.address.poke(consumer_write_addr(i).U)
            dut.io.consumer_write_receiver(i).bits.data.poke(consumer_write_data(i).U)
          }

          for (i <- 0 until NumChannels) {
            dut.io.mem_read_sender(i).ready.poke(mem_read_ready(i).B)
            dut.io.mem_read_data(i).poke(mem_read_data(i).U)
            dut.io.mem_write_sender(i).ready.poke(mem_write_ready(i).B)
          }

          dut.clock.step()

          // Update model
          controllerModel.update(
            consumer_read_valid,
            consumer_read_addr,
            consumer_write_valid,
            consumer_write_addr,
            consumer_write_data,
            mem_read_ready,
            mem_read_data,
            mem_write_ready
          )

          // Compare model with DUT
          for (i <- 0 until NumConsumers) {
            dut.io.consumer_read_addr_receiver(i).ready.expect(controllerModel.consumer_read_ready(i).B)
            dut.io.consumer_read_data(i).expect(controllerModel.consumer_read_data(i).U)
            dut.io.consumer_write_receiver(i).ready.expect(controllerModel.consumer_write_ready(i).B)
          }

          for (i <- 0 until NumChannels) {
            dut.io.mem_read_sender(i).valid.expect(controllerModel.mem_read_valid(i).B)
            dut.io.mem_read_sender(i).bits.expect(controllerModel.mem_read_address(i).U)
            dut.io.mem_write_sender(i).valid.expect(controllerModel.mem_write_valid(i).B)
            if (controllerModel.mem_write_valid(i)) {
              dut.io.mem_write_sender(i).bits.address.expect(controllerModel.mem_write_address(i).U)
              dut.io.mem_write_sender(i).bits.data.expect(controllerModel.mem_write_data(i).U)
            }
          }

          cnt += 1
        }
      }
    }
  }
}
