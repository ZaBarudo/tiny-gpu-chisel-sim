package statecode

import chisel3._

object CoreState extends ChiselEnum {
  val IDLE    = Value("b000".U)
  val FETCH   = Value("b001".U)
  val DECODE  = Value("b010".U)
  val REQUEST = Value("b011".U)
  val WAIT    = Value("b100".U)
  val EXECUTE = Value("b101".U)
  val UPDATE  = Value("b110".U)
  val DONE    = Value("b111".U)
}
