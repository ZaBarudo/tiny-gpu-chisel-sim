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

object AluOpCode extends ChiselEnum {
  val ADD, SUB, MUL, DIV = Value
}

object RegInputOp extends ChiselEnum {
  val ARITHMETIC = Value("b00".U)
  val MEMORY     = Value("b01".U)
  val CONSTANT   = Value("b10".U)
}

object DecoderState extends ChiselEnum {
  val NOP   = Value("b0000".U)
  val BRnzp = Value("b0001".U)
  val CMP   = Value("b0010".U)
  val ADD   = Value("b0011".U)
  val SUB   = Value("b0100".U)
  val MUL   = Value("b0101".U)
  val DIV   = Value("b0110".U)
  val LDR   = Value("b0111".U)
  val STR   = Value("b1000".U)
  val CONST = Value("b1001".U)
  val RET   = Value("b1111".U)
}

object ControlState extends ChiselEnum {
  val IDLE           = Value("b000".U)
  val READ_WAITING   = Value("b010".U)
  val WRITE_WAITING  = Value("b011".U)
  val READ_RELAYING  = Value("b100".U)
  val WRITE_RELAYING = Value("b101".U)
}

object FetcherState extends ChiselEnum {
  val IDLE     = Value("b000".U)
  val FETCHING = Value("b001".U)
  val FETCHED  = Value("b010".U)
}

object LSUState extends ChiselEnum {
  val IDLE, REQUESTING, WAITING, DONE = Value
}
