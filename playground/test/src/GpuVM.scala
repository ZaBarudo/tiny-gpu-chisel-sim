package gpu

import scala.collection.mutable.ArrayBuffer

// First, let's define our Token types
sealed trait Token
object Token {
  // Keywords
  case object Thread extends Token
  case object Data   extends Token
//   case object Comment extends Token
  // case object Comma  extends Token
  case object Const  extends Token
  case object Nop    extends Token
  case object Brn    extends Token
  case object Brz    extends Token
  case object Brp    extends Token
  case object Cmp    extends Token
  case object Add    extends Token
  case object Sub    extends Token
  case object Mul    extends Token
  case object Div    extends Token
  case object Ldr    extends Token
  case object Str    extends Token
  case object Ret    extends Token

  // Special variables
  case object BlockIdx  extends Token
  case object BlockDim  extends Token
  case object ThreadIdx extends Token

  // Values
  case class Number(value: Int)    extends Token
  case class Immediate(value: Int) extends Token // Added for #NUMBER format
  case class Register(number: Int) extends Token // Added for Rxx format

  case class LabelDef(name: String) extends Token
  case class LabelUse(name: String) extends Token

  // For any unrecognized tokens
  case class Invalid(value: String) extends Token
}

class Lexer {
  def tokenize(input: String): Vector[Token] = {
    // Split the input into lines and process each line
    val tokens = input
      .replace(",", " ")
      .split("\n")
      .flatMap { line =>
        // Split each line into parts, handling comments
        val parts    = line.split(";", 2)
        val codeLine = parts(0).trim

        // println("*Debug: " + codeLine)

        if (codeLine.isEmpty) Vector.empty
        else {
          // Split the line into words while preserving special characters
          val words = codeLine.split("\\s+").filter(_.nonEmpty)
          words.map(tokenizeWord).toVector
        }
      }
      .toVector

    // Report any invalid tokens
    val invalidTokens = tokens.collect { case Token.Invalid(value) => value }
    if (invalidTokens.nonEmpty) {
      println("Error: Invalid tokens found:")
      invalidTokens.foreach(token => println(s" - $token"))
      Vector.empty
    } else {
      tokens
    }
  }

  private def tokenizeWord(word: String): Token = {
    import Token._

    word.toLowerCase match {
      case ".thread" | ".threads"                 => Thread
      case ".data"                                => Data
      // case ";"                                    => Comment
      // case ","                                    => Comma
      case "const"                                => Const
      case "nop"                                  => Nop
      case "brn"                                  => Brn
      case "brz"                                  => Brz
      case "brp"                                  => Brp
      case "cmp"                                  => Cmp
      case "add"                                  => Add
      case "sub"                                  => Sub
      case "mul"                                  => Mul
      case "div"                                  => Div
      case "ldr"                                  => Ldr
      case "str"                                  => Str
      case "ret"                                  => Ret
      case "%blockidx"                            => BlockIdx
      case "%blockdim"                            => BlockDim
      case "%threadidx"                           => ThreadIdx
      // Handle register numbers (R0-R99)
      case reg if reg.toLowerCase.startsWith("r") =>
        try {
          val number = reg.substring(1).toInt
          if (number >= 0 && number <= 99) Register(reg.substring(1).toInt)
          else Invalid(reg)
        } catch {
          case _: NumberFormatException => Invalid(reg)
        }
      // Handle immediate numbers (#NUMBER)
      case imm if imm.startsWith("#")             =>
        try {
          val number = imm.substring(1, imm.length).toInt
          Immediate(number)
        } catch {
          case _: NumberFormatException => Invalid(imm)
        }
      case num if num.matches("-?\\d+")           => Number(num.toInt)
      case label if label.endsWith(":")           => LabelDef(label.dropRight(1))
      case use if use.matches("^[a-z_]*$")        => LabelUse(use)
      case other                                  => Invalid(other)
    }
  }
}

// Test the lexer
object LexerTest {
  def main(args: Array[String]): Unit = {
    val lexer = new Lexer()
    val input = """
        .threads 8
        .data 0 1 2 3 4 5 6 7          ; matrix A (1 x 8)
        .data 0 1 2 3 4 5 6 7          ; matrix B (1 x 8)

        MUL R0, %blockIdx, %blockDim
        ADD R0, R0, %threadIdx         ; i = blockIdx * blockDim + threadIdx

        CONST R1, #0                   ; baseA (matrix A base address)
        CONST R2, #8                   ; baseB (matrix B base address)
        CONST R3, #16                  ; baseC (matrix C base address)

        ADD R4, R1, R0                 ; addr(A[i]) = baseA + i
        LDR R4, R4                     ; load A[i] from global memory

        ADD R5, R2, R0                 ; addr(B[i]) = baseB + i
        LDR R5, R5                     ; load B[i] from global memory

        ADD R6, R4, R15                 ; C[i] = A[i] + B[i]

        ADD R7, R3, R0                 ; addr(C[i]) = baseC + i
        STR R7, R6                     ; store C[i] in global memory

        RET                            ; end of kernel
    """

    val tokens = lexer.tokenize(input)
    tokens.foreach(println)
  }
}

sealed trait RegType
object RegType {
  // Keywords
  case class Imm(value: Int)        extends RegType
  case class Reg(value: Int)        extends RegType
  case class LabelUse(name: String) extends RegType
  // case class BlockIdx()             extends RegType
  // case class BlockDim()             extends RegType
  // case class ThreadIdx()            extends RegType
}

class Instruction(op: Token, args: Vector[RegType]) {
  private var _args:                  Vector[RegType] = args
  def getOp:                          Token           = op
  def getArgs:                        Vector[RegType] = _args
  def setArgs(args: Vector[RegType]): Unit            = {
    _args = args
  }
}

class AsmParser() {
  val BlockIdxOffset  = 13
  val BlockDimOffset  = 14
  val ThreadIdxOffset = 15

  private var debug:            Boolean = false
  def setDebug(debug: Boolean): Unit    = {
    this.debug = debug
  }

  private var tokens:       Vector[Token]       = Vector.empty
  private var threadCount:  Int                 = 0
  private var dataArrays:   Vector[Vector[Int]] = Vector.empty
  private var instructions: Vector[Instruction] = Vector.empty
  private var labels:       Map[String, Int]    = Map.empty
  private var idx:          Int                 = 0
  def peek():               Token               = tokens(idx)
  def consume():            Token               = {
    val tok = peek()
    idx += 1
    tok
  }
  def lookahead():          Token               = tokens(idx + 1)
  def isEof():              Boolean             = idx >= tokens.length

  def parseReg(reg: Token): RegType = {
    reg match {
      case Token.Register(number) => RegType.Reg(number)
      case Token.BlockIdx         => RegType.Reg(BlockIdxOffset)
      case Token.BlockDim         => RegType.Reg(BlockDimOffset)
      case Token.ThreadIdx        => RegType.Reg(ThreadIdxOffset)
      case _                      => throw new IllegalArgumentException(s"Invalid register expression in token: $reg")
    }
  }

  def parseBinayOp(op: Token): Instruction = {
    try {
      val (r1, r2) = (consume(), consume())
      new Instruction(
        op,
        Vector(parseReg(r1), parseReg(r2))
      )
    } catch {
      case _: NoSuchElementException => throw new IllegalArgumentException("Invalid binary operation expression")
    }
  }

  def parseTernaryOp(op: Token): Instruction = {
    try {
      val (r1, r2, r3) = (consume(), consume(), consume())
      new Instruction(
        op,
        Vector(parseReg(r1), parseReg(r2), parseReg(r3))
      )
    } catch {
      case _: NoSuchElementException => throw new IllegalArgumentException("Invalid ternary operation expression")
    }
  }

  def parse(tokens: Vector[Token]): Unit = {
    this.tokens = tokens
    while (!isEof()) {
      val tok = consume()
      if (debug) {
        println(s"*Debug current token: $tok")
      }
      tok match {
        case Token.Thread         => {
          val num = consume()
          assert(num.isInstanceOf[Token.Number], "Invalid thread count")
          threadCount = num.asInstanceOf[Token.Number].value
        }
        case Token.Data           => {
          var array = Vector.empty[Int]
          while (peek().isInstanceOf[Token.Number]) {
            val n = consume()
            array = array :+ n.asInstanceOf[Token.Number].value
          }
          dataArrays = dataArrays :+ array
        }
        case Token.Const          => {
          val reg = consume()
          assert(reg.isInstanceOf[Token.Register], "Invalid Const expr")
          val imm = consume()
          assert(imm.isInstanceOf[Token.Immediate], "Invalid Const expr")

          val (r, i)      = (reg.asInstanceOf[Token.Register].number, imm.asInstanceOf[Token.Immediate].value)
          val instruction = new Instruction(Token.Const, Vector(RegType.Reg(r), RegType.Imm(i)))
          instructions = instructions :+ instruction
        }
        case Token.Nop            => {
          val instruction = new Instruction(Token.Nop, Vector.empty)
          instructions = instructions :+ instruction
        }
        case Token.Brn            => {
          val label       = consume()
          assert(label.isInstanceOf[Token.LabelUse], "Invalid Brn expr")
          val instruction =
            new Instruction(Token.Brn, Vector(RegType.LabelUse(label.asInstanceOf[Token.LabelUse].name)))
          instructions = instructions :+ instruction
        }
        case Token.Brz            => {
          val label       = consume()
          assert(label.isInstanceOf[Token.LabelUse], "Invalid Brz expr")
          val instruction =
            new Instruction(Token.Brz, Vector(RegType.LabelUse(label.asInstanceOf[Token.LabelUse].name)))
          instructions = instructions :+ instruction
        }
        case Token.Brp            => {
          val label       = consume()
          assert(label.isInstanceOf[Token.LabelUse], "Invalid Brp expr")
          val instruction =
            new Instruction(Token.Brp, Vector(RegType.LabelUse(label.asInstanceOf[Token.LabelUse].name)))
          instructions = instructions :+ instruction
        }
        case Token.LabelDef(name) => {
          labels = labels + (name -> instructions.length)
        }
        case Token.Cmp            => {
          instructions = instructions :+ parseBinayOp(Token.Cmp)
        }
        case Token.Add            => {
          instructions = instructions :+ parseTernaryOp(Token.Add)
        }
        case Token.Sub            => {
          instructions = instructions :+ parseTernaryOp(Token.Sub)
        }
        case Token.Mul            => {
          instructions = instructions :+ parseTernaryOp(Token.Mul)
        }
        case Token.Div            => {
          instructions = instructions :+ parseTernaryOp(Token.Div)
        }
        case Token.Ldr            => {
          instructions = instructions :+ parseBinayOp(Token.Ldr)
        }
        case Token.Str            => {
          instructions = instructions :+ parseBinayOp(Token.Str)
        }
        case Token.Ret            => {
          instructions = instructions :+ new Instruction(Token.Ret, Vector.empty)
        }
        case other                => {
          println(s"Unrecognized token: $other")
        }
      }
    }

    // post process:
    // Turn labels into immediate values
    instructions
      .filter(inst =>
        inst.getOp == Token.Brn || inst.getOp == Token.Brn || inst.getOp == Token.Brz || inst.getOp == Token.Brp
      )
      .foreach { inst =>
        val label = inst.getArgs.head.asInstanceOf[RegType.LabelUse].name
        val idx   = labels(label)
        inst.setArgs(Vector(RegType.Imm(idx)))
      }

  }

  // Getter methods
  def getThreadCount:  Int                 = threadCount
  def getDataArrays:   Vector[Vector[Int]] = dataArrays
  def getInstructions: Vector[Instruction] = instructions
}

object MatAddAsm {
  val src = """
        .threads 8
        .data 0 1 2 3 4 5 6 7          ; matrix A (1 x 8)
        .data 0 1 2 3 4 5 6 7          ; matrix B (1 x 8)

        MUL R0, %blockIdx, %blockDim
        ADD R0, R0, %threadIdx         ; i = blockIdx * blockDim + threadIdx

        CONST R1, #0                   ; baseA (matrix A base address)
        CONST R2, #8                   ; baseB (matrix B base address)
        CONST R3, #16                  ; baseC (matrix C base address)

        ADD R4, R1, R0                 ; addr(A[i]) = baseA + i
        LDR R4, R4                     ; load A[i] from global memory

        ADD R5, R2, R0                 ; addr(B[i]) = baseB + i
        LDR R5, R5                     ; load B[i] from global memory

        ADD R6, R4, R15                 ; C[i] = A[i] + B[i]

        ADD R7, R3, R0                 ; addr(C[i]) = baseC + i
        STR R7, R6                     ; store C[i] in global memory

        RET                            ; end of kernel
    """
}

object MatMulAsm {
  val src = """
    .threads 4
    .data 1 2 3 4                  ; matrix A (2 x 2)
    .data 1 2 3 4                  ; matrix B (2 x 2)

    MUL R0, %blockIdx, %blockDim
    ADD R0, R0, %threadIdx         ; i = blockIdx * blockDim + threadIdx

    CONST R1, #1                   ; increment
    CONST R2, #2                   ; N (matrix inner dimension)
    CONST R3, #0                   ; baseA (matrix A base address)
    CONST R4, #4                   ; baseB (matrix B base address)
    CONST R5, #8                   ; baseC (matrix C base address)

    DIV R6, R0, R2                 ; row = i // N
    MUL R7, R6, R2
    SUB R7, R0, R7                 ; col = i % N

    CONST R8, #0                   ; acc = 0
    CONST R9, #0                   ; k = 0

    LOOP:
      MUL R10, R6, R2
      ADD R10, R10, R9
      ADD R10, R10, R3             ; addr(A[i]) = row * N + k + baseA
      LDR R10, R10                 ; load A[i] from global memory

      MUL R11, R9, R2
      ADD R11, R11, R7
      ADD R11, R11, R4             ; addr(B[i]) = k * N + col + baseB
      LDR R11, R11                 ; load B[i] from global memory

      MUL R12, R10, R11
      ADD R8, R8, R12              ; acc = acc + A[i] * B[i]

      ADD R9, R9, R1               ; increment k

      CMP R9, R2
      BRn LOOP                    ; loop while k < N

    ADD R9, R5, R0                 ; addr(C[i]) = baseC + i
    STR R9, R8                     ; store C[i] in global memory

    RET                            ; end of kernel
    """
}

object AsmParserTest1 {
  def main(args: Array[String]): Unit = {
    val lexer  = new Lexer()
    val parser = new AsmParser()
    parser.parse(lexer.tokenize(MatAddAsm.src))

    println("Thread count: " + parser.getThreadCount)
    println("\nData arrays:")
    parser.getDataArrays.zipWithIndex.foreach { case (array, i) =>
      println(s"Array $i: ${array.mkString(", ")}")
    }
  }
}

object AsmParserTest2 {
  def main(args: Array[String]): Unit = {
    val lexer  = new Lexer()
    val tokens = lexer.tokenize(MatMulAsm.src)
    tokens.foreach(println)

    val parser = new AsmParser()
    println("\n################## Parse result ##################\n")

    parser.parse(tokens)

    println("Thread count: " + parser.getThreadCount)
    println("\nData arrays:")
    parser.getDataArrays.zipWithIndex.foreach { case (array, i) =>
      println(s"Array $i: ${array.mkString(", ")}")
    }

    parser.getInstructions.zipWithIndex.foreach { case (inst, i) =>
      println(s"Instruction $i: ${inst.getOp} ${inst.getArgs.mkString(", ")}")
    }
  }
}

class GpuVM(NumCores: Int = 2, ThreadsPerBlock: Int = 4, MemSize: Int = 256) {
  private var debug:            Boolean = false
  def setDebug(debug: Boolean): Unit    = {
    this.debug = debug
  }

  val NumOfThread = NumCores * ThreadsPerBlock

  private var registers: Vector[ArrayBuffer[Int]] = Vector.fill(NumOfThread)(ArrayBuffer.fill(16)(0))
  private var memory:    ArrayBuffer[Int]         = ArrayBuffer.empty

  private var pc:  Int                       = 0
  private var nzp: ArrayBuffer[Seq[Boolean]] = ArrayBuffer.fill(NumOfThread)(Seq.fill(3)(false))

  def getRegisters: Vector[ArrayBuffer[Int]] = registers
  def getMemory:    ArrayBuffer[Int]         = memory

  val BlockIdxOffset  = 13
  val BlockDimOffset  = 14
  val ThreadIdxOffset = 15

  def printRegister(threadIdx: Int): Unit = {
    print(s"Thread $threadIdx registers: [")
    // registers(threadIdx).map(v => f"$v%3d").mkString(", ")
    print(registers(threadIdx).foldLeft("")((acc, v) => acc + f"$v%3d, "))
    println("]")
  }

  def printAllRegisters(): Unit = {
    println("################## Registers ##################")
    for (threadIdx <- 0 until NumOfThread) {
      printRegister(threadIdx)
    }
    println()
  }

  def printMemory(): Unit = {
    println("***************** Memory *****************")
    memory.grouped(8).foreach(group => println(group.map(n => f"$n%4d").mkString(" ")))
    println()
  }

  def init(dataArrays: Vector[Vector[Int]]): Unit = {
    // Initialize blockIdx, blockDim, threadIdx for each thread's register file
    for (blockId <- 0 until NumCores) {
      for (threadId <- 0 until ThreadsPerBlock) {
        val threadIdx = blockId * ThreadsPerBlock + threadId
        registers(threadIdx)(BlockIdxOffset) = blockId
        registers(threadIdx)(BlockDimOffset) = ThreadsPerBlock
        registers(threadIdx)(ThreadIdxOffset) = threadId
      }
    }

    // copy data arrays to memory
    memory = ArrayBuffer.fill(MemSize)(0)
    var offset = 0
    for (array <- dataArrays) {
      for (v <- array) {
        memory(offset) = v
        offset += 1
      }
    }
  }

  def run(instructions: Vector[Instruction]): Unit = {
    while (pc < instructions.length) {
      val inst = instructions(pc)
      if (debug) {
        println(s"*Debug current instruction: ${inst.getOp} ${inst.getArgs.mkString(", ")}")
        printAllRegisters()
        printMemory()
        println("\n")
      }
      for (threadIdx <- 0 until NumOfThread) {
        def nextPc(): Int = {
          inst.getOp match {
            case Token.Nop   => {
              // do nothing
            }
            case Token.Brn   => {
              val labelIdx = inst.getArgs.head.asInstanceOf[RegType.Imm].value
              if (nzp(threadIdx)(0)) {
                return labelIdx
              }
            }
            case Token.Brz   => {
              val labelIdx = inst.getArgs.head.asInstanceOf[RegType.Imm].value
              if (nzp(threadIdx)(1)) {
                return labelIdx
              }
            }
            case Token.Brp   => {
              val labelIdx = inst.getArgs.head.asInstanceOf[RegType.Imm].value
              if (nzp(threadIdx)(2)) {
                return labelIdx
              }
            }
            case Token.Cmp   => {
              val s = inst.getArgs(0).asInstanceOf[RegType.Reg].value
              val t = inst.getArgs(1).asInstanceOf[RegType.Reg].value
              nzp(threadIdx) = Seq(
                registers(threadIdx)(s) < registers(threadIdx)(t),  // neg : idx 0
                registers(threadIdx)(s) == registers(threadIdx)(t), // zero: idx 1
                registers(threadIdx)(s) > registers(threadIdx)(t)   // pos : idx 2
              )
            }
            case Token.Add   => {
              val d = inst.getArgs(0).asInstanceOf[RegType.Reg].value
              val s = inst.getArgs(1).asInstanceOf[RegType.Reg].value
              val t = inst.getArgs(2).asInstanceOf[RegType.Reg].value
              registers(threadIdx)(d) = registers(threadIdx)(s) + registers(threadIdx)(t)
            }
            case Token.Sub   => {
              val d = inst.getArgs(0).asInstanceOf[RegType.Reg].value
              val s = inst.getArgs(1).asInstanceOf[RegType.Reg].value
              val t = inst.getArgs(2).asInstanceOf[RegType.Reg].value
              registers(threadIdx)(d) = registers(threadIdx)(s) - registers(threadIdx)(t)
            }
            case Token.Mul   => {
              val d = inst.getArgs(0).asInstanceOf[RegType.Reg].value
              val s = inst.getArgs(1).asInstanceOf[RegType.Reg].value
              val t = inst.getArgs(2).asInstanceOf[RegType.Reg].value
              registers(threadIdx)(d) = registers(threadIdx)(s) * registers(threadIdx)(t)
            }
            case Token.Div   => {
              val d = inst.getArgs(0).asInstanceOf[RegType.Reg].value
              val s = inst.getArgs(1).asInstanceOf[RegType.Reg].value
              val t = inst.getArgs(2).asInstanceOf[RegType.Reg].value
              registers(threadIdx)(d) = registers(threadIdx)(s) / registers(threadIdx)(t)
            }
            case Token.Ldr   => {
              val d = inst.getArgs(0).asInstanceOf[RegType.Reg].value
              val s = inst.getArgs(1).asInstanceOf[RegType.Reg].value
              registers(threadIdx)(d) = memory(registers(threadIdx)(s))
            }
            case Token.Str   => {
              val s = inst.getArgs(0).asInstanceOf[RegType.Reg].value
              val t = inst.getArgs(1).asInstanceOf[RegType.Reg].value
              // println(
              //   s"*Debug store: thread=$threadIdx, s=$s, t=$t, registers(threadIdx)(s)=${registers(threadIdx)(s)}, registers(threadIdx)(t)=${registers(threadIdx)(t)}"
              // )
              memory(registers(threadIdx)(s)) = registers(threadIdx)(t)
            }
            case Token.Const => {
              val (reg, imm) =
                (inst.getArgs.head.asInstanceOf[RegType.Reg].value, inst.getArgs.last.asInstanceOf[RegType.Imm].value)
              registers(threadIdx)(reg) = imm
            }
            case Token.Ret   => {
              return instructions.length
            }
            case _           => {
              throw new Exception(s"Unrecognized instruction: ${inst.getOp}")
            }
          }
          return pc + 1
        }
        val npc = nextPc()
        // for each instruction, only update pc once
        if (threadIdx == NumOfThread - 1) {
          pc = npc
        }
        if (debug) {
          println(s"Thread $threadIdx npc: $npc")
          printRegister(threadIdx)
          println(nzp(threadIdx))
          printMemory()
          println("\n")
        }
      }
    }
  }
}

object GpuVMAddTest {
  def main(args: Array[String]): Unit = {
    val lexer  = new Lexer()
    val tokens = lexer.tokenize(MatAddAsm.src)
    val parser = new AsmParser()
    parser.parse(tokens)

    println("################## MatAddTest ##################")
    println("################## Parse result ##################")
    parser.getDataArrays.foreach(println)

    println("################## VM init ##################")
    val vm = new GpuVM()
    vm.setDebug(true)
    vm.init(parser.getDataArrays)
    vm.printAllRegisters()
    vm.printMemory()

    println("################## Run result ##################")
    vm.run(parser.getInstructions)

    vm.printAllRegisters()
    vm.printMemory()
  }
}

object GpuVMMulTest {
  def main(args: Array[String]): Unit = {
    val lexer  = new Lexer()
    val tokens = lexer.tokenize(MatMulAsm.src)
    val parser = new AsmParser()
    parser.parse(tokens)

    println("################## MatMulTest ##################")
    println("################## Parse result ##################")
    parser.getDataArrays.foreach(println)

    println("################## VM init ##################")
    val vm = new GpuVM()
    // vm.setDebug(true)
    vm.init(parser.getDataArrays)
    vm.printAllRegisters()
    vm.printMemory()

    println("################## Run result ##################")
    vm.run(parser.getInstructions)

    vm.printAllRegisters()
    vm.printMemory()
  }
}
