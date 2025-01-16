package gpu

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
  case object Brnzp  extends Token
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
  case class Immediate(value: Int) extends Token // Added for #[NUMBER] format
  case class Register(number: Int) extends Token // Added for Rxx format

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

        // println("*Debug: ", codeLine)

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
      //   case ";"                                    => Comment
      // case ","                                    => Comma
      case "const"                                => Const
      case "nop"                                  => Nop
      case "brnzp"                                => Brnzp
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
  case object Imm       extends RegType
  case object Reg       extends RegType
  case object BlockIdx  extends RegType
  case object BlockDim  extends RegType
  case object ThreadIdx extends RegType
}

class Instruction(op: Token, args: Vector[RegType]) {
  def getOp:   Token           = op
  def getArgs: Vector[RegType] = args
}

class AsmParser(tokens: Vector[Token]) {
  private var threadCount: Int                 = 0
  private var dataArrays:  Vector[Vector[Int]] = Vector.empty

  def parse(input: String): Unit = {
    // var i = 0
    // while (i < tokens.length) {
    //   val tok = tokens(i)
    //   tok match {
    //     case Token.Thread => {
    //       val n = tokens(i + 1)
    //       if (n.isInstanceOf[Token.Number]) {
    //         threadCount = n.asInstanceOf[Token.Number].value
    //         i += 2
    //       } else {
    //         throw new Exception("Invalid thread count")
    //       }
    //     }
    //     case Token.Data   => {
    //       val n = tokens(i + 1)
    //       if (n.isInstanceOf[Token.Number]) {
    //         dataArrays = tok.asInstanceOf[Token.Number].value
    //         i += 2
    //       } else {
    //         throw new Exception("Invalid data array")
    //       }
    //     }
    //   }

    //   i += 1
    // }
  }

  // Getter methods
  def getThreadCount: Int                 = threadCount
  def getDataArrays:  Vector[Vector[Int]] = dataArrays
}

object AsmParserTest {
  def main(args: Array[String]): Unit = {

    val matAddSrc = """
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

    val lexer  = new Lexer()
    val parser = new AsmParser(lexer.tokenize(matAddSrc))

    // parser.parse(input)

    println("Thread count: " + parser.getThreadCount)
    println("\nData arrays:")
    parser.getDataArrays.zipWithIndex.foreach { case (array, i) =>
      println(s"Array $i: ${array.mkString(", ")}")
    }
  }
}
