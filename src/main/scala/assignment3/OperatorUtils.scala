package assignment3

/** Operator character constants for consistent usage across the compiler. */
object Operators:
  // Unary operators
  val NEG = '~'
  val NOT = '!'

  // Binary operators - arithmetic
  val ADD = '+'
  val SUB = '-'
  val MUL = '*'
  val DIV = '/'
  val MOD = '%'

  // Binary operators - logical/bitwise
  val AND = '&'
  val OR = '|'

  // Binary operators - comparison
  val LT = '<'
  val GT = '>'
  val EQ = '='

  val allBinary: Set[Char] = Set(ADD, SUB, MUL, DIV, MOD, AND, OR, LT, GT, EQ)
  val allUnary: Set[Char] = Set(NEG, NOT)

object OperatorUtils {
  import Operators._

  // Lazy-initialized instruction cache for better performance
  private lazy val unopInstructions: Map[Char, String] = Map(
    NEG -> new SamBuilder().pushImm(-1).mul().toString,
    NOT -> new SamBuilder().pushImm(1).add().pushImm(2).mod().toString
  )

  private lazy val binopInstructions: Map[Char, String] = Map(
    ADD -> new SamBuilder().add().toString,
    SUB -> new SamBuilder().sub().toString,
    MUL -> new SamBuilder().mul().toString,
    DIV -> new SamBuilder().div().toString,
    MOD -> new SamBuilder().mod().toString,
    AND -> new SamBuilder().andB().toString,
    OR  -> new SamBuilder().orB().toString,
    GT  -> new SamBuilder().greater().toString,
    LT  -> new SamBuilder().less().toString,
    EQ  -> new SamBuilder().equal().toString
  )

  /** Get unary operator code, returning Either for diagnostic-first flow. */
  def getUnopE(op: Char): Either[String, String] =
    unopInstructions.get(op).toRight(s"Invalid unary operator: $op")

  /** Get binary operator code, returning Either for diagnostic-first flow. */
  def getBinopE(op: Char): Either[String, String] =
    binopInstructions.get(op).toRight(s"Invalid binary operator: $op")

  /** Legacy throwing API - prefer getUnopE for new code. */
  @deprecated("Use getUnopE instead", "2.0")
  def getUnop(op: Char): String =
    getUnopE(op).getOrElse(throw TypeErrorException(s"getUnop received invalid input: $op", -1))

  /** Legacy throwing API - prefer getBinopE for new code. */
  @deprecated("Use getBinopE instead", "2.0")
  def getBinop(op: Char): String =
    getBinopE(op).getOrElse(throw TypeErrorException(s"getBinop received invalid input: $op", -1))

  @deprecated("Use getBinopTypeE instead", "2.0")
  def getBinopType(op: Char): BinopType = op match {
    case ADD | SUB | MUL | DIV | MOD => BinopType.ARITHMETIC
    case AND | OR => BinopType.BITWISE
    case GT | LT | EQ => BinopType.COMPARISON
    case other => throw TypeErrorException(s"categorizeBinop received invalid input: $other", -1)
  }

  def getBinopTypeE(op: Char): Either[String, BinopType] = op match {
    case ADD | SUB | MUL | DIV | MOD => Right(BinopType.ARITHMETIC)
    case AND | OR => Right(BinopType.BITWISE)
    case GT | LT | EQ => Right(BinopType.COMPARISON)
    case other => Left(s"Invalid binary operator: $other")
  }
}
