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

/** Bidirectional mapping between BinaryOp enum and operator characters. */
object BinaryOpMapping:
  import Operators._
  import assignment3.ast.BinaryOp

  /** Convert BinaryOp to operator character. */
  def toChar(bop: BinaryOp): Char = bop match
    case BinaryOp.Add => ADD
    case BinaryOp.Sub => SUB
    case BinaryOp.Mul => MUL
    case BinaryOp.Div => DIV
    case BinaryOp.Mod => MOD
    case BinaryOp.And => AND
    case BinaryOp.Or  => OR
    case BinaryOp.Eq  => EQ
    case BinaryOp.Lt  => LT
    case BinaryOp.Gt  => GT
    case _ => EQ // fallback for other ops (Ne, Le, Ge, Concat not in SAM)

  /** Convert operator character to BinaryOp. */
  def fromChar(ch: Char): Option[BinaryOp] = ch match
    case ADD => Some(BinaryOp.Add)
    case SUB => Some(BinaryOp.Sub)
    case MUL => Some(BinaryOp.Mul)
    case DIV => Some(BinaryOp.Div)
    case MOD => Some(BinaryOp.Mod)
    case AND => Some(BinaryOp.And)
    case OR  => Some(BinaryOp.Or)
    case EQ  => Some(BinaryOp.Eq)
    case LT  => Some(BinaryOp.Lt)
    case GT  => Some(BinaryOp.Gt)
    case _   => None

object OperatorUtils {
  import Operators._

  // Lazy-initialized instruction cache for better performance
  private lazy val unopInstructions: Map[Char, String] = Map(
    NEG -> CodeBuilder.buildString(_.pushImm(-1).mul()),
    NOT -> CodeBuilder.buildString(sb => sb.pushImm(1).add().pushImm(2).mod())
  )

  private lazy val binopInstructions: Map[Char, String] = Map(
    ADD -> CodeBuilder.buildString(_.add()),
    SUB -> CodeBuilder.buildString(_.sub()),
    MUL -> CodeBuilder.buildString(_.mul()),
    DIV -> CodeBuilder.buildString(_.div()),
    MOD -> CodeBuilder.buildString(_.mod()),
    AND -> CodeBuilder.buildString(_.andB()),
    OR  -> CodeBuilder.buildString(_.orB()),
    GT  -> CodeBuilder.buildString(_.greater()),
    LT  -> CodeBuilder.buildString(_.less()),
    EQ  -> CodeBuilder.buildString(_.equal())
  )

  /** Get unary operator code, returning Either for diagnostic-first flow. */
  def getUnopE(op: Char): Either[String, String] =
    unopInstructions.get(op).toRight(s"Invalid unary operator: $op")

  /** Get binary operator code, returning Either for diagnostic-first flow. */
  def getBinopE(op: Char): Either[String, String] =
    binopInstructions.get(op).toRight(s"Invalid binary operator: $op")

  /** Get binary operator category, returning Either for diagnostic-first flow. */
  def getBinopTypeE(op: Char): Either[String, BinopType] = op match
    case ADD | SUB | MUL | DIV | MOD => Right(BinopType.ARITHMETIC)
    case AND | OR => Right(BinopType.BITWISE)
    case GT | LT | EQ => Right(BinopType.COMPARISON)
    case other => Left(s"Invalid binary operator: $other")
}
