package assignment3.ast

import assignment3.{StringRuntime, Type}

/** Binary operation categories for clearer dispatch in code generation. */
enum BinaryOpCategory:
  case ShortCircuit, String, Numeric

/** ADT for string operations with type-safe emission. */
enum StringOperation:
  case Repeat(leftType: Type, rightType: Type)
  case Concat
  case Compare(op: Char)

  /** Emit the runtime code for this string operation. */
  def emitRuntime(): String = this match
    case Repeat(lt, rt) => StringRuntime.repeatString(lt, rt)
    case Concat => StringRuntime.concatString()
    case Compare(op) => StringRuntime.compareString(op)

object StringOperation:
  /** Determine the string operation from BinaryOp and types. */
  def from(op: BinaryOp, lt: Type, rt: Type): Option[StringOperation] = op match
    case BinaryOp.Mul if lt == Type.STRING || rt == Type.STRING => Some(Repeat(lt, rt))
    case BinaryOp.Add if lt == Type.STRING && rt == Type.STRING => Some(Concat)
    case BinaryOp.Lt  if lt == Type.STRING && rt == Type.STRING => Some(Compare('<'))
    case BinaryOp.Gt  if lt == Type.STRING && rt == Type.STRING => Some(Compare('>'))
    case BinaryOp.Eq  if lt == Type.STRING && rt == Type.STRING => Some(Compare('='))
    case _ => None

object BinaryOpUtils:
  /** Categorize a binary operation based on operator and operand types. */
  def categorize(op: BinaryOp, lt: Type, rt: Type): BinaryOpCategory =
    if (op == BinaryOp.And || op == BinaryOp.Or) && lt == Type.BOOL && rt == Type.BOOL then
      BinaryOpCategory.ShortCircuit
    else if lt == Type.STRING || rt == Type.STRING then
      BinaryOpCategory.String
    else
      BinaryOpCategory.Numeric
