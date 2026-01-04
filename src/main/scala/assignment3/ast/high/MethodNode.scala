package assignment3.ast.high

import assignment3.Type
import assignment3.ast.Stmt

/** Return type signature for methods (Scala 3 enum). */
enum ReturnSig:
  case Void
  case Prim(tpe: Type)
  case Obj(className: String)

object ReturnSig:
  /** Extension methods for ReturnSig. */
  extension (sig: ReturnSig)
    /** Check if return signature is void. */
    def isVoid: Boolean = sig match
      case Void => true
      case _ => false

    /** Check if return signature is a primitive type. */
    def isPrimitive: Boolean = sig match
      case Prim(_) => true
      case _ => false

    /** Check if return signature is an object type. */
    def isObject: Boolean = sig match
      case Obj(_) => true
      case _ => false

    /** Convert to Option[ValueType]. */
    def toValueTypeOpt: Option[assignment3.ValueType] =
      ReturnSigUtils.toValueTypeOpt(sig)

    /** Check if this return signature matches another. */
    def matches(other: ReturnSig): Boolean = (sig, other) match
      case (Void, Void) => true
      case (Prim(t1), Prim(t2)) => t1 == t2
      case (Obj(c1), Obj(c2)) => c1 == c2
      case _ => false

final case class MethodNode(
  className: String,
  name: String,
  params: List[ParamNode],            // user params only (excludes implicit this)
  returnSig: ReturnSig,               // unified return signature
  // Method body (idiomatic AST)
  body: Stmt
)
