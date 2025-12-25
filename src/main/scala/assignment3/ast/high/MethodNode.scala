package assignment3.ast.high

import assignment3.Type
import assignment3.ast.Stmt

/** Return type signature for methods (Scala 3 enum). */
enum ReturnSig:
  case Void
  case Prim(tpe: Type)
  case Obj(className: String)

final case class MethodNode(
  className: String,
  name: String,
  params: List[ParamNode],            // user params only (excludes implicit this)
  returnSig: ReturnSig,               // unified return signature
  // Method body (idiomatic AST)
  body: Stmt
)
