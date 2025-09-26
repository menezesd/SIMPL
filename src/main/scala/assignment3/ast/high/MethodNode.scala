package assignment3.ast.high

import assignment3.Type

sealed trait ReturnSig
object ReturnSig {
  case object Void extends ReturnSig
  final case class Prim(tpe: Type) extends ReturnSig
  final case class Obj(className: String) extends ReturnSig
}

final case class MethodNode(
  className: String,
  name: String,
  params: List[ParamNode],            // user params only (excludes implicit this)
  returnSig: ReturnSig,               // unified return signature
  // Method body may be either legacy BlockStmt or idiomatic Block during migration
  body: AnyRef
)
