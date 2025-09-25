package assignment3.ast.high

import assignment3.Type

final case class MethodNode(
  className: String,
  name: String,
  params: List[ParamNode],            // user params only (excludes implicit this)
  returnObject: String,               // null if primitive or void
  returnPrimitive: Type,              // null if object or void
  isVoid: Boolean,
  // Method body may be either legacy BlockStmt or idiomatic Block during migration
  body: AnyRef
)
