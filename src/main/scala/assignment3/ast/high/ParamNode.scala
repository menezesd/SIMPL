package assignment3.ast.high

import assignment3.Type

// Use Options instead of nulls for object/primitive type information
final case class ParamNode(
	name: String,
	objectType: Option[String],
	primitiveType: Option[Type]
)
