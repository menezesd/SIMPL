package assignment3.ast

/** Unary operators for the idiomatic AST. */
enum UnaryOp:
  case Neg    // numeric negation: -x
  case Not    // boolean negation: !x

/** Binary operators for the idiomatic AST. */
enum BinaryOp:
  // arithmetic
  case Add, Sub, Mul, Div, Mod
  // boolean logic
  case And, Or
  // comparisons
  case Eq, Ne, Lt, Le, Gt, Ge
  // string concatenation (when semantically applicable)
  case Concat
