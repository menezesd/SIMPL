package assignment3.ast

import assignment3.{Type, ValueType}
import assignment3.symbol.ClassSymbol

// Idiomatic, sealed AST to enable pattern matching and immutable trees.

// Expressions
sealed trait Expr:
  def pos: Int // source position (line), 0 if unknown

final case class IntLit(value: Int, pos: Int = 0) extends Expr
final case class BoolLit(value: Boolean, pos: Int = 0) extends Expr
final case class StrLit(value: String, pos: Int = 0) extends Expr
final case class NullLit(pos: Int = 0) extends Expr

final case class Var(name: String, pos: Int = 0) extends Expr
final case class Unary(op: UnaryOp, expr: Expr, resultType: Option[Type] = None, pos: Int = 0) extends Expr
final case class Binary(op: BinaryOp, left: Expr, right: Expr, resultType: Option[Type] = None, pos: Int = 0) extends Expr
final case class Ternary(cond: Expr, thenExpr: Expr, elseExpr: Expr, resultType: Option[Type] = None, pos: Int = 0) extends Expr

// Function/Method calls
final case class Call(method: CallableMethod, args: List[Expr], pos: Int = 0) extends Expr
final case class This(pos: Int = 0) extends Expr
final case class NewObject(className: String, args: List[Expr], pos: Int = 0) extends Expr
final case class InstanceCall(target: Expr, method: CallableMethod, args: List[Expr], pos: Int = 0) extends Expr
final case class FieldAccess(target: Expr, field: String, fieldInfo: ClassSymbol.FieldInfo | Null = null, pos: Int = 0) extends Expr

// Statements
sealed trait Stmt:
  def pos: Int

final case class Assign(name: String, value: Expr, pos: Int = 0) extends Stmt
final case class FieldAssign(target: Expr, field: String, offset: Int, value: Expr, pos: Int = 0) extends Stmt
final case class VarDecl(name: String, varType: Option[Type], valueType: Option[ValueType], init: Option[Expr], pos: Int = 0) extends Stmt
final case class Return(value: Option[Expr], pos: Int = 0) extends Stmt
final case class If(cond: Expr, thenBranch: Stmt, elseBranch: Stmt, pos: Int = 0) extends Stmt
final case class While(cond: Expr, body: Stmt, pos: Int = 0) extends Stmt
final case class Block(statements: List[Stmt], pos: Int = 0) extends Stmt
final case class Break(pos: Int = 0) extends Stmt
