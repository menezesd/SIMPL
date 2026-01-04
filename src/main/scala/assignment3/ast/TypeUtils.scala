package assignment3.ast

import assignment3.{Type, PrimitiveType, ObjectRefType}

/** Helpers for resolving simple Type and ValueType for idiomatic AST.
  *
  * Note: Methods returning `Type` use `Type.INT` as the lowered representation
  * for object types (heap pointers) since the underlying VM represents all
  * object references as integers. This is intentional for code generation.
  */
object IdiomaticTypeUtils:
  /** Extract object class name from a callable's return signature, if it returns an object. */
  private def returnClassName(m: CallableMethod): Option[String] = m match
    case sm: ScalaCallableMethod => sm.getReturnSig match
      case assignment3.ast.high.ReturnSig.Obj(cn) => Some(cn)
      case _ => None
    case _ => None

  // Lowered return type for expressions from a method/callable: object/void -> INT, primitive -> that type
  def loweredReturnOf(m: CallableMethod): Type = m match
    case sm: ScalaCallableMethod => sm.getReturnSig match
      case assignment3.ast.high.ReturnSig.Prim(t) => t
      case _ => Type.INT
    case _ => m.getReturnType

  def typeOf(e: Expr, method: assignment3.ast.MethodContext, programSymbols: assignment3.symbol.ProgramSymbols): Type = e match
    case IntLit(_, _)  => Type.INT
    case BoolLit(_, _) => Type.BOOL
    case StrLit(_, _)  => Type.STRING
    case NullLit(_)    => Type.INT
    case Var(name, _)  =>
      method match
        case nmc: NewMethodContext =>
          nmc.lookupVar(name).flatMap(_.primitiveOpt).getOrElse(Type.INT)
        case _ => Type.INT
    case Unary(UnaryOp.Not, _, _, _) => Type.BOOL
    case Unary(UnaryOp.Neg, inner, _, _) =>
      val t = typeOf(inner, method, programSymbols); if (t == Type.STRING) Type.STRING else Type.INT
    case Binary(_, _, _, rt, _) => rt.getOrElse(Type.INT)
    case Ternary(_, t, _, rt, _) => rt.getOrElse(typeOf(t, method, programSymbols))
    case This(_) => Type.INT
    case FieldAccess(_, _, fi, _) =>
      fi.map(_.valueType).collect { case PrimitiveType(t) => t }.getOrElse(Type.INT)
    case Call(m, _, _) => loweredReturnOf(m)
    case InstanceCall(_, m, _, _) => loweredReturnOf(m)
    case NewObject(_, _, _) => Type.INT

  def classNameOf(e: Expr, method: assignment3.ast.MethodContext, programSymbols: assignment3.symbol.ProgramSymbols): Option[String] = e match
    case NewObject(cn, _, _) => Some(cn)
    case FieldAccess(_, _, fi, _) =>
      fi.map(_.valueType).collect { case ObjectRefType(cn) => cn }
    case InstanceCall(_, m, _, _) => returnClassName(m)
    case Call(m, _, _) => returnClassName(m)
    case Var(name, _) =>
      method match
        case nmc: NewMethodContext =>
          nmc.lookupVar(name).flatMap(_.classTypeNameOpt)
        case _ => None
    case _ => None

  /** Unified field resolution: className -> classSymbol -> fieldInfo. */
  def resolveFieldInfo(
    target: Expr,
    fieldName: String,
    method: assignment3.ast.MethodContext,
    programSymbols: assignment3.symbol.ProgramSymbols
  ): Option[assignment3.symbol.ClassSymbol.FieldInfo] =
    for
      cn <- classNameOf(target, method, programSymbols)
      cs <- programSymbols.getClass(cn)
      fi <- cs.getFieldInfo(fieldName)
    yield fi

  /** Check if a block ends with a return statement. */
  def endsWithReturn(block: Block): Boolean =
    block.statements.lastOption.exists {
      case _: Return => true
      case _ => false
    }
