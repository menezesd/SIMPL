package assignment3.ast

import assignment3.Type

/** Helpers for resolving simple Type and ValueType for idiomatic AST. */
object IdiomaticTypeUtils:
  def typeOf(e: Expr, method: assignment3.ast.MethodContext, programSymbols: assignment3.symbol.ProgramSymbols): Type = e match
    case IntLit(_, _)  => Type.INT
    case BoolLit(_, _) => Type.BOOL
    case StrLit(_, _)  => Type.STRING
    case NullLit(_)    => Type.INT
    case Var(name, _)  =>
      method match
        case nmc: NewMethodContext =>
          nmc.lookupVar(name).map(_.getType).getOrElse(Type.INT)
        case _ => Type.INT
    case Unary(UnaryOp.Not, _, _, _) => Type.BOOL
    case Unary(UnaryOp.Neg, inner, _, _) =>
      val t = typeOf(inner, method, programSymbols); if (t == Type.STRING) Type.STRING else Type.INT
    case Binary(_, _, _, rt, _) => rt.getOrElse(Type.INT)
    case Ternary(_, t, _, rt, _) => rt.getOrElse(typeOf(t, method, programSymbols))
    case This(_) => Type.INT
    case FieldAccess(_, _, fi, _) => fi.filter(_.valueType.isPrimitive).map(_.valueType.getPrimitive).getOrElse(Type.INT)
    case Call(m, _, _) =>
      m match
        case sm: assignment3.ast.ScalaCallableMethod =>
          sm.getReturnSig match
            case assignment3.ast.high.ReturnSig.Prim(t) => t
            case _ => Type.INT
        case _ => m.getReturnType
    case InstanceCall(_, m, _, _) =>
      m match
        case sm: assignment3.ast.ScalaCallableMethod =>
          sm.getReturnSig match
            case assignment3.ast.high.ReturnSig.Prim(t) => t
            case _ => Type.INT
        case _ => m.getReturnType
    case NewObject(_, _, _) => Type.INT

  def classNameOf(e: Expr, method: assignment3.ast.MethodContext, programSymbols: assignment3.symbol.ProgramSymbols): Option[String] = e match
    case NewObject(cn, _, _) => Some(cn)
    case FieldAccess(_, _, fi, _) => fi.filter(_.valueType.isObject).map(_.valueType.getObject.getClassName)
    case InstanceCall(_, m, _, _) =>
      m match
        case sm: assignment3.ast.ScalaCallableMethod =>
          sm.getReturnSig match
            case assignment3.ast.high.ReturnSig.Obj(cn) => Some(cn)
            case _ => None
        case _ => None
    case Call(m, _, _) =>
      m match
        case sm: assignment3.ast.ScalaCallableMethod =>
          sm.getReturnSig match
            case assignment3.ast.high.ReturnSig.Obj(cn) => Some(cn)
            case _ => None
        case _ => None
    case Var(name, _) =>
      method match
        case nmc: NewMethodContext =>
          nmc.lookupVar(name).filter(_.isObject).map(_.getClassTypeName)
        case _ => None
    case _ => None
