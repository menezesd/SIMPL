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
          val vs = nmc.getSymbol.lookup(name)
          if (vs != null) vs.getType else Type.INT
        case _ => Type.INT
    case Unary(UnaryOp.Not, _, _, _) => Type.BOOL
    case Unary(UnaryOp.Neg, inner, _, _) =>
      val t = typeOf(inner, method, programSymbols); if (t == Type.STRING) Type.STRING else Type.INT
    case Binary(_, _, _, rt, _) => rt.getOrElse(Type.INT)
    case Ternary(_, t, _, rt, _) => rt.getOrElse(typeOf(t, method, programSymbols))
    case This(_) => Type.INT
    case FieldAccess(_, _, fi, _) => if fi != null && fi.valueType != null && fi.valueType.isPrimitive then fi.valueType.getPrimitive else Type.INT
    case Call(m, _, _) => m.getReturnType
    case InstanceCall(_, m, _, _) => m.getReturnType
    case NewObject(_, _, _) => Type.INT

  def classNameOf(e: Expr, method: assignment3.ast.MethodContext, programSymbols: assignment3.symbol.ProgramSymbols): String = e match
    case NewObject(cn, _, _) => cn
    case FieldAccess(_, _, fi, _) => if fi != null && fi.valueType != null && fi.valueType.isObject then fi.valueType.getObject.getClassName else null
    case InstanceCall(_, m, _, _) =>
      m match
        case sm: assignment3.ast.ScalaCallableMethod =>
          val rv = sm.getReturnValueType
          if rv != null && rv.isObject then rv.getObject.getClassName else null
        case _ => null
    case Call(m, _, _) =>
      m match
        case sm: assignment3.ast.ScalaCallableMethod =>
          val rv = sm.getReturnValueType
          if rv != null && rv.isObject then rv.getObject.getClassName else null
        case _ => null
    case Var(name, _) =>
      method match
        case nmc: NewMethodContext =>
          val vs = nmc.getSymbol.lookup(name)
          if (vs != null && vs.isObject) vs.getClassTypeName else null
        case _ => null
    case _ => null
