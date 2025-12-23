package assignment3.ast.high

import assignment3.{CompilerUtils, ValueType}

object ReturnSigUtils {
  def fromRawType(rawType: String, line: Int): ReturnSig =
    if ("void" == rawType) ReturnSig.Void
    else {
      val vt = CompilerUtils.parseTypeOrObjectName(rawType, line)
      if (vt.isObject) ReturnSig.Obj(vt.getObject.getClassName)
      else ReturnSig.Prim(vt.getPrimitive)
    }

  def toValueTypeOpt(sig: ReturnSig): Option[ValueType] = sig match {
    case ReturnSig.Void => None
    case ReturnSig.Obj(cn) => Some(ValueType.ofObject(cn))
    case ReturnSig.Prim(tpe) => Some(ValueType.ofPrimitive(tpe))
  }
}
