package assignment3.symbol

import assignment3.ValueType

// Immutable symbol model used by aggressive migration
final case class IProgramSymbols(classes: Map[String, IClassSymbol]) {
  def getClass(name: String): Option[IClassSymbol] = classes.get(name)
  def existsClass(name: String): Boolean = classes.contains(name)
  def allClasses: List[IClassSymbol] = classes.values.toList
  def getMethod(className: String, methodName: String): Option[IMethodSymbol] =
    getClass(className).flatMap(_.method(methodName))
  def getEntrypoint(): Option[IMethodSymbol] = getMethod("Main", "main")
}

final case class IClassSymbol(
  name: String,
  fields: Vector[VarSymbol],
  methods: Map[String, IMethodSymbol],
  fieldOrder: Vector[String]
) {
  def getName: String = name
  def field(n: String): Option[VarSymbol] = fields.find(_.getName == n)
  def method(n: String): Option[IMethodSymbol] = methods.get(n)
  def allFields: List[VarSymbol] = fields.toList
  def allMethods: List[IMethodSymbol] = methods.values.toList
  def fieldOffset(fieldName: String): Int = fieldOrder.indexOf(fieldName)
  def numFields(): Int = fieldOrder.size
}

final case class IMethodSymbol(
  name: String,
  parameters: Vector[VarSymbol],
  locals: Vector[VarSymbol],
  returnValueTypeOpt: Option[ValueType],
  bodyStartLine: Int,
  returnTypeLine: Int,
  returnTypeColumn: Int
) {
  def getName: String = name
  def parametersList: List[VarSymbol] = parameters.toList
  def localsList: List[VarSymbol] = locals.toList
  def lookup(ident: String): Option[VarSymbol] = (parameters ++ locals).find(_.getName == ident)
  def numParameters(): Int = parameters.size
  def numLocals(): Int = locals.size
  def expectedUserArgs(): Int = math.max(0, numParameters() - 1)
  def returnAddressOffset(): Int = -(1 + numParameters())
  def getReturnSig: assignment3.ast.high.ReturnSig = returnValueTypeOpt match {
    case None => assignment3.ast.high.ReturnSig.Void
    case Some(vt) if vt.isObject => assignment3.ast.high.ReturnSig.Obj(vt.getObject.getClassName)
    case Some(vt) => assignment3.ast.high.ReturnSig.Prim(vt.getPrimitive)
  }
  def getReturnTypeLine(): Int = returnTypeLine
  def getReturnTypeColumn(): Int = returnTypeColumn
}
