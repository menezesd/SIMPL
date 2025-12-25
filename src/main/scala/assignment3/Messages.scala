package assignment3

/** Centralized error message definitions. */
object Messages {
  // Syntax errors - parsing
  val expectedCloseParenAfterFieldDecls = ") expected after field declarations"
  val expectedFieldListSemicolon = "Expected ';' in field list"
  val expectedFieldDeclSemicolon = "Expected ';' in field declaration"
  val expectedVarDeclSeparator = "Expected ',' or ';' in variable declaration"
  val expectedVarNameAfterType = "Expected variable name after type"
  val expectedStatement = "Expected statement"
  val expectedReturnType = "Expected return type"
  val unbalancedBraces = "Unbalanced braces in method body"
  val unsupportedLhs = "Unsupported LHS in assignment"
  val unexpectedTokenInExpression = "Unexpected token in expression"
  val mainNoParams = "Main.main method must not have parameters"

  // Type errors
  val invalidFieldType = "'void' is not a valid field type"
  val invalidLocalVariableType = "'void' is not a valid local variable type"
  val missingFinalReturn = "Method missing final return"
  val cannotDetermineTargetClass = "Cannot determine target class for instance call"

  // Symbol resolution errors
  def classSymbolMissing(className: String): String =
    s"Class symbol missing for '$className'"

  def methodSymbolMissing(className: String, methodName: String): String =
    s"Method symbol missing for '$className.$methodName'"

  def duplicateClass(className: String): String =
    s"Duplicate class '$className' declared"

  def undeclaredVariable(name: String): String =
    s"Undeclared variable: $name"

  def undeclaredSymbol(name: String): String =
    s"Undeclared symbol: $name"

  // Class/method structure errors
  def expectedClassHeaderOpenBrace(className: String): String =
    s"Expected '{' after class header for class '$className'"

  def missingEntrypoint(entryClass: String, entryMethod: String): String =
    s"Missing $entryClass.$entryMethod entry point"

  def entrypointNoParams(entryClass: String, entryMethod: String): String =
    s"$entryClass.$entryMethod must not declare parameters"

  def entrypointMustBeInstance(entryClass: String, entryMethod: String): String =
    s"$entryClass.$entryMethod must be an instance method of $entryClass"

  // Parameter/type mismatch errors
  def tooManyParameters(methodName: String, expected: Int, atLeast: Int): String =
    s"Too many parameters in '$methodName': expected $expected, got at least $atLeast"

  def parameterMismatch(methodName: String, position: Int, expectedType: String, expectedName: String, actualType: String, actualName: String): String =
    s"Parameter mismatch in '$methodName' at position $position: expected $expectedType $expectedName, but found $actualType $actualName"

  // Unknown type errors
  def unknownFieldType(className: String, fieldName: String, typeName: String): String =
    s"Unknown type '$typeName' for field '$fieldName' in class '$className'"

  def unknownParamType(className: String, methodName: String, paramName: String, typeName: String): String =
    s"Unknown parameter type '$typeName' for parameter '$paramName' in method '$className.$methodName'"

  def unknownLocalType(className: String, methodName: String, localName: String, typeName: String): String =
    s"Unknown local type '$typeName' for variable '$localName' in method '$className.$methodName'"

  def unknownReturnType(className: String, methodName: String, typeName: String): String =
    s"Unknown return type '$typeName' in method '$className.$methodName'"
}
