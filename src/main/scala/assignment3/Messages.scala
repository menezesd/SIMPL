package assignment3

object Messages {
  def classSymbolMissing(className: String): String = s"Class symbol missing for '$className'"
  def methodSymbolMissing(className: String, methodName: String): String = s"Method symbol missing for '$className.$methodName'"
  def duplicateClass(className: String): String = s"Duplicate class '$className' declared"
  def expectedCloseParenAfterFieldDecls: String = ") expected after field declarations"
  def expectedFieldListSemicolon: String = "Expected ';' in field list"
  def expectedFieldDeclSemicolon: String = "Expected ';' in field declaration"
  def expectedVarDeclSeparator: String = "Expected ',' or ';' in variable declaration"
  def expectedVarNameAfterType: String = "Expected variable name after type"
  def expectedStatement: String = "Expected statement"
  def expectedReturnType: String = "Expected return type"
  def invalidFieldType: String = "'void' is not a valid field type"
  def invalidLocalVariableType: String = "'void' is not a valid local variable type"
  def missingFinalReturn: String = "Method missing final return"
  def unbalancedBraces: String = "Unbalanced braces in method body"
  def mainNoParams: String = "Main.main method must not have parameters"
  def expectedClassHeaderOpenBrace(className: String): String = s"Expected '{' after class header for class '$className'"
  def missingEntrypoint(entryClass: String, entryMethod: String): String = s"Missing $entryClass.$entryMethod entry point"
  def entrypointNoParams(entryClass: String, entryMethod: String): String = s"$entryClass.$entryMethod must not declare parameters"
  def entrypointMustBeInstance(entryClass: String, entryMethod: String): String =
    s"$entryClass.$entryMethod must be an instance method of $entryClass"
  def tooManyParameters(methodName: String, expected: Int, atLeast: Int): String =
    s"Too many parameters in '$methodName': expected $expected, got at least $atLeast"
  def parameterMismatch(methodName: String, position: Int, expectedType: String, expectedName: String, actualType: String, actualName: String): String =
    s"Parameter mismatch in '$methodName' at position $position: expected $expectedType $expectedName, but found $actualType $actualName"
  def undeclaredVariable(name: String): String = s"Undeclared variable: $name"
  def unsupportedLhs: String = "Unsupported LHS in assignment"
  def unknownFieldType(className: String, fieldName: String, typeName: String): String =
    s"Unknown type '$typeName' for field '$fieldName' in class '$className'"
  def unknownParamType(className: String, methodName: String, paramName: String, typeName: String): String =
    s"Unknown parameter type '$typeName' for parameter '$paramName' in method '$className.$methodName'"
  def unknownLocalType(className: String, methodName: String, localName: String, typeName: String): String =
    s"Unknown local type '$typeName' for variable '$localName' in method '$className.$methodName'"
  def unknownReturnType(className: String, methodName: String, typeName: String): String =
    s"Unknown return type '$typeName' in method '$className.$methodName'"
  def cannotDetermineTargetClass: String = "Cannot determine target class for instance call"
  def undeclaredSymbol(name: String): String = s"Undeclared symbol: $name"
  def unexpectedTokenInExpression: String = "Unexpected token in expression"
}
