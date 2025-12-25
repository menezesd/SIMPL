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

  // Codegen-specific messages
  object Codegen {
    val noFrameForVariable = "No frame in context for variable lookup"
    val noFrameForThis = "No frame in context for 'this'"
    val noFrameForAssignment = "No frame in context for assignment"
    val thisNotFound = "'this' not found in current frame"
    val returnLabelNotFound = "Return label not found in scope"
    val breakOutsideLoop = "'break' used outside of loop"

    def unknownField(field: String): String = s"Unknown field '$field'"

    // Binary operator errors
    val plusOnlyStringOrNumeric = "'+' only defined for String+String or numeric addition"
    val repeatRequiresStringInt = "Repeat requires (String*Int) or (Int*String)"
    val stringComparisonRequiresBothString = "String comparison requires both operands String"
    val unsupportedStringOperator = "Unsupported string operator"
    val concatInternalOnly = "'concat' is internal-only for strings"
    val leNotSupported = "'<=' not supported by runtime"
    val geNotSupported = "'>=' not supported by runtime"
    val neNotSupported = "'!=' not supported by runtime"
  }

  // Type-checking messages (AstEither)
  object TypeCheck {
    val logicalOpRequiresBool = "Logical op requires BOOL operands"
    val arithmeticOpRequiresInt = "Arithmetic op requires INT operands"
    val comparisonRequiresMatchingTypes = "Comparison requires matching types"
    val ternaryBranchTypeMismatch = "Ternary branch type mismatch"
    val negRequiresIntOrString = "'~' requires INT or STRING"
    val notRequiresBool = "'!' requires BOOL"
    def unsupportedOperator(op: Char): String = s"Unsupported operator: $op"
    def unsupportedUnaryOperator(op: Char): String = s"Unsupported unary operator: $op"
    val methodCallOnThisForbidden = "Method call on 'this' is not allowed"
    val inappropriateChaining = "Inappropriate method/field chaining"
    val unableToResolveClass = "Unable to resolve class for expression"
    def unknownClass(cn: String): String = s"Unknown class '$cn'"
    def unknownMethod(cn: String, mn: String): String = s"Unknown method '$mn' on class '$cn'"
    def unknownField(cn: String, fn: String): String = s"Unknown field '$fn' on class '$cn'"
  }

  // Semantic-specific messages
  object Semantic {
    val thisMethodCallNotAllowed = "Explicit 'this.method()' is not allowed in LO-3"
    val nullDerefInstanceCall = "Null dereference in instance call"
    val incorrectArgCount = "Incorrect number of arguments for instance method"
    val ifConditionMustBeBool = "If condition must be BOOL"
    val whileConditionMustBeBool = "While condition must be BOOL"
    val nonVoidMustReturnValue = "Non-void method must return a value"
    val voidShouldNotReturn = "Void method should not return a value"
    val returnTypeMismatch = "Return type mismatch"
    val returnObjectTypeMismatch = "Return object type mismatch"

    def nullDerefFieldAccess(field: String): String = s"Null dereference in field access '$field'"
    def nullDerefFieldAssign(field: String): String = s"Null dereference in field assignment '$field'"
    def argTypeMismatch(param: String): String = s"Argument type mismatch for parameter '$param'"
    def objAssignMismatch(name: String): String = s"Object assignment type mismatch for variable '$name'"
    def primAssignMismatch(name: String): String = s"Type mismatch in assignment to '$name'"
    def fieldObjAssignMismatch(field: String): String = s"Object assignment type mismatch for field '$field'"
    def fieldPrimAssignMismatch(field: String): String = s"Type mismatch assigning to field '$field'"
  }
}
