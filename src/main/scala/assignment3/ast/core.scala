package assignment3.ast

import assignment3.Type

// Keep only the shared callable and variable-binding contracts used by Scala code.

// Callable abstraction used across codegen and parsing
trait CallableMethod {
  def getName: String
  def getReturnType: Type
  def getParamCount: Int
  def getParameterType(index: Int): Type
}

// Variable binding contract used by frames and variables
trait VarBinding {
  def getName: String
  def getAddress: Int
  def getType: Type
}
