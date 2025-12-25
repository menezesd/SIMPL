package assignment3.symbol

import scala.collection.immutable.Map

/** Immutable program symbol table. */
final case class ProgramSymbols(classes: Map[String, ClassSymbol]) {
  def getClass(name: String): Option[ClassSymbol] = classes.get(name)
  def existsClass(name: String): Boolean = classes.contains(name)
  def allClasses: List[ClassSymbol] = classes.values.toList

  def getMethod(className: String, methodName: String): Option[MethodSymbol] =
    getClass(className).flatMap(_.method(methodName))

  def getEntrypoint(): Option[MethodSymbol] = getMethod("Main", "main")
}

object ProgramSymbols {
  def empty(): ProgramSymbols = ProgramSymbols(Map.empty)

  @scala.annotation.varargs
  def withClassNames(names: String*): ProgramSymbols = {
    val classes = names.map { name =>
      val cls = ClassSymbol(name, Vector.empty[VarSymbol], Map.empty, Vector.empty)
      name -> cls
    }.toMap
    ProgramSymbols(classes)
  }
}
