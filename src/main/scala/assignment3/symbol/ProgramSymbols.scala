package assignment3.symbol

import scala.collection.immutable.Map

final class ProgramSymbols {
  private var classes: Map[String, ClassSymbol] = Map.empty
  private var frozen = false

  def addClass(c: ClassSymbol): Unit = {
    if (frozen) throw new IllegalStateException(s"ProgramSymbols is frozen; cannot add class '${c.getName}'")
    if (classes.contains(c.getName)) throw new IllegalStateException(s"Duplicate class '${c.getName}'")
    classes = classes + (c.getName -> c)
  }

  def getClass(name: String): Option[ClassSymbol] = classes.get(name)
  def existsClass(name: String): Boolean = classes.contains(name)
  import scala.jdk.CollectionConverters._
  def allClasses: List[ClassSymbol] = classes.values.toList

  def freeze(): Unit = if (!frozen) { frozen = true; classes.values.foreach(_.freeze()) }
  def isFrozen: Boolean = frozen

  def getMethod(className: String, methodName: String): Option[MethodSymbol] =
    getClass(className).flatMap(_.method(methodName))

  def getEntrypoint(): Option[MethodSymbol] = getMethod("Main", "main")
}
