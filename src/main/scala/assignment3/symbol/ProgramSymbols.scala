package assignment3.symbol

import assignment3.CompilerException
import scala.collection.mutable

final class ProgramSymbols {
  private val classes = mutable.LinkedHashMap.empty[String, ClassSymbol]
  private var frozen = false

  def addClass(c: ClassSymbol): Unit = {
    if (frozen) throw new CompilerException(s"ProgramSymbols is frozen; cannot add class '${c.getName}'", -1)
    if (classes.contains(c.getName)) throw new CompilerException(s"Duplicate class '${c.getName}'", -1)
    classes += c.getName -> c
  }

  def getClass(name: String): Option[ClassSymbol] = classes.get(name)
  def existsClass(name: String): Boolean = classes.contains(name)
  import scala.jdk.CollectionConverters._
  def allClasses(): java.lang.Iterable[ClassSymbol] = java.util.Collections.unmodifiableCollection(classes.values.toList.asJava)

  def freeze(): Unit = if (!frozen) { frozen = true; classes.values.foreach(_.freeze()) }
  def isFrozen: Boolean = frozen

  def getMethod(className: String, methodName: String): Option[MethodSymbol] = {
    getClass(className).flatMap(_.getMethod(methodName))
  }

  def getEntrypoint(): Option[MethodSymbol] = getMethod("Main", "main")
}
