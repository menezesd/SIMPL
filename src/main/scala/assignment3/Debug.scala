package assignment3

import scala.jdk.CollectionConverters._

/** Lightweight debug category helper (Scala port).
  * Enable with -Dliveoak.debug=<comma-separated categories> (symbols,tokens,sam,all)
  */
object Debug {
  private val CATEGORIES: Set[String] = {
    val prop = System.getProperty("liveoak.debug", "").trim
    if (prop.isEmpty) Set.empty
    else prop.split(',').iterator.map(_.trim).filter(_.nonEmpty).map(_.toLowerCase).toSet
  }

  def enabled(cat: String): Boolean = {
    if (CATEGORIES.isEmpty) false
    else CATEGORIES.contains("all") || CATEGORIES.contains(cat.toLowerCase)
  }

  def log(cat: String, msg: () => String): Unit = {
    if (enabled(cat)) {
      try System.err.println("[" + cat.toUpperCase + "] " + msg())
      catch { case _: Throwable => () /* logging must never throw */ }
    }
  }
}
