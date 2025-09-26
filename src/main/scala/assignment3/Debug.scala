
package assignment3

/** Lightweight debug category helper (Scala port, idiomatic).
  * Enable with -Dliveoak.debug=<comma-separated categories> (symbols,tokens,sam,all)
  */
object Debug {
  private val CATEGORIES: Set[String] = Option(System.getProperty("liveoak.debug")).map(_.trim)
    .filter(_.nonEmpty)
    .map(_.split(',').iterator.map(_.trim).filter(_.nonEmpty).map(_.toLowerCase).toSet)
    .getOrElse(Set.empty[String])

  def enabled(cat: String): Boolean =
    CATEGORIES.nonEmpty && (CATEGORIES.contains("all") || CATEGORIES.contains(cat.toLowerCase))

  def log(cat: String, msg: () => String): Unit =
    if (enabled(cat)) {
      try System.err.println(s"[${cat.toUpperCase}] ${msg()}")
      catch { case _: Throwable => () /* logging must never throw */ }
    }
}
