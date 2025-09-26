package assignment3

import edu.utexas.cs.sam.io.SamTokenizer

/** Utilities for interop with SamTokenizer without JVM reflection.
  * Column information isn't exposed via a stable public API in the SaM jar,
  * so we return -1 to indicate "unknown". Error messages still include line numbers.
  */
object TokenizerOps {
  /** Best-effort column extraction; returns -1 (unknown). */
  def column(tz: SamTokenizer): Int = -1
}
