
package assignment3

import java.util.UUID

/** Type-safe label for code generation. Opaque type wrapping String for zero-cost abstraction. */
opaque type Label = String

object Label:
  /** Create a new unique label with auto-generated UUID-based name. */
  def apply(): Label = generateUUID()

  /** Create a label with a specific name. */
  def apply(name: String): Label = name

  // Factory helpers for common patterns

  /** Create a pair of start/stop loop labels. */
  def loopPair(): (Label, Label) = (Label(), Label())

  /** Create a triple of labels for conditional branches (false, true, end). */
  def conditionalTriple(): (Label, Label, Label) = (Label(), Label(), Label())

  /** Create a pair of labels for if-then-else (else branch, end). */
  def ifElsePair(): (Label, Label) = (Label(), Label())

  /** Extension methods for Label. */
  extension (l: Label)
    /** Get the label name (String representation). */
    inline def name: String = l

    /** Java-style getter for compatibility. */
    inline def getName: String = l

  private[assignment3] def generateUUID(): String =
    val base = UUID.randomUUID().toString.substring(0, 8)
    if base.head.isLetter then base
    else
      val randomLetter = (('a' + util.Random.nextInt('z' - 'a' + 1)).toChar)
      s"$randomLetter${base.substring(1)}"
