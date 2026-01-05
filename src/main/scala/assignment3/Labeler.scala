package assignment3

/** Type-safe label wrappers for compile-time safety. */
object LabelTypes:
  /** Label for a method (class_method format). */
  opaque type MethodLabel = String
  object MethodLabel:
    def apply(className: String, methodName: String): MethodLabel = s"${className}_${methodName}"
    extension (l: MethodLabel) def value: String = l

  /** Label for runtime string functions. */
  opaque type RuntimeLabel = String
  object RuntimeLabel:
    def apply(name: String): RuntimeLabel = name
    extension (l: RuntimeLabel) def value: String = l

    // Pre-defined runtime labels
    val StringLength: RuntimeLabel = "STR_LENGTH"
    val StringReverse: RuntimeLabel = "STR_REVERSE"
    val StringConcat: RuntimeLabel = "STR_CONCAT"
    val StringAppend: RuntimeLabel = "STR_APPEND"
    val StringCompare: RuntimeLabel = "STR_COMPARE"
    val StringRepeat: RuntimeLabel = "STR_REPEAT"

/** Stateless label utilities. Previously a class, now an object since no state is needed. */
object Labeler:
  import LabelTypes._

  def methodLabel(className: String, methodName: String): String = MethodLabel(className, methodName).value
