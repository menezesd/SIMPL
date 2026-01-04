package assignment3

import assignment3.LabelTypes.RuntimeLabel
import assignment3.Offsets.StackOffset

/** Shared string runtime SAM snippets (Scala port). */
object StringRuntime {
  // Use centralized labels from LabelTypes
  private val LENGTH_LABEL  = RuntimeLabel.StringLength.value
  private val REVERSE_LABEL = RuntimeLabel.StringReverse.value
  private val CONCAT_LABEL  = RuntimeLabel.StringConcat.value
  private val APPEND_LABEL  = RuntimeLabel.StringAppend.value
  private val COMPARE_LABEL = RuntimeLabel.StringCompare.value
  private val REPEAT_LABEL  = RuntimeLabel.StringRepeat.value

  // Named constants for stack offsets used in string operations
  // These represent positions relative to the current stack frame
  private object StackSlots {
    val ReturnAddress = StackOffset(-1)   // Caller's return address / first arg
    val SecondArg     = StackOffset(-2)   // Second argument from caller
    val ThirdArg      = StackOffset(-3)   // Third argument (for append)
    val Local1        = StackOffset(2)    // First local variable
    val Local2        = StackOffset(3)    // Second local variable
    val Local3        = StackOffset(4)    // Third local variable
  }

  // Stack cleanup constants (argument count to pop after runtime calls)
  private object ArgCleanup {
    val RepeatArgs  = -1  // REPEAT takes 2 args, leaves 1 result (net: cleanup 1)
    val ConcatArgs  = -1  // CONCAT takes 2 args, leaves 1 result (net: cleanup 1)
    val AppendArgs  = -2  // APPEND takes 3 args, leaves 1 result (net: cleanup 2)
    val CompareArgs = -1  // COMPARE takes 2 args, leaves 1 result (net: cleanup 1)
    val AppendCall  = -2  // Internal APPEND call cleanup
    val ThreeArgs   = -3  // Full 3-argument cleanup for complex operations
    val TwoArgs     = -2  // Full 2-argument cleanup
  }

  // String operation constants
  private object StringConstants:
    val NullTerminator = '\u0000'
    val MinimumBufferSize = 1  // Space for null terminator
    val InvalidRepeatDefault = "\"\""  // Empty string for negative repeat count

  def repeatString(firstInputType: Type, secondInputType: Type): String =
    CodeBuilder.buildString { sb =>
      if (firstInputType == Type.STRING) sb.swap()
      sb.linkCall(REPEAT_LABEL).addSp(ArgCleanup.RepeatArgs)
    }

  // Java-friendly wrappers: avoid referencing Scala enum constants in Java tests
  def repeatStringSI(): String = repeatString(Type.STRING, Type.INT) // first is String, second is Int
  def repeatStringIS(): String = repeatString(Type.INT, Type.STRING) // first is Int, second is String

  // Code-returning convenience wrappers (optional)
  def repeatStringC(firstInputType: Type, secondInputType: Type): Code = Code.fromString(repeatString(firstInputType, secondInputType))
  def getStringLengthC(): Code = Code.fromString(getStringLength())
  def reverseStringC(): Code = Code.fromString(reverseString())
  def appendStringHeapC(): Code = Code.fromString(appendStringHeap())
  def concatStringC(): Code = Code.fromString(concatString())
  def compareStringC(op: Char): Code = Code.fromString(compareString(op))

  def getStringLength(): String =
    CodeBuilder.buildString(_.jsr(LENGTH_LABEL))

  // --- Individual runtime function emitters ---

  /** Emit STR_LENGTH subroutine: counts characters until null terminator */
  private def emitLengthFunction(sb: SamBuilder): Unit =
    val startCountLabel = Label()
    val stopCountLabel = Label()
    sb.label(LENGTH_LABEL)
    sb.swap().dup()
    sb.label(startCountLabel.name)
    sb.dup().pushInd()
    sb.isNil().branchIfTruthy(stopCountLabel)
    sb.pushImmInt(1).add()
    sb.jump(startCountLabel.name)
    sb.label(stopCountLabel.name)
    sb.swap().sub().swap().rst()

  /** Emit STR_REVERSE subroutine: creates reversed copy of string */
  private def emitReverseFunction(sb: SamBuilder): Unit =
    import StackSlots._
    val reverseStartLoopLabel = Label()
    val reverseStopLoopLabel = Label()
    sb.label(REVERSE_LABEL)
    sb.pushImmInt(0).pushImmInt(0).pushImmInt(0)
    sb.pushOffS(ReturnAddress)
    sb.append(getStringLength())
    sb.storeOffS(Local1)
    sb.pushOffS(Local1).pushImmInt(1).add().malloc().storeOffS(Local2)
    sb.pushOffS(Local2).storeOffS(Local3)
    sb.pushOffS(Local2).pushOffS(Local1).add().pushImmCh('\u0000').storeInd()
    sb.label(reverseStartLoopLabel.name)
    sb.pushOffS(Local1).jumpIfNil(reverseStopLoopLabel)
    sb.pushOffS(Local2).pushOffS(ReturnAddress).pushOffS(Local1).add().pushImmInt(1).sub().pushInd().storeInd()
    sb.pushOffS(Local2).pushImmInt(1).add().storeOffS(Local2)
    sb.pushOffS(Local1).pushImmInt(1).sub().storeOffS(Local1)
    sb.jump(reverseStartLoopLabel.name)
    sb.label(reverseStopLoopLabel.name)
    sb.pushOffS(Local3).storeOffS(ReturnAddress).addSp(ArgCleanup.ThreeArgs).rst()

  /** Emit STR_CONCAT subroutine: concatenates two strings */
  private def emitConcatFunction(sb: SamBuilder): Unit =
    import StackSlots._
    sb.label(CONCAT_LABEL)
    sb.pushImmInt(0).pushImmInt(0)
    sb.pushOffS(ReturnAddress)
    sb.append(getStringLength())
    sb.pushOffS(SecondArg)
    sb.append(getStringLength())
    sb.add().pushImmInt(1).add().malloc().storeOffS(Local1)
    sb.pushOffS(Local1).storeOffS(Local2)
    sb.pushImmInt(0).pushOffS(Local1).pushOffS(SecondArg)
    sb.append(appendStringHeap())
    sb.storeOffS(Local1)
    sb.pushImmInt(0).pushOffS(Local1).pushOffS(ReturnAddress)
    sb.append(appendStringHeap())
    sb.storeOffS(Local1)
    sb.pushOffS(Local2).storeOffS(SecondArg).addSp(ArgCleanup.TwoArgs).rst()

  /** Emit STR_APPEND subroutine: appends source string to destination buffer */
  private def emitAppendFunction(sb: SamBuilder): Unit =
    import StackSlots._
    val appendStartLoopLabel = Label()
    val appendStopLoopLabel = Label()
    sb.label(APPEND_LABEL)
    sb.pushOffS(SecondArg).pushOffS(ReturnAddress)
    sb.label(appendStartLoopLabel.name)
    sb.pushOffS(Local2).pushInd().jumpIfNil(appendStopLoopLabel)
    sb.pushOffS(Local1).pushOffS(Local2).pushInd().storeInd()
    sb.pushOffS(Local1).pushImmInt(1).add().storeOffS(Local1)
    sb.pushOffS(Local2).pushImmInt(1).add().storeOffS(Local2)
    sb.jump(appendStartLoopLabel.name)
    sb.label(appendStopLoopLabel.name)
    sb.pushOffS(Local1).pushImmCh('\u0000').storeInd().pushOffS(Local1).storeOffS(ThirdArg).addSp(ArgCleanup.TwoArgs).rst()

  /** Emit STR_REPEAT subroutine: repeats string N times */
  private def emitRepeatFunction(sb: SamBuilder): Unit =
    import StackSlots._
    val repeatStartLoopLabel = Label()
    val repeatStopLoopLabel = Label()
    val repeatInvalidParamLabel = Label()
    val repeatReturnLabel = Label()
    sb.label(REPEAT_LABEL)
    sb.pushImmInt(0).pushImmInt(0).pushImmInt(0)
    sb.pushOffS(SecondArg).isNeg().branchIfTruthy(repeatInvalidParamLabel.name)
    sb.pushOffS(ReturnAddress)
    sb.append(getStringLength())
    sb.pushOffS(SecondArg).mul().pushImmInt(1).add().malloc().storeOffS(Local2)
    sb.pushOffS(Local2).storeOffS(Local3)
    sb.label(repeatStartLoopLabel.name)
    sb.pushOffS(Local1).pushOffS(SecondArg).equal().branchIfTruthy(repeatStopLoopLabel)
    sb.pushImmInt(0).pushOffS(Local2).pushOffS(ReturnAddress)
    sb.append(appendStringHeap())
    sb.storeOffS(Local2)
    sb.pushOffS(Local1).pushImmInt(1).add().storeOffS(Local1)
    sb.jump(repeatStartLoopLabel.name)
    sb.label(repeatStopLoopLabel.name)
    sb.pushOffS(Local3).storeOffS(SecondArg).jump(repeatReturnLabel.name)
    sb.label(repeatInvalidParamLabel.name)
    sb.pushImmStr("\"\"").storeOffS(SecondArg)
    sb.label(repeatReturnLabel.name)
    sb.addSp(ArgCleanup.ThreeArgs).rst()

  /** Emit STR_COMPARE subroutine: lexicographic comparison */
  private def emitCompareFunction(sb: SamBuilder): Unit =
    import StackSlots._
    val cmpStartLoopLabel = Label()
    val cmpStopLoopLabel = Label()
    sb.label(COMPARE_LABEL)
    sb.pushImmInt(0).pushImmInt(0)
    sb.label(cmpStartLoopLabel.name)
    sb.pushOffS(SecondArg).pushOffS(Local1).add().pushInd().isNil()
    sb.pushOffS(ReturnAddress).pushOffS(Local1).add().pushInd().isNil().and()
    sb.branchIfTruthy(cmpStopLoopLabel.name)
    sb.pushOffS(SecondArg).pushOffS(Local1).add().pushInd()
    sb.pushOffS(ReturnAddress).pushOffS(Local1).add().pushInd().cmp().storeOffS(Local2)
    sb.pushOffS(Local2).branchIfTruthy(cmpStopLoopLabel.name)
    sb.pushOffS(Local1).pushImmInt(1).add().storeOffS(Local1)
    sb.jump(cmpStartLoopLabel.name)
    sb.label(cmpStopLoopLabel.name)
    sb.pushOffS(Local2).storeOffS(SecondArg).addSp(ArgCleanup.TwoArgs).rst()

  /** Emit all shared string helper subroutines exactly once. */
  def emitAllStringFunctions(): String =
    val sb = new SamBuilder()
    emitLengthFunction(sb)
    emitReverseFunction(sb)
    emitConcatFunction(sb)
    emitAppendFunction(sb)
    emitRepeatFunction(sb)
    emitCompareFunction(sb)
    sb.toString

  /** Code-returning variant of emitAllStringFunctions (preferred for composition). */
  def emitAllStringFunctionsC(): Code = Code.fromString(emitAllStringFunctions())

  def reverseString(): String =
    CodeBuilder.buildString(_.linkCall(REVERSE_LABEL))

  def appendStringHeap(): String =
    CodeBuilder.buildString(_.linkCall(APPEND_LABEL).addSp(ArgCleanup.AppendCall))

  def concatString(): String =
    CodeBuilder.buildString(_.linkCall(CONCAT_LABEL).addSp(ArgCleanup.ConcatArgs))

  def compareString(op: Char): String =
    // Maintain signature for existing Java tests, but avoid throwing.
    // If an invalid operator is passed, emit code that leaves 'false' on stack.
    val isComparison = OperatorUtils.getBinopTypeE(op).exists(_ == BinopType.COMPARISON)
    CodeBuilder.buildString { sb =>
      sb.linkCall(COMPARE_LABEL).addSp(ArgCleanup.CompareArgs)
      op match
        case '<' if isComparison => sb.pushImmInt(1)
        case '>' if isComparison => sb.pushImmInt(-1)
        case '=' if isComparison => sb.pushImmInt(0)
        case _ => sb.pushImmInt(0) // invalid op => always false
      sb.equal()
    }
}
