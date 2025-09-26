package assignment3

import assignment3.Offsets.StackOffset

import assignment3.{BinopType, OperatorUtils, SyntaxErrorException, Type}

/** Shared string runtime SAM snippets (Scala port). */
object StringRuntime {
  private val LENGTH_LABEL  = "STR_LENGTH"
  private val REVERSE_LABEL = "STR_REVERSE"
  private val CONCAT_LABEL  = "STR_CONCAT"
  private val APPEND_LABEL  = "STR_APPEND"
  private val COMPARE_LABEL = "STR_COMPARE"
  private val REPEAT_LABEL  = "STR_REPEAT"

  def repeatString(firstInputType: Type, secondInputType: Type): String = {
    val sb = new SamBuilder()
  if (firstInputType == Type.STRING) sb.swap()
    sb.linkCall(REPEAT_LABEL)
      .addSp(-1)
    sb.toString
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

  def getStringLength(): String = {
    val sb = new SamBuilder()
    sb.jsr(LENGTH_LABEL)
    sb.toString
  }

  /** Emit all shared string helper subroutines exactly once. */
  def emitAllStringFunctions(): String = {
    val sb = new SamBuilder()

    // STR_LENGTH
    val startCountLabel = new Label()
    val stopCountLabel  = new Label()
    sb.label(LENGTH_LABEL)
  sb.swap().dup()
    sb.label(startCountLabel.name)
  sb.dup().pushInd()
  sb.isNil().branchIfTruthy(stopCountLabel)
  sb.pushImmInt(1).add()
  sb.jump(startCountLabel.name)
    sb.label(stopCountLabel.name)
  sb.swap().sub().swap().rst()

    // STR_REVERSE
    val reverseStartLoopLabel = new Label()
    val reverseStopLoopLabel  = new Label()
    sb.label(REVERSE_LABEL)
  sb.pushImmInt(0).pushImmInt(0).pushImmInt(0)
  sb.pushOffS(StackOffset(-1))
    sb.append(getStringLength())
  sb.storeOffS(StackOffset(2))
  sb.pushOffS(StackOffset(2)).pushImmInt(1).add().malloc().storeOffS(StackOffset(3))
  sb.pushOffS(StackOffset(3)).storeOffS(StackOffset(4))
  sb.pushOffS(StackOffset(3)).pushOffS(StackOffset(2)).add().pushImmCh('\u0000').storeInd()
    sb.label(reverseStartLoopLabel.name)
  sb.pushOffS(StackOffset(2)).jumpIfNil(reverseStopLoopLabel)
  sb.pushOffS(StackOffset(3)).pushOffS(StackOffset(-1)).pushOffS(StackOffset(2)).add().pushImmInt(1).sub().pushInd().storeInd()
  sb.pushOffS(StackOffset(3)).pushImmInt(1).add().storeOffS(StackOffset(3))
  sb.pushOffS(StackOffset(2)).pushImmInt(1).sub().storeOffS(StackOffset(2))
  sb.jump(reverseStartLoopLabel.name)
    sb.label(reverseStopLoopLabel.name)
  sb.pushOffS(StackOffset(4)).storeOffS(StackOffset(-1)).addSp(-3).rst()

    // STR_CONCAT
    sb.label(CONCAT_LABEL)
  sb.pushImmInt(0).pushImmInt(0)
  sb.pushOffS(StackOffset(-1))
    sb.append(getStringLength())
  sb.pushOffS(StackOffset(-2))
    sb.append(getStringLength())
  sb.add().pushImmInt(1).add().malloc().storeOffS(StackOffset(2))
  sb.pushOffS(StackOffset(2)).storeOffS(StackOffset(3))
  sb.pushImmInt(0).pushOffS(StackOffset(2)).pushOffS(StackOffset(-2))
    sb.append(appendStringHeap())
  sb.storeOffS(StackOffset(2))
  sb.pushImmInt(0).pushOffS(StackOffset(2)).pushOffS(StackOffset(-1))
    sb.append(appendStringHeap())
  sb.storeOffS(StackOffset(2))
  sb.pushOffS(StackOffset(3)).storeOffS(StackOffset(-2)).addSp(-2).rst()

    // STR_APPEND
    val appendStartLoopLabel = new Label()
    val appendStopLoopLabel  = new Label()
    sb.label(APPEND_LABEL)
  sb.pushOffS(StackOffset(-2)).pushOffS(StackOffset(-1))
    sb.label(appendStartLoopLabel.name)
  sb.pushOffS(StackOffset(3)).pushInd().jumpIfNil(appendStopLoopLabel)
  sb.pushOffS(StackOffset(2)).pushOffS(StackOffset(3)).pushInd().storeInd()
  sb.pushOffS(StackOffset(2)).pushImmInt(1).add().storeOffS(StackOffset(2))
  sb.pushOffS(StackOffset(3)).pushImmInt(1).add().storeOffS(StackOffset(3))
  sb.jump(appendStartLoopLabel.name)
    sb.label(appendStopLoopLabel.name)
  sb.pushOffS(StackOffset(2)).pushImmCh('\u0000').storeInd().pushOffS(StackOffset(2)).storeOffS(StackOffset(-3)).addSp(-2).rst()

    // STR_REPEAT
    val repeatStartLoopLabel   = new Label()
    val repeatStopLoopLabel    = new Label()
    val repeatInvalidParamLabel= new Label()
    val repeatReturnLabel      = new Label()
    sb.label(REPEAT_LABEL)
  sb.pushImmInt(0).pushImmInt(0).pushImmInt(0)
  sb.pushOffS(StackOffset(-2)).isNeg().branchIfTruthy(repeatInvalidParamLabel.name)
  sb.pushOffS(StackOffset(-1))
    sb.append(getStringLength())
  sb.pushOffS(StackOffset(-2)).mul().pushImmInt(1).add().malloc().storeOffS(StackOffset(3))
  sb.pushOffS(StackOffset(3)).storeOffS(StackOffset(4))
    sb.label(repeatStartLoopLabel.name)
  sb.pushOffS(StackOffset(2)).pushOffS(StackOffset(-2)).equal().branchIfTruthy(repeatStopLoopLabel)
  sb.pushImmInt(0).pushOffS(StackOffset(3)).pushOffS(StackOffset(-1))
    sb.append(appendStringHeap())
  sb.storeOffS(StackOffset(3))
  sb.pushOffS(StackOffset(2)).pushImmInt(1).add().storeOffS(StackOffset(2))
  sb.jump(repeatStartLoopLabel.name)
    sb.label(repeatStopLoopLabel.name)
  sb.pushOffS(StackOffset(4)).storeOffS(StackOffset(-2)).jump(repeatReturnLabel.name)
    sb.label(repeatInvalidParamLabel.name)
  sb.pushImmStr("\"\"").storeOffS(StackOffset(-2))
    sb.label(repeatReturnLabel.name)
  sb.addSp(-3).rst()

    // STR_COMPARE
    val cmpStartLoopLabel = new Label()
    val cmpStopLoopLabel  = new Label()
    sb.label(COMPARE_LABEL)
  sb.pushImmInt(0).pushImmInt(0)
    sb.label(cmpStartLoopLabel.name)
  sb.pushOffS(StackOffset(-2)).pushOffS(StackOffset(2)).add().pushInd().isNil()
  sb.pushOffS(StackOffset(-1)).pushOffS(StackOffset(2)).add().pushInd().isNil().and()
  sb.branchIfTruthy(cmpStopLoopLabel.name)
  sb.pushOffS(StackOffset(-2)).pushOffS(StackOffset(2)).add().pushInd()
  sb.pushOffS(StackOffset(-1)).pushOffS(StackOffset(2)).add().pushInd().cmp().storeOffS(StackOffset(3))
  sb.pushOffS(StackOffset(3)).branchIfTruthy(cmpStopLoopLabel.name)
  sb.pushOffS(StackOffset(2)).pushImmInt(1).add().storeOffS(StackOffset(2))
  sb.jump(cmpStartLoopLabel.name)
    sb.label(cmpStopLoopLabel.name)
  sb.pushOffS(StackOffset(3)).storeOffS(StackOffset(-2)).addSp(-2).rst()

    sb.toString
  }

  /** Code-returning variant of emitAllStringFunctions (preferred for composition). */
  def emitAllStringFunctionsC(): Code = Code.fromString(emitAllStringFunctions())

  def reverseString(): String = {
    val sb = new SamBuilder()
  sb.linkCall(REVERSE_LABEL)
    sb.toString
  }

  def appendStringHeap(): String = {
    val sb = new SamBuilder()
  sb.linkCall(APPEND_LABEL).addSp(-2)
    sb.toString
  }

  def concatString(): String = {
    val sb = new SamBuilder()
  sb.linkCall(CONCAT_LABEL).addSp(-1)
    sb.toString
  }

  def compareString(op: Char): String = {
    if (OperatorUtils.getBinopType(op) != BinopType.COMPARISON)
      throw new SyntaxErrorException(s"compareString receive invalid operation: $op", -1)
    val sb = new SamBuilder()
  sb.linkCall(COMPARE_LABEL).addSp(-1)
    if (op == '<') sb.pushImmInt(1)
    else if (op == '>') sb.pushImmInt(-1)
    else sb.pushImmInt(0)
    sb.equal()
    sb.toString
  }
}
