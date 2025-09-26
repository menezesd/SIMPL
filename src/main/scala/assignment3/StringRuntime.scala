package assignment3

import assignment3.{BinopType, CompilerException, OperatorUtils, SyntaxErrorException, Type}

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
    if (firstInputType == Type.STRING) sb.append("SWAP\n")
    sb.append("LINK\n")
      .append(s"JSR $REPEAT_LABEL\n")
      .append("UNLINK\n")
      .append("ADDSP -1\n")
    sb.toString
  }

  def getStringLength(): String = {
    val sb = new SamBuilder()
    sb.append(s"JSR $LENGTH_LABEL\n")
    sb.toString
  }

  /** Emit all shared string helper subroutines exactly once. */
  def emitAllStringFunctions(): String = {
    val sb = new SamBuilder()

    // STR_LENGTH
    val startCountLabel = new Label()
    val stopCountLabel  = new Label()
    sb.label(LENGTH_LABEL)
    sb.append("SWAP\nDUP\n")
    sb.label(startCountLabel.name)
    sb.append("DUP\nPUSHIND\n")
    sb.append("ISNIL\n")
    sb.append(s"JUMPC ${stopCountLabel.name}\n")
    sb.append("PUSHIMM 1\nADD\n")
    sb.append(s"JUMP ${startCountLabel.name}\n")
    sb.label(stopCountLabel.name)
    sb.append("SWAP\nSUB\nSWAP\nRST\n")

    // STR_REVERSE
    val reverseStartLoopLabel = new Label()
    val reverseStopLoopLabel  = new Label()
    sb.label(REVERSE_LABEL)
    sb.append("PUSHIMM 0\nPUSHIMM 0\nPUSHIMM 0\n")
    sb.append("PUSHOFF -1\n")
    sb.append(getStringLength())
    sb.append("STOREOFF 2\n")
    sb.append("PUSHOFF 2\nPUSHIMM 1\nADD\nMALLOC\nSTOREOFF 3\n")
    sb.append("PUSHOFF 3\nSTOREOFF 4\n")
    sb.append("PUSHOFF 3\nPUSHOFF 2\nADD\nPUSHIMMCH '\\0'\nSTOREIND\n")
    sb.label(reverseStartLoopLabel.name)
    sb.append(s"PUSHOFF 2\nISNIL\nJUMPC ${reverseStopLoopLabel.name}\n")
    sb.append("PUSHOFF 3\nPUSHOFF -1\nPUSHOFF 2\nADD\nPUSHIMM 1\nSUB\nPUSHIND\nSTOREIND\n")
    sb.append("PUSHOFF 3\nPUSHIMM 1\nADD\nSTOREOFF 3\n")
    sb.append("PUSHOFF 2\nPUSHIMM 1\nSUB\nSTOREOFF 2\n")
    sb.append(s"JUMP ${reverseStartLoopLabel.name}\n")
    sb.label(reverseStopLoopLabel.name)
    sb.append("PUSHOFF 4\nSTOREOFF -1\nADDSP -3\nRST\n")

    // STR_CONCAT
    sb.label(CONCAT_LABEL)
    sb.append("PUSHIMM 0\nPUSHIMM 0\n")
    sb.append("PUSHOFF -1\n")
    sb.append(getStringLength())
    sb.append("PUSHOFF -2\n")
    sb.append(getStringLength())
    sb.append("ADD\nPUSHIMM 1\nADD\nMALLOC\nSTOREOFF 2\n")
    sb.append("PUSHOFF 2\nSTOREOFF 3\n")
    sb.append("PUSHIMM 0\nPUSHOFF 2\nPUSHOFF -2\n")
    sb.append(appendStringHeap())
    sb.append("STOREOFF 2\n")
    sb.append("PUSHIMM 0\nPUSHOFF 2\nPUSHOFF -1\n")
    sb.append(appendStringHeap())
    sb.append("STOREOFF 2\n")
    sb.append("PUSHOFF 3\nSTOREOFF -2\nADDSP -2\nRST\n")

    // STR_APPEND
    val appendStartLoopLabel = new Label()
    val appendStopLoopLabel  = new Label()
    sb.label(APPEND_LABEL)
    sb.append("PUSHOFF -2\nPUSHOFF -1\n")
    sb.label(appendStartLoopLabel.name)
    sb.append(s"PUSHOFF 3\nPUSHIND\nISNIL\nJUMPC ${appendStopLoopLabel.name}\n")
    sb.append("PUSHOFF 2\nPUSHOFF 3\nPUSHIND\nSTOREIND\n")
    sb.append("PUSHOFF 2\nPUSHIMM 1\nADD\nSTOREOFF 2\n")
    sb.append("PUSHOFF 3\nPUSHIMM 1\nADD\nSTOREOFF 3\n")
    sb.append(s"JUMP ${appendStartLoopLabel.name}\n")
    sb.label(appendStopLoopLabel.name)
    sb.append("PUSHOFF 2\nPUSHIMMCH '\\0'\nSTOREIND\nPUSHOFF 2\nSTOREOFF -3\nADDSP -2\nRST\n")

    // STR_REPEAT
    val repeatStartLoopLabel   = new Label()
    val repeatStopLoopLabel    = new Label()
    val repeatInvalidParamLabel= new Label()
    val repeatReturnLabel      = new Label()
    sb.label(REPEAT_LABEL)
    sb.append("PUSHIMM 0\nPUSHIMM 0\nPUSHIMM 0\n")
    sb.append(s"PUSHOFF -2\nISNEG\nJUMPC ${repeatInvalidParamLabel.name}\n")
    sb.append("PUSHOFF -1\n")
    sb.append(getStringLength())
    sb.append("PUSHOFF -2\nTIMES\nPUSHIMM 1\nADD\nMALLOC\nSTOREOFF 3\n")
    sb.append("PUSHOFF 3\nSTOREOFF 4\n")
    sb.label(repeatStartLoopLabel.name)
    sb.append(s"PUSHOFF 2\nPUSHOFF -2\nEQUAL\nJUMPC ${repeatStopLoopLabel.name}\n")
    sb.append("PUSHIMM 0\nPUSHOFF 3\nPUSHOFF -1\n")
    sb.append(appendStringHeap())
    sb.append("STOREOFF 3\n")
    sb.append("PUSHOFF 2\nPUSHIMM 1\nADD\nSTOREOFF 2\n")
    sb.append(s"JUMP ${repeatStartLoopLabel.name}\n")
    sb.label(repeatStopLoopLabel.name)
    sb.append(s"PUSHOFF 4\nSTOREOFF -2\nJUMP ${repeatReturnLabel.name}\n")
    sb.label(repeatInvalidParamLabel.name)
    sb.append("PUSHIMMSTR \"\"\nSTOREOFF -2\n")
    sb.label(repeatReturnLabel.name)
    sb.append("ADDSP -3\nRST\n")

    // STR_COMPARE
    val cmpStartLoopLabel = new Label()
    val cmpStopLoopLabel  = new Label()
    sb.label(COMPARE_LABEL)
    sb.append("PUSHIMM 0\nPUSHIMM 0\n")
    sb.label(cmpStartLoopLabel.name)
    sb.append(s"PUSHOFF -2\nPUSHOFF 2\nADD\nPUSHIND\nISNIL\n\n")
    sb.append("PUSHOFF -1\nPUSHOFF 2\nADD\nPUSHIND\nISNIL\nAND\n")
    sb.append(s"JUMPC ${cmpStopLoopLabel.name}\n")
    sb.append("PUSHOFF -2\nPUSHOFF 2\nADD\nPUSHIND\n")
    sb.append("PUSHOFF -1\nPUSHOFF 2\nADD\nPUSHIND\nCMP\nSTOREOFF 3\n")
    sb.append(s"PUSHOFF 3\nJUMPC ${cmpStopLoopLabel.name}\n")
    sb.append("PUSHOFF 2\nPUSHIMM 1\nADD\nSTOREOFF 2\n")
    sb.append(s"JUMP ${cmpStartLoopLabel.name}\n")
    sb.label(cmpStopLoopLabel.name)
    sb.append("PUSHOFF 3\nSTOREOFF -2\nADDSP -2\nRST\n")

    sb.toString
  }

  def reverseString(): String = {
    val sb = new SamBuilder()
    sb.append(s"LINK\nJSR $REVERSE_LABEL\nUNLINK\n")
    sb.toString
  }

  def appendStringHeap(): String = {
    val sb = new SamBuilder()
    sb.append(s"LINK\nJSR $APPEND_LABEL\nUNLINK\nADDSP -2\n")
    sb.toString
  }

  def concatString(): String = {
    val sb = new SamBuilder()
    sb.append(s"LINK\nJSR $CONCAT_LABEL\nUNLINK\nADDSP -1\n")
    sb.toString
  }

  @throws[CompilerException]
  def compareString(op: Char): String = {
    if (OperatorUtils.getBinopType(op) != BinopType.COMPARISON)
      throw new SyntaxErrorException(s"compareString receive invalid operation: $op", -1)
    val sb = new SamBuilder()
    sb.append(s"LINK\nJSR $COMPARE_LABEL\nUNLINK\nADDSP -1\n")
    if (op == '<') sb.append("PUSHIMM 1\n")
    else if (op == '>') sb.append("PUSHIMM -1\n")
    else sb.append("PUSHIMM 0\n")
    sb.append("EQUAL\n")
    sb.toString
  }
}
