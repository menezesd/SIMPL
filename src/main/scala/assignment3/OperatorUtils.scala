package assignment3

object OperatorUtils {
  def getUnop(op: Char): String = op match {
    case '~' => "PUSHIMM -1\nTIMES\n"
    case '!' => "PUSHIMM 1\nADD\nPUSHIMM 2\nMOD\n"
    case other => throw new TypeErrorException("getUnop received invalid input: " + other, -1)
  }
  def getBinop(op: Char): String = op match {
    case '+' => "ADD\n"
    case '-' => "SUB\n"
    case '*' => "TIMES\n"
    case '/' => "DIV\n"
    case '%' => "MOD\n"
    case '&' => "AND\n"
    case '|' => "OR\n"
    case '>' => "GREATER\n"
    case '<' => "LESS\n"
    case '=' => "EQUAL\n"
    case other => throw new TypeErrorException("getBinop received invalid input: " + other, -1)
  }
  def getBinopType(op: Char): BinopType = op match {
    case '+' | '-' | '*' | '/' | '%' => BinopType.ARITHMETIC
    case '&' | '|' => BinopType.BITWISE
    case '>' | '<' | '=' => BinopType.COMPARISON
    case other => throw new TypeErrorException("categorizeBinop received invalid input: " + other, -1)
  }
}
