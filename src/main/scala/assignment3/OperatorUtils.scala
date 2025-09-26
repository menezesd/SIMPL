
package assignment3

object OperatorUtils {
  def getUnop(op: Char): String = op match {
    case '~' =>
      val sb = new SamBuilder()
      sb.pushImm(-1).mul().toString
    case '!' =>
      val sb = new SamBuilder()
      sb.pushImm(1).add().pushImm(2).mod().toString
    case other => throw TypeErrorException(s"getUnop received invalid input: $other", -1)
  }
  def getBinop(op: Char): String = op match {
  case '+' => new SamBuilder().add().toString
  case '-' => new SamBuilder().sub().toString
  case '*' => new SamBuilder().mul().toString
  case '/' => new SamBuilder().div().toString
  case '%' => new SamBuilder().mod().toString
  case '&' => new SamBuilder().andB().toString
  case '|' => new SamBuilder().orB().toString
  case '>' => new SamBuilder().greater().toString
  case '<' => new SamBuilder().less().toString
  case '=' => new SamBuilder().equal().toString
    case other => throw TypeErrorException(s"getBinop received invalid input: $other", -1)
  }
  def getBinopType(op: Char): BinopType = op match {
    case '+' | '-' | '*' | '/' | '%' => BinopType.ARITHMETIC
    case '&' | '|' => BinopType.BITWISE
    case '>' | '<' | '=' => BinopType.COMPARISON
    case other => throw TypeErrorException(s"categorizeBinop received invalid input: $other", -1)
  }
}
