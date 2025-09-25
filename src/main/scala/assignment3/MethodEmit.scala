package assignment3

import assignment3.ast.MethodFrame

object MethodEmit {
  def begin(sb: SamBuilder, frame: MethodFrame, localCount: Int): Label = {
    if (localCount > 0) sb.append("ADDSP ").append(Integer.toString(localCount)).append("\n")
    val cleanup = new Label()
    frame.setReturnLabel(cleanup)
    cleanup
  }
  def end(sb: SamBuilder, frame: MethodFrame, cleanup: Label): Unit = {
    sb.append(cleanup.getName).append(":\n")
    sb.append("STOREOFF ").append(Integer.toString(frame.returnAddressOffset())).append("\n")
    sb.append("ADDSP -").append(Integer.toString(frame.numLocals())).append("\n")
    sb.append("RST\n")
  }
}
