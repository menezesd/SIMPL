
package assignment3

import assignment3.ast.MethodFrame

object MethodEmit {
  def begin(sb: SamBuilder, frame: MethodFrame, localCount: Int): Label = {
    if (localCount > 0) sb.append(s"ADDSP $localCount\n")
    val cleanup = Label()
    frame.setReturnLabel(cleanup)
    cleanup
  }
  def end(sb: SamBuilder, frame: MethodFrame, cleanup: Label): Unit = {
    sb.append(s"${cleanup.name}:\n")
    sb.append(s"STOREOFF ${frame.returnAddressOffset()}\n")
    sb.append(s"ADDSP -${frame.numLocals()}\n")
    sb.append("RST\n")
  }
}
