
package assignment3

import assignment3.ast.MethodFrame
import assignment3.Offsets.StackOffset

object MethodEmit {
  def begin(sb: SamBuilder, frame: MethodFrame, localCount: Int): Label = {
    if (localCount > 0) sb.addSp(localCount)
    val cleanup = Label()
    frame.setReturnLabel(cleanup)
    cleanup
  }
  def end(sb: SamBuilder, frame: MethodFrame, cleanup: Label): Unit = {
    sb.label(cleanup)
    sb.storeOffS(StackOffset(frame.returnAddressOffset()))
    sb.addSp(-frame.numLocals())
    sb.rst()
  }
}
