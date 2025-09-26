package assignment3

object Offsets {
  opaque type StackOffset = Int
  object StackOffset {
    def apply(n: Int): StackOffset = n
    extension (s: StackOffset) inline def value: Int = s
    // Conventional slots (aliases) for readability; these are relative to SP within a frame
    inline def returnSlot: StackOffset = 0      // commonly used as temporary return value slot
    inline def thisSlot: StackOffset = -1       // implicit receiver position in many call sites
  }

  opaque type FieldOffset = Int
  object FieldOffset {
    def apply(n: Int): FieldOffset = n
    extension (f: FieldOffset) inline def value: Int = f
  }
}
