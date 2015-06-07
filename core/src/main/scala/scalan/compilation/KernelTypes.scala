package scalan.compilation

object KernelTypes extends Enumeration {
  type KernelType = Value
  val ScalaKernel, CppKernel = Value
}
