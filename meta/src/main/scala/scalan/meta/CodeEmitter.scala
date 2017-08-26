package scalan.meta

class CodeEmitter[T](emit: T => String) extends (T => String) {
  override def apply(x: T) = emit(x)
}
