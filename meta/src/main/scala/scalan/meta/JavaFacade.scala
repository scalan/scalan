package scalan.meta

object JavaFacade {
  def nil[T](): List[T] = Nil
  def cons[T](x: T, xs: List[T]): List[T] = x :: xs
  def list[T](xs: T*): List[T] = List(xs: _*)
  def none[T](): Option[T] = None
  def some[T](x: T): Option[T] = Some(x)
  def emitCode[T](x: T)(implicit emitter: CodeEmitter[T]): String = emitter(x)
}
