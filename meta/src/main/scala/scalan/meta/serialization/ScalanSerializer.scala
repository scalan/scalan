package scalan.meta.serialization

trait ScalanSerializer {
  def serialize[T](t: T): String
  def deserialize[T: Manifest](fromString: String): T
}
