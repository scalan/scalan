package scalan.meta.serialization

trait ScalanSerializer[T] {
  def serialize(t: T): String
  def deserialize(fromString: String): T
}
