package scalan.util

import java.lang.reflect.{ParameterizedType, Type}

import com.fasterxml.jackson.core.`type`.TypeReference
import org.scalatest.{Matchers, FlatSpec}

trait DeserializerTest extends FlatSpec with Matchers with JacksonTest  {

  def serialize(o: AnyRef) = newMapper.writeValueAsString(o)

  def deserialize[T: Manifest](value: String) : T =
    newMapper.readValue(value, typeReference[T])

  private [this] def typeReference[T: Manifest] = new TypeReference[T] {
    override def getType = typeFromManifest(manifest[T])
  }

  private [this] def typeFromManifest(m: Manifest[_]): Type = {
    if (m.typeArguments.isEmpty) { m.runtimeClass }
    else new ParameterizedType {
      def getRawType = m.runtimeClass

      def getActualTypeArguments = m.typeArguments.map(typeFromManifest).toArray

      def getOwnerType = null
    }
  }

}
