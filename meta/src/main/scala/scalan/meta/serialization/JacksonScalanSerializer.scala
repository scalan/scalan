package scalan.meta.serialization

import com.fasterxml.jackson.annotation.{JsonIgnoreProperties, JsonInclude}
import com.fasterxml.jackson.core.{JsonGenerator, JsonParser}
import com.fasterxml.jackson.databind.{SerializationConfig, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper

class JacksonScalanSerializer[T: Manifest](val mapper: JacksonScalanSerializer.Mapper)
  extends ScalanSerializer[T] {
  def serialize(t: T): String = {
    mapper.writerWithDefaultPrettyPrinter().writeValueAsString(t)
  }

  def deserialize(fromString: String): T = {
    mapper.readValue[T](fromString)
  }
}

object JacksonScalanSerializer {
  type Mapper = ObjectMapper with ScalaObjectMapper

  def apply[T: Manifest]: ScalanSerializer[T] = {
    val m = new ObjectMapper with ScalaObjectMapper
    m.registerModule(DefaultScalaModule)
    m.configure(JsonGenerator.Feature.QUOTE_FIELD_NAMES, false)
    m.configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true)
    m.configOverride(classOf[List[_]]).setInclude(JsonInclude.Value.construct(JsonInclude.Include.NON_NULL, JsonInclude.Include.NON_NULL))
    apply(m)
  }

  def apply[T: Manifest](mapper: Mapper) = {
    new JacksonScalanSerializer(mapper)
  }
}
