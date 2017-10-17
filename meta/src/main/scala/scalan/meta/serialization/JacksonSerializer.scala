package scalan.meta.serialization

import com.fasterxml.jackson.annotation.{JsonIgnoreProperties, JsonInclude}
import com.fasterxml.jackson.core.{JsonGenerator, JsonParser}
import com.fasterxml.jackson.databind.{SerializationConfig, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper

class JacksonSerializer()
  extends ScalanSerializer {
  val mapper = {
    val m = new ObjectMapper with ScalaObjectMapper
    m.registerModule(DefaultScalaModule)
    m.configure(JsonGenerator.Feature.QUOTE_FIELD_NAMES, false)
    m.configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true)
    m.configOverride(classOf[List[_]]).setInclude(JsonInclude.Value.construct(JsonInclude.Include.NON_NULL, JsonInclude.Include.NON_NULL))
    m
  }
  def serialize[T](t: T): String = {
    mapper.writerWithDefaultPrettyPrinter().writeValueAsString(t)
  }

  def deserialize[T: Manifest](fromString: String): T = {
    mapper.readValue[T](fromString)
  }
}

object JacksonSerializer {
  type Mapper = ObjectMapper with ScalaObjectMapper
  private val serde = new JacksonSerializer()
  def apply() = serde
}
