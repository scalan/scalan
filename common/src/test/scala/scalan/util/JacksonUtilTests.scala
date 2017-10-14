package scalan.util

import com.fasterxml.jackson.databind.annotation.JsonDeserialize
import com.fasterxml.jackson.databind.{Module, ObjectMapper, JsonMappingException, JsonNode}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import org.scalatest.{fixture, Matchers, Outcome}
import org.scalatest.junit.JUnitRunner

import scalan.BaseShouldTests

abstract class JacksonUtilTests extends fixture.FlatSpec with Matchers {

  type FixtureParam = ObjectMapper with ScalaObjectMapper

  def getMapper: FixtureParam = new ObjectMapper() with ScalaObjectMapper

  def withFixture(test: OneArgTest): Outcome =
  {
    val m = getMapper
    m.registerModule(module)
    test(m)
  }

  def module: Module

  def newMapper = {
    val result = new ObjectMapper
    result.registerModule(module)
    result
  }

  def serialize(value: Any, mapper: ObjectMapper = newMapper): String = mapper.writeValueAsString(value)

  def jsonOf(s: String): JsonNode = newMapper.readTree(s)
}

object Examples {

  case class OptionInt(value: Option[Int])
  case class AnnotatedOptionInt(@JsonDeserialize(contentAs = classOf[java.lang.Integer]) value: Option[Int])
  case class OptionLong(value: Option[Long])
  case class AnnotatedOptionLong(@JsonDeserialize(contentAs = classOf[java.lang.Long]) value: Option[Long])

  case class AnnotatedHashKeyLong(@JsonDeserialize(keyAs = classOf[java.lang.Long]) value: Map[Long, String])
  case class AnnotatedHashValueLong(@JsonDeserialize(contentAs = classOf[java.lang.Long]) value: Map[String, Long])

  trait M1
  case class F1(label: String) extends M1
  case class C1(m: Option[M1])
}

class PrimitiveContainersTests extends JacksonUtilTests
{
  import Examples._
  lazy val module = DefaultScalaModule

  behavior of "Primitive Containers"

  it should "support deserializing primitives" in { f =>
    val value = f.readValue[OptionInt]("""{"value":1}""")
    value.value shouldBe Some(1)
  }

  it should "support primitive conversions in" in { f =>
    val value = f.readValue[AnnotatedOptionInt]("""{"value":"1"}""")
    value.value shouldBe Some(1)
  }

  it should "support type widening"  in { f =>
    val value = f.readValue[AnnotatedOptionLong]("""{"value":1}""")
    value.value.get shouldBe 1L
  }

  it should "enforce type constraints"  in { f =>
    val thrown = intercept[JsonMappingException] {
      f.readValue[AnnotatedOptionInt]("""{"value":9223372036854775807}""").value.get
    }
    thrown.getMessage should startWith ("Numeric value (9223372036854775807) out of range")
  }

  it should "support map keys" in { f =>
    val value = f.readValue[AnnotatedHashKeyLong]("""{"value":{"1":"one"}}""")
    value.value should contain key 1L
    value.value(1L) shouldBe "one"
  }

  it should "support map values" in { f =>
    val value = f.readValue[AnnotatedHashValueLong]("""{"value":{"key": "1"}}""")
    value.value should contain key "key"
    value.value("key") shouldBe 1L
  }

}

/**
  * See (https://github.com/FasterXML/jackson-module-scala/blob/master/src/test/scala/com/fasterxml/jackson/module/scala/ser/OptionSerializerTest.scala)
  * for more tests
  */
class OptionSerializerTests extends JacksonUtilTests {
  import Examples._
  lazy val module = DefaultScalaModule
  
  "An ObjectMapper with OptionSerializer" should "serialize an Option[Int]" in { f =>
    val noneOption: Option[Int] = None
    serialize(Option(1)) should be ("1")
    serialize(Some(1)) should be ("1")
    serialize(noneOption) should be ("null")
  }

  it should "serialize concrete type when using Option[Trait] (in object)" in { f =>
    // Additional test case for #240, for some reason this test case works fine.
    // However, if the classes are moved outside of the object then it starts
    // breaking.
    val text = """{"m":{"label":"foo"}}"""
    val obj = C1(Some(F1("foo")))
    serialize(obj) should be (text)
    f.readValue[C1](text) shouldBe obj
  }
}