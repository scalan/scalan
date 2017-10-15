package scalan.util

import com.fasterxml.jackson.databind.{Module, ObjectMapper}
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import org.scalatest.{FlatSpec, Matchers}

abstract class JacksonTest extends FlatSpec with Matchers {
  def module: Module

  def newMapper = {
    val result = new ObjectMapper with ScalaObjectMapper
    result.registerModule(module)
    result
  }
}
