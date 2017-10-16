package scalan.util

import com.fasterxml.jackson.databind.{Module, ObjectMapper}
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import org.scalatest.{FlatSpec, Matchers, FunSpec}

import scalan.TestUtils

trait JacksonTest {
  def module: Module

  def newMapper = {
    val result = new ObjectMapper with ScalaObjectMapper
    result.registerModule(module)
    result
  }
}
