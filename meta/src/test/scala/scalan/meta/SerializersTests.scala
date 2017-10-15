package scalan.meta

import com.fasterxml.jackson.module.scala.DefaultScalaModule

import scalan.util.{JacksonUtilTests}

class SerializersTests extends JacksonUtilTests
{
  import ScalanAst._
  lazy val module = DefaultScalaModule

  behavior of "ScalanAst serialization"

  def testType[T <: STpeExpr : Manifest](t: T)(implicit f: FixtureParam): Unit = {
    val str = f.writeValueAsString(t)
    println(str)
    val v = f.readValue[T](str)
    v should be(t)
  }

  it should "serialize STpeExpr" in { implicit f =>
    testType(STpeEmpty())
    testType(STpeConst(SConst(1, Some(TpeInt))))
    STpePrimitives.foreach { case (n, t) => testType(t) }
    testType(STpeAnnotated(TpeInt, "NonNull"))
    testType(STpeSingleton(SEmpty(Some(TpeUnit))))
//    testType(STpeSingleton())
  }
}
