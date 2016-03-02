package scalan.util

import scala.language.reflectiveCalls
import scalan.common.{SegmentsDslExp, SegmentsDsl}
import scalan.{ScalanDslExp, BaseCtxTests, ScalanDsl, BaseTests}
import scala.reflect.runtime.universe._

class ReflectionTests extends BaseCtxTests {

  trait ReflectionExamples extends ScalanDslExp with SegmentsDslExp {
    val interval = new ExpInterval(1, 2)
  }

  test("paramFieldMirrors") {
    val ctx = new TestContext with ReflectionExamples
    val clazz = classOf[ctx.ExpInterval]
    val javaMirror = runtimeMirror(clazz.getClassLoader)
    val selfTypeSym = ReflectionUtil.classToSymbol(classOf[ctx.BaseDef[_]]).toType.decl(TermName("selfType")).asTerm
    val instanceMirror = javaMirror.reflect(ctx.interval)
    val fieldMirrors = ReflectionUtil.paramFieldMirrors(clazz, instanceMirror, selfTypeSym)
    assert(fieldMirrors.distinct.length == 2)
  }

}
