package scalan.util

import scala.reflect.runtime.universe._
import scalan.common.SegmentsDsl
import scalan.{BaseCtxTests, ScalanDslExp}

class ReflectionTests extends BaseCtxTests {

  trait ReflectionExamples extends ScalanDslExp with SegmentsDsl {
  }

  test("paramMirrors") {
    val ctx = new TestContext with ReflectionExamples
    import ctx._

    def check(x: Any)(preds: (List[ParamMirror] => Boolean)*) = {
      val paramMirrors = ReflectionUtil.paramMirrors(x)
      preds.foreach { pred => assert(pred(paramMirrors)) }
    }

    def namesAre(names: String*) = (_: List[ParamMirror]).map(_.name.toString) == names.toList

    def canGetAll = { params: List[ParamMirror] =>
      // don't check results
      params.foreach(_.get)
      true
    }

    // Classes on the right-hand side are used to get a variety of constructor parameter definitions

    // Same field type
    val interval = new IntervalDef(1, 2)
    check(interval)(canGetAll, namesAre("start", "end"))

    // No implicit parameters, `extends ArrayDef`
//    val arr = ArrayRangeFrom0(10)
//    check(arr)(canGetAll, namesAre("n"))

    // A lambda
    val f @ Def(lambda) = fun { x: Rep[Int] => x + 1 }
    check(f)(canGetAll, namesAre("id", "eT"))

    check(lambda)(canGetAll, namesAre("f", "x", "y", "self0", "mayInline", "eA", "eB"))

    // `implicit eItem` without `val` with `extends ArrayDef`
//    val sort = ArraySort(arr, implicitly[Ordering[Int]])
//    // Scala-reflect bug: last name is `eItem ` and `selfType `
//    check(sort)(canGetAll)//, namesAre("xs", "o", "eItem"))

    // implicit constructor parameters with different name from `eItem` and `extends ArrayDef`
//    val zip = ArrayZip(arr, sort)
//    check(zip)(canGetAll, namesAre("xs", "ys", "eT", "eU"))

    // context bound with `extends ArrayBufferDef` (`eItem` is reused for it)
//    val arrBuf = ArrayBufferEmpty[Int]
//    check(arrBuf)(canGetAll)//, namesAre("eItem"))

    // context bound with `extends BaseDef` (`selfType` is reused for it)
//    val arrBufApply = ArrayBufferApply(arrBuf, 0)
//    check(arrBufApply)(canGetAll)//, namesAre("buf", "i", "selfType"))
  }

}