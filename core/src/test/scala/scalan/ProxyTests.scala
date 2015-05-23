package scalan

import scala.reflect.runtime.universe._
import scalan.common.{SegmentsDslExp, Lazy}
//import scalan.monads.{FreesDslExp, MonadsDslExp}

class ProxyTests extends BaseTests { suite =>
  class Ctx(testName: String) extends TestContext(suite, testName) with SegmentsDslExp /*with FreesDslExp with MonadsDslExp*/ {
    def testElemFromType(tpe: Type, elemMap: Map[Symbol, TypeDesc], expected: Elem[_]) = {
      assertResult(expected)(elemFromType(tpe, elemMap, definitions.NothingTpe))
    }

    /**
     * Check that calling getResultElem on TReceiver.name produces expectedResultElem.
     * params is a seq of pairs (JVM param class, param). Note that for JVM class for `Rep` is `classOf[Object]`
     */
    def testResultElem[TReceiver, TResult](name: String, params: (Class[_], AnyRef)*)(implicit tag: TypeTag[TReceiver], expectedResultElem: Elem[TResult]) = {
      val tpe = tag.tpe
      val clazz = runtimeMirror(getClass.getClassLoader).asInstanceOf[JavaMirror].runtimeClass(tpe)
      val (paramClasses, realParams) = params.unzip
      val method = clazz.getMethod(name, paramClasses: _*)
      val receiverElem = elemFromType(tpe, Map.empty, definitions.NothingTpe)
      val receiver = fresh(Lazy(receiverElem))
      val actualResultElem = getResultElem(receiver, method, realParams.toList)

      assertResult(expectedResultElem)(actualResultElem)
    }
  }

  test("getResultElem") {
    val ctx = new Ctx("getResultElem")
    import ctx._

    testResultElem[Segment, Int]("start")
    testResultElem[SThrowable, SThrowable]("initCause", classOf[Object] -> SThrowable(""))
    // Tests below don't work and probably need changes in testResultElem and/or getResultElem
    // pending: testResultElem[Monad[Id], Int]("unit", classOf[Object] -> toRep(0), classOf[Element[_]] -> IntElement)
    // pending: testResultElem[Array[Double], Double]("apply", classOf[Object] -> toRep(0))
  }
}
