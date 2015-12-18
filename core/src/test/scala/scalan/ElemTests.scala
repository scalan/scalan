package scalan

import scalan.common.{KindsDslExp, KindsExamples}

abstract class AbstractElemTests extends BaseNestedTests {
  class Ctx extends ScalanDslExp {
    def elementsShouldBeEqual[A: Elem, B: Elem] =
      assert(element[A] == element[B])
    def elementsShouldNotBeEqual[A: Elem, B: Elem] =
      assert(element[A] != element[B])

    def containersShouldBeEqual[A[_]: Cont, B[_]: Cont] =
      assert(container[A] == container[B])
    def containersShouldNotBeEqual[A[_]: Cont, B[_]: Cont] =
      assert(container[A] != container[B])
  }
}

class ElemTests extends AbstractElemTests {
  class Ctx extends super.Ctx with KindsExamples with KindsDslExp with JNIExtractorOpsExp

  val ctx = new Ctx
  import ctx._

  describe("Equality works as expected") {
    it("for elements") {
      elementsShouldBeEqual[Int, Int]
      elementsShouldBeEqual[Double, Double]
      elementsShouldNotBeEqual[Int, Double]

      elementsShouldBeEqual[Array[Int], Array[Int]]
      elementsShouldNotBeEqual[Array[Int], Array[Double]]
      elementsShouldNotBeEqual[Array[Int], List[Int]]

      elementsShouldBeEqual[Double => Int, Double => Int]
      elementsShouldNotBeEqual[Int => Double, Double => Int]

      elementsShouldBeEqual[(Int, Double), (Int, Double)]
      elementsShouldNotBeEqual[(Int, Double), (Double, Double)]

      elementsShouldBeEqual[Int | Int, Int | Int]
      elementsShouldNotBeEqual[Int | Int, Int]

      elementsShouldBeEqual[AString, AString]
      elementsShouldNotBeEqual[String, AString]
      elementsShouldBeEqual[CString, CString]
      elementsShouldNotBeEqual[AString, CString]
      elementsShouldBeEqual[SThrowable, SThrowable]
      elementsShouldBeEqual[Thunk[Int], Thunk[Int]]
    }

    it("for JNI types") {
      elementsShouldNotBeEqual(JNIArrayElem(element[Int]), element[Array[Int]])
    }

    it("for containers") {
      containersShouldBeEqual[ArrayBuffer, ArrayBuffer]
      containersShouldBeEqual[Array, Array]
      containersShouldBeEqual[Id, Id]
      containersShouldNotBeEqual[Array, ArrayBuffer]
    }
  }
}
