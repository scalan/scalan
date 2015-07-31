package scalan

import scalan.common.{KindsDslExp, KindsExamples}

abstract class AbstractElemTests extends BaseTests {
  class Ctx extends ScalanCtxExp {
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
  class Ctx extends super.Ctx with KindsExamples with KindsDslExp

  describe("Equality works as expected") {
    test("for elements") {
      val ctx = new Ctx
      import ctx._

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
      elementsShouldBeEqual[SException, SException]
      elementsShouldBeEqual[Thunk[Int], Thunk[Int]]
    }

    test("for containers") {
      val ctx = new Ctx
      import ctx._

      containersShouldBeEqual[ArrayBuffer, ArrayBuffer]
      containersShouldBeEqual[Array, Array]
      containersShouldBeEqual[Id, Id]
      containersShouldNotBeEqual[Array, ArrayBuffer]
    }
  }
}
