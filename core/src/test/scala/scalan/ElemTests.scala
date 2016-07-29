package scalan

import scalan.common.{KindsDslExp, KindsExamples}
import scala.reflect.runtime.universe._

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

/** See also scalan.collections.MoreElemTests in collections subproject */
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

    it("for ArgElem") {
      val ae = ArgElem("A")
      ae.tag.tpe match {
        case t: TypeRef =>
          assert(t.typeArgs.isEmpty)
          assert(t.sym.isInstanceOf[FreeTypeSymbolApi])
      }
      println(ae.tag.tpe)
      //    println(s"ClassTag: ${ae.classTag}")
      val be = ArgElem("B")
      val ae2 = ArgElem("A")
      assert(!ae.equals(be))
      assert(ae.equals(ae2))

      assert(ae.name == "A")
      assert(be.name == "B")

      assert(!(ae <:< AnyElement))
      assert(!(AnyElement <:< ae))
      assert(ae <:< ae)
      assert(ae <:< ae2)
      assert(be <:< be)
      assert(!(ae <:< be))
      assert(ae.toString == "A")
    }

    it("for Struct elements") {
      val se1 = tuple3StructElement[Int, Char, String]
      val se2 = tuple3StructElement[Int, Char, String]
      val se3 = tuple3StructElement[Int, Char, Double]
      elementsShouldBeEqual(se1, se2)
      elementsShouldNotBeEqual(se1, se3)

      val ae1 = arrayElement(se1)
      val ae2 = arrayElement(se2)
      val ae3 = arrayElement(se3)
      elementsShouldBeEqual(ae1, ae2)
      elementsShouldNotBeEqual(ae1, ae3)

      val be1 = arrayBufferElement(se1)
      val be2 = arrayBufferElement(se2)
      val be3 = arrayBufferElement(se3)
      elementsShouldBeEqual(be1, be2)
      elementsShouldNotBeEqual(be1, be3)

      elementsShouldNotBeEqual(ae1, be1)
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
