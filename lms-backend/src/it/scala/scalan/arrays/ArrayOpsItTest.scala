package scalan.arrays

import scalan.compilation.lms.CommunityLmsBackend
import scalan.compilation.lms.scalac.{CommunityLmsCompilerScala, LmsCompilerScala}
import scalan.compilation.lms.uni.{LmsCompilerUni, LmsBackendUni}
import scalan.it.BaseItTests
import scalan._

class ArrayOpsItTests extends BaseItTests {
  trait Prog extends ScalanDsl {
    lazy val arrayBinarySearch = fun2 {(arr: Rep[Array[Int]], v: Rep[Int]) =>
      array_binary_search(v, arr)
    }
  }

  class ProgSeq extends Prog with ScalanCtxSeq
  class ProgExp extends Prog with ScalanCommunityDslExp with JNIExtractorOpsExp

  val progSeq = new ProgSeq
  val comp1 = new CommunityLmsCompilerScala(new ProgExp)
  val comp2 = new LmsCompilerUni(new ProgExp)

  def invokeMethod[A,B]( m: java.lang.reflect.Method, instance: AnyRef, in: A): B = {
    m.invoke(instance, in.asInstanceOf[AnyRef]).asInstanceOf[B]
  }

  def compareOutputWithSequential[S <: Scalan, A, B](back: LmsCompilerScala[S with ScalanCtxExp], forth: S with ScalanCtxSeq)
                                       (f: S => S#Rep[A => B], functionName: String, inputs: A*)
                                       (implicit comparator: (B, B) => Unit) {
    val compiled = compileSource[S](back)(f, functionName, back.defaultCompilerConfig)
    val (cls, method) = back.loadMethod(compiled)
    val instance = cls.newInstance().asInstanceOf[AnyRef]

    val invokeExp: A => B = invokeMethod[A,B](method, instance, _:A)
    val invokeSeq = f(forth).asInstanceOf[A => B]

    var i = -1
    inputs.foreach {in =>
      i += 1
      println(s"${getClass.getName}: checking input $i")
      comparator(invokeExp(in), invokeSeq(in))
    }
  }


  test("arrayBinarySearch") {
    val arr = Array(1, 2, 4, 7, 9)

    val vals: Array[Int] = Array(1, 4, 9, 1024, 0, -1024)
    val ins = vals map {v => (arr,v)}
    compareOutputWithSequential(comp1, progSeq)(_.arrayBinarySearch, "arrayBinarySearch", ins:_*)
    compareOutputWithSequential(comp2, progSeq)(_.arrayBinarySearch, "arrayBinarySearch", ins:_*)
  }

}
