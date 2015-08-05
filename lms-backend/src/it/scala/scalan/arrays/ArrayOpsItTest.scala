package scalan.arrays

import scalan.compilation.lms.CommunityLmsBackend
import scalan.compilation.lms.scalac.{CommunityLmsCompilerScala, LmsCompilerScala}
import scalan.compilation.lms.uni.{LmsCompilerUni, LmsBackendUni}
import scalan.it.BaseItTests
import scalan.{JNIExtractorOpsExp, ScalanCommunityDslExp, ScalanCtxSeq, ScalanDsl}

class ArrayOpsItTests extends BaseItTests {
  trait Prog extends ScalanDsl {
    lazy val arrayBinarySearch = fun2 {(arr: Rep[Array[Int]], v: Rep[Int]) =>
      array_binary_search(v, arr)
    }
  }

  class ProgSeq extends Prog with ScalanCtxSeq
  class ProgExp extends Prog with ScalanCommunityDslExp with JNIExtractorOpsExp

  val progSeq = new ProgSeq
  val comp1 = new CommunityLmsCompilerScala {
    lazy val scalan = new ProgExp
  }
  val comp2 = new LmsCompilerUni {
    lazy val scalan = new ProgExp
  }

  def invokeMethod[A,B]( m: java.lang.reflect.Method, instance: AnyRef, in: A): B = {
    m.invoke(instance, in.asInstanceOf[AnyRef]).asInstanceOf[B]
  }

  def compareOutputWithSequential[A, B](back: LmsCompilerScala)
                                       (fSeq: A => B, f: back.Exp[A => B], functionName: String, inputs: A*)
                                       (implicit comparator: (B, B) => Unit) {
    val compiled = compileSource(back)(f, functionName, back.defaultCompilerConfig)
    val (cls, method) = back.loadMethod(compiled)
    val instance = cls.newInstance().asInstanceOf[AnyRef]

    val invoke: A => B = invokeMethod[A,B](method, instance, _:A)

    var i = -1
    inputs.foreach {in =>
      i += 1
      println(s"${getClass.getName}: checking input $i")
      comparator(invoke(in), fSeq(in))
    }
  }


  test("arrayBinarySearch") {
    val arr = Array(1, 2, 4, 7, 9)

    val vals: Array[Int] = Array(1, 4, 9, 1024, 0, -1024)
    val ins = vals map {v => (arr,v)}
    compareOutputWithSequential(comp1)(progSeq.arrayBinarySearch, comp1.scalan.arrayBinarySearch, "arrayBinarySearch", ins:_*)
    compareOutputWithSequential(comp2)(progSeq.arrayBinarySearch, comp2.scalan.arrayBinarySearch, "arrayBinarySearch", ins:_*)
  }

}
