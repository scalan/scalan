package scalan.arrays

import scalan._
import scalan.compilation.lms.scalac.LmsCompilerScala
import scalan.compilation.lms.uni.LmsCompilerUni
import scalan.it.BaseItTests
import scalan.linalgebra.MatricesDslExp

trait ArrayOpsProg extends ScalanDsl {
  lazy val arrayBinarySearch = fun2 {(arr: Rep[Array[Int]], v: Rep[Int]) =>
    array_binary_search(v, arr)
  }
}

class ArrayOpsItTests extends BaseItTests[ArrayOpsProg](new ScalanDslSeq with ArrayOpsProg) {

  class ProgExp extends ScalanDslExp with ArrayOpsProg with JNIExtractorOpsExp

  val comp1 = new LmsCompilerScala(new ProgExp)
  val comp2 = new LmsCompilerUni(new ProgExp)

  val defaultCompilers = compilers(comp1, comp2)

  test("arrayBinarySearch") {
    val arr = Array(1, 2, 4, 7, 9)

    val vals: Array[Int] = Array(1, 4, 9, 1024, 0, -1024)
    val ins = vals map {v => (arr,v)}
    compareOutputWithSequential(_.arrayBinarySearch)(ins: _*)
  }

}
