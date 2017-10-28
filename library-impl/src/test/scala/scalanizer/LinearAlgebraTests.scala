package scalanizer

import org.scalatest._

import scala.util.Random
import scalan.collection.ColOverArrayBuilder

class LinearAlgebraTests extends FunSuite  {

  object CommonData {
    val rnd = new Random(1)

    def genArray(len: Int): Array[Double] = {
      { for (i <- 1 to len) yield { rnd.nextDouble() } }.toArray
    }

    def genMatr(rows: Int, cols: Int): Array[Array[Double]] = {
      { for (i <- 1 to rows) yield { genArray(cols) } }.toArray
    }
  }

  test("ddmvm") {
    val len = new ColOverArrayBuilder().ddmvm(Array(10, 20))
    assertResult(2)(len)
  }
//  test("ddmvm") {
//    val rows = 10000
//    val cols = rows;
//
//    println( s"""Generating random matrix ${rows}x${cols} dense format "Array[Array[Double]]" ...""")
//    val inM = CommonData.genMatr(rows, cols)
//    println( s"""Generating random vector ${rows} elements "Array[Double]" ...""")
//    val inV = CommonData.genArray(cols)
//
//    val correctRes = {
//      println("Calculating expected result")
//      val t1 = System.currentTimeMillis()
//    val correctRes = inM.map({ r: Array[Double] => r.zip(inV).map({ p => p._1 * p._2 }).fold(0.0)(_ + _) })
//    val t2 = System.currentTimeMillis()
//    val dt = t2 - t1
//    println(s"  $dt ms")
//    correctRes
//  }
//
//    {
//      println(s"Heating up JVM ...")
//      val t1 = System.currentTimeMillis()
//      for (i <- 0 to 1) {
//        val res0 = LA.ddmvm(inM, inV)
//      }
//      val t2 = System.currentTimeMillis()
//      val dt = t2 - t1
//      println(s"  $dt ms")
//    }
//    {
//      println(s"Multiplying matrix by vector...")
//      val t1 = System.currentTimeMillis()
//      val res = LA.ddmvm(inM, inV)
//      val t2 = System.currentTimeMillis()
//      assertResult(correctRes)(res)
//      val dt = t2 - t1
//      println(s"  $dt ms")
//    }
//
//
//  }
}
