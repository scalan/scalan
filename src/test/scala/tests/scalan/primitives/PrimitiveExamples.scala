/**
 * User: Alexander Slesarenko
 * Date: 11/24/13
 */
package tests.scalan.primitives

import scalan.ScalanDsl

trait PrimitiveExamples extends ScalanDsl {
  lazy val id = fun {(x: Rep[Int]) => x}
  lazy val inc = fun {(x: Rep[Int]) => x + 1}
  //lazy val curred = fun {(x: Rep[Int]) => (y: Rep[Int]) => x + y }
  lazy val tupled = fun {(x: Rep[(Int,Int)]) => x._1 + x._2 }
  lazy val highOrder = fun {(x: Rep[Int]) => {
    val x1 = x + 1
    fun {(y: Rep[Int]) => y + x + x1 }
  } }
  lazy val inc2 = fun {(x: Rep[Int]) => x + ((1:Rep[Int]) + 1)}
  lazy val inc_times = fun {(x: Rep[Int]) => x + ((1:Rep[Int]) + 1) * 2 }
  lazy val scalar = fun {(x: Rep[Int]) => (x + 1) * (x + 2) }
}
