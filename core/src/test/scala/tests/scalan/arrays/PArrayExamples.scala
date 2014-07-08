/**
 * User: Alexander Slesarenko
 * Date: 11/24/13
 */
package tests.scalan.arrays

import scalan.ScalanDsl
import scalan.arrays.PArraysDsl
import tests.scalan.primitives.PrimitiveExamples

trait PArrayExamples extends ScalanDsl with PArraysDsl with PrimitiveExamples {
  lazy val fromArray = fun { xs: Arr[Int] => PArray(xs) }
  lazy val fromArrayOfPairs = fun { xs: Arr[(Int,Float)] => PArray(xs) }
  lazy val fromAndTo = fun { xs: Arr[(Int,Float)] => 
    val ps = PArray(xs)
    ps.arr 
  }

  lazy val mapped = fun {(xs: PA[Int]) => xs.mapBy(inc) }
  lazy val zippedMap = fun {(xs: PA[Int]) => (xs zip xs).mapBy(tupled) }
  lazy val mapped2 = fun {(xs: PA[Int]) => xs.mapBy(inc2) }

  lazy val splitMap = fun {(xs: PA[Int]) => Pair(xs.mapBy(inc), xs.mapBy(inc2)) }
  lazy val splitMap2 = fun {(xs: PA[Int]) => Pair(xs.mapBy(inc_times), xs.mapBy(inc2)) }
  lazy val mapInc3Times = fun {(xs: PA[Int]) => Pair(xs.mapBy(inc), Pair(xs.mapBy(inc), xs.mapBy(inc))) }
  lazy val splitMap3 = fun {(xs: PA[Int]) => Pair(xs.mapBy(inc), Pair(xs.mapBy(inc2), xs.mapBy(inc_times))) }
  lazy val splitMapMap = fun {(xs: PA[Int]) => Pair(xs.mapBy(inc), xs.mapBy(inc2).mapBy(inc_times)) }

  lazy val mapScalar = fun {(xs: PA[Int]) => xs.mapBy(scalar) }
  lazy val mapArrays = fun { xs: Arr[Int] => xs.map(inc) }
  //lazy val mapScalarNested = fun {(xs: NA[Int]) => xs.mapBy(mapScalar) }

  //lazy val filterScalar = fun {(xs: PA[Int]) => xs.filter(x => x > 0) }
  //lazy val filterScalarNested = fun {(xss: NA[Int]) => xss.mapBy(filterScalar) }

}
