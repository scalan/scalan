/**
 * User: Alexander Slesarenko
 * Date: 11/24/13
 */
package scalan.scalan.arrays

import scalan.ScalanDsl
import scalan.arrays.PArraysDsl
import scalan.primitives.PrimitiveExamples

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
  lazy val mapInc3Times = fun {(xs: PA[Int]) => Tuple(xs.mapBy(inc), xs.mapBy(inc), xs.mapBy(inc)) }
  lazy val splitMap3 = fun {(xs: PA[Int]) => Tuple(xs.mapBy(inc), xs.mapBy(inc2), xs.mapBy(inc_times)) }
  lazy val splitMapMap = fun {(xs: PA[Int]) => Pair(xs.mapBy(inc), xs.mapBy(inc2).mapBy(inc_times)) }

  lazy val mapScalar = fun {(xs: PA[Int]) => xs.mapBy(scalar) }
  lazy val mapArrays = fun { xs: Arr[Int] => xs.mapBy(inc) }
  //lazy val mapScalarNested = fun {(xs: NA[Int]) => xs.mapBy(mapScalar) }

  //lazy val filterScalar = fun {(xs: PA[Int]) => xs.filter(x => x > 0) }
  //lazy val filterScalarNested = fun {(xss: NA[Int]) => xss.mapBy(filterScalar) }

  lazy val expBaseArraysInIf = fun { xs: Arr[Int] =>
    val ys = BaseArray(xs)
    val zs = BaseArray(xs.map { x => x + 1 })
    val res = IF (xs.length > 10) THEN { ys } ELSE { zs }
    res
  }
  lazy val expBaseArraysInIfSpec = fun { xs: Arr[Int] =>
    expBaseArraysInIf(xs).arr
  }

  lazy val expPairArraysInIf = fun { xs: Arr[Int] =>
    val pairs1 = PairArray(BaseArray(xs), BaseArray(xs))
    val ys = xs.map { x => x + 1 }
    val pairs2 = PairArray(BaseArray(ys), BaseArray(xs))
    val res = IF (xs.length > 10) THEN { pairs1 } ELSE { pairs2 }
    res
  }
  lazy val expPairArraysInIfSpec = fun { xs: Arr[Int] =>
    expPairArraysInIf(xs).arr
  }

  lazy val expPairArraysInIfDiffTypes = fun { xs: Arr[(Int,Int)] =>
    val ys = xs.map { x => x._1 + x._2 }
    val pairs1: PA[(Int,Int)] = PairArray(BaseArray(ys), BaseArray(ys))
    val pairs2: PA[(Int,Int)] = BaseArray(xs)
    val res = IF (xs.length > 10) THEN { pairs1 } ELSE { pairs2 }
    res
  }
  lazy val expPairArraysInIfDiffTypesSpec = fun { xs: Arr[(Int,Int)] =>
    expPairArraysInIfDiffTypes(xs).arr
  }

  lazy val sumFold = fun { in: Rep[Array[(Int,Int)] | Array[Int]] =>
    in.fold(xs => {
      val ys = xs.map { x => x._1 + x._2}
      val pairs1: PA[(Int, Int)] = PairArray(BaseArray(ys), BaseArray(ys))
      val pairs2: PA[(Int, Int)] = BaseArray(xs)
      val res = IF(xs.length > 10) THEN {
        pairs1
      } ELSE {
        pairs2
      }
      res
    },
    ys => PairArray(BaseArray(ys), BaseArray(ys)))
  }
  lazy val sumFoldSpec = fun { in: Rep[Array[(Int,Int)] | Array[Int]] =>
    sumFold(in).arr
  }

  lazy val pairInIf = fun { in: Arr[Int] =>
    val xs = BaseArray(in)
    val ys = xs.map { x => x + 1 }
    val res = IF (xs.length > 10) THEN { Pair(ys, xs) } ELSE { Pair(xs,xs) }
    res._1
  }
  lazy val pairInIfSpec = fun { xs: Arr[Int] =>
    pairInIf(xs).arr
  }

  lazy val nestedPairInIf = fun { in: Arr[Int] =>
    val xs: PA[Int] = BaseArray(in)
    val ys = xs.map { x => x + 1 }
    val res = IF (xs.length > 10) THEN { Pair(Pair(xs, ys), xs) } ELSE { Pair(Pair(xs,xs),ys) }
    Pair(res._1._2, res._2)
  }
  lazy val nestedPairInIfSpec = fun { xs: Arr[Int] =>
    val res = nestedPairInIf(xs)
    (res._1.arr, res._2.arr)
  }

}
