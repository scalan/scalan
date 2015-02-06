package scalan.collection

import scalan.ScalanDsl
import scalan.primitives.PrimitiveExamples

trait CollectionExamples extends ScalanDsl with CollectionsDsl with PrimitiveExamples {
  lazy val fromArray = fun { xs: Arr[Int] => Collection(xs) }
  lazy val fromArrayOfPairs = fun { xs: Arr[(Int,Float)] => Collection(xs) }
  lazy val fromAndTo = fun { xs: Arr[(Int,Float)] =>
    val ps = Collection(xs)
    ps.arr
  }

  lazy val mapped = fun {(xs: Coll[Int]) => xs.mapBy(inc) }
  lazy val zippedMap = fun {(xs: Coll[Int]) => (xs zip xs).mapBy(tupled) }
  lazy val mapped2 = fun {(xs: Coll[Int]) => xs.mapBy(inc2) }

  lazy val splitMap = fun {(xs: Coll[Int]) => Pair(xs.mapBy(inc), xs.mapBy(inc2)) }
  lazy val splitMap2 = fun {(xs: Coll[Int]) => Pair(xs.mapBy(inc_times), xs.mapBy(inc2)) }
  lazy val mapInc3Times = fun {(xs: Coll[Int]) => Tuple(xs.mapBy(inc), xs.mapBy(inc), xs.mapBy(inc)) }
  lazy val splitMap3 = fun {(xs: Coll[Int]) => Tuple(xs.mapBy(inc), xs.mapBy(inc2), xs.mapBy(inc_times)) }
  lazy val splitMapMap = fun {(xs: Coll[Int]) => Pair(xs.mapBy(inc), xs.mapBy(inc2).mapBy(inc_times)) }

  lazy val mapScalar = fun {(xs: Coll[Int]) => xs.mapBy(scalar) }
  lazy val mapArrays = fun { xs: Arr[Int] => xs.mapBy(inc) }
  //lazy val mapScalarNested = fun {(xs: NA[Int]) => xs.mapBy(mapScalar) }

  //lazy val filterScalar = fun {(xs: Coll[Int]) => xs.filter(x => x > 0) }
  //lazy val filterScalarNested = fun {(xss: NA[Int]) => xss.mapBy(filterScalar) }

  lazy val expBaseCollectionsInIf = fun { xs: Arr[Int] =>
    val ys = BaseCollection(xs)
    val zs = BaseCollection(xs.map { x => x + 1 })
    val res = IF (xs.length > 10) THEN { ys } ELSE { zs }
    res
  }

  lazy val expListCollectionsInIf = fun { xs: Lst[Int] =>
    val ys = ListCollection(xs)
    val zs = ListCollection(xs.map { x => x + 1 })
    val res = IF (xs.length > 10) THEN { ys } ELSE { zs }
    res
  }

  lazy val expBaseCollectionsInIfSpec = fun { xs: Arr[Int] =>
    expBaseCollectionsInIf(xs).arr
  }

  lazy val expPairCollectionsInIf = fun { xs: Arr[Int] =>
    val pairs1 = PairCollection(BaseCollection(xs), BaseCollection(xs))
    val ys = xs.map { x => x + 1 }
    val pairs2 = PairCollection(BaseCollection(ys), BaseCollection(xs))
    val res = IF (xs.length > 10) THEN { pairs1 } ELSE { pairs2 }
    res
  }
  lazy val expPairCollectionsInIfSpec = fun { xs: Arr[Int] =>
    expPairCollectionsInIf(xs).arr
  }

  lazy val expPairCollectionsInIfDiffTypes = fun { xs: Arr[(Int,Int)] =>
    val ys = xs.map { x => x._1 + x._2 }
    val pairs1: Coll[(Int,Int)] = PairCollection(BaseCollection(ys), BaseCollection(ys))
    val pairs2: Coll[(Int,Int)] = BaseCollection(xs)
    val res = IF (xs.length > 10) THEN { pairs1 } ELSE { pairs2 }
    res
  }
  lazy val expPairCollectionsInIfDiffTypesSpec = fun { xs: Arr[(Int,Int)] =>
    expPairCollectionsInIfDiffTypes(xs).arr
  }

  lazy val sumFold = fun { in: Rep[Array[(Int,Int)] | Array[Int]] =>
    in.fold(xs => {
      val ys = xs.map { x => x._1 + x._2}
      val pairs1: Coll[(Int, Int)] = PairCollection(BaseCollection(ys), BaseCollection(ys))
      val pairs2: Coll[(Int, Int)] = BaseCollection(xs)
      val res = IF(xs.length > 10) THEN {
        pairs1
      } ELSE {
        pairs2
      }
      res
    },
      ys => PairCollection(BaseCollection(ys), BaseCollection(ys)))
  }
  lazy val sumFoldSpec = fun { in: Rep[Array[(Int,Int)] | Array[Int]] =>
    sumFold(in).arr
  }

  lazy val pairInIf = fun { in: Arr[Int] =>
    val xs = BaseCollection(in)
    val ys = xs.map { x => x + 1 }
    val res = IF (xs.length > 10) THEN { Pair(ys, xs) } ELSE { Pair(xs,xs) }
    res._1
  }
  lazy val pairInIfSpec = fun { xs: Arr[Int] =>
    pairInIf(xs).arr
  }

  lazy val nestedPairInIf = fun { in: Arr[Int] =>
    val xs: Coll[Int] = BaseCollection(in)
    val ys = xs.map { x => x + 1 }
    val res = IF (xs.length > 10) THEN { Pair(Pair(xs, ys), xs) } ELSE { Pair(Pair(xs,xs),ys) }
    Pair(res._1._2, res._2)
  }
  lazy val nestedPairInIfSpec = fun { xs: Arr[Int] =>
    val res = nestedPairInIf(xs)
    (res._1.arr, res._2.arr)
  }

  lazy val nestedListPairInIf = fun { in: Lst[Int] =>
    val xs: Coll[Int] = ListCollection(in)
    val ys = xs.map { x => x + 1 }
    val res = IF (xs.length > 10) THEN { Pair(Pair(xs, ys), xs) } ELSE { Pair(Pair(xs,xs),ys) }
    Pair(res._1._2, res._2)
  }
  lazy val nestedListPairInIfSpec = fun { xs: Lst[Int] =>
    val res = nestedListPairInIf(xs)
    (res._1.arr, res._2.arr)
  }

}
