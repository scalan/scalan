package scalan.collections

import scalan.ScalanCommunityDsl
import scalan.primitives.PrimitiveExamples

trait CollectionExamples extends ScalanCommunityDsl with PrimitiveExamples {
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
  lazy val mapCollections = fun { xs: Coll[Int] => xs.mapBy(inc) }
  //lazy val mapScalarNested = fun {(xs: NA[Int]) => xs.mapBy(mapScalar) }

  //lazy val filterScalar = fun {(xs: Coll[Int]) => xs.filter(x => x > 0) }
  //lazy val filterScalarNested = fun {(xss: NA[Int]) => xss.mapBy(filterScalar) }

  lazy val expCollectionOverArraysInIf = fun { xs: Arr[Int] =>
    val ys = CollectionOverArray(xs)
    val zs = CollectionOverArray(xs.map { x => x + 1 })
    val res = IF (xs.length > 10) THEN { ys } ELSE { zs }
    res
  }

  lazy val expCollectionOverListsInIf = fun { xs: Lst[Int] =>
    val ys = CollectionOverList(xs)
    val zs = CollectionOverList(xs.map { x => x + 1 })
    val res = IF (xs.length > 10) THEN { ys } ELSE { zs }
    res
  }

  lazy val expCollectionOverArraysInIfSpec = fun { xs: Arr[Int] =>
    expCollectionOverArraysInIf(xs).arr
  }

  lazy val expPairCollectionSOAsInIf = fun { xs: Arr[Int] =>
    val pairs1 = PairCollectionSOA(CollectionOverArray(xs), CollectionOverArray(xs))
    val ys = xs.map { x => x + 1 }
    val pairs2 = PairCollectionSOA(CollectionOverArray(ys), CollectionOverArray(xs))
    val res = IF (xs.length > 10) THEN { pairs1 } ELSE { pairs2 }
    res
  }
  lazy val expPairCollectionSOAsInIfSpec = fun { xs: Arr[Int] =>
    expPairCollectionSOAsInIf(xs).arr
  }

  lazy val expPairCollectionSOAsInIfDiffTypes = fun { xs: Arr[(Int,Int)] =>
    val ys = xs.map { x => x._1 + x._2 }
    val pairs1: Coll[(Int,Int)] = PairCollectionSOA(CollectionOverArray(ys), CollectionOverArray(ys))
    val pairs2: Coll[(Int,Int)] = CollectionOverArray(xs)
    val res = IF (xs.length > 10) THEN { pairs1 } ELSE { pairs2 }
    res
  }
  lazy val expPairCollectionSOAsInIfDiffTypesSpec = fun { xs: Arr[(Int,Int)] =>
    expPairCollectionSOAsInIfDiffTypes(xs).arr
  }

  lazy val sumFold = fun { in: Rep[Array[(Int,Int)] | Array[Int]] =>
    in.fold(xs => {
      val ys = xs.map { x => x._1 + x._2}
      val pairs1: Coll[(Int, Int)] = PairCollectionSOA(CollectionOverArray(ys), CollectionOverArray(ys))
      val pairs2: Coll[(Int, Int)] = CollectionOverArray(xs)
      val res = IF(xs.length > 10) THEN {
        pairs1
      } ELSE {
        pairs2
      }
      res
    },
      ys => PairCollectionSOA(CollectionOverArray(ys), CollectionOverArray(ys)))
  }
  lazy val sumFoldSpec = fun { in: Rep[Array[(Int,Int)] | Array[Int]] =>
    sumFold(in).arr
  }

  lazy val pairInIf = fun { in: Arr[Int] =>
    val xs = CollectionOverArray(in)
    val ys = xs.map { x => x + 1 }
    val res = IF (xs.length > 10) THEN { Pair(ys, xs) } ELSE { Pair(xs,xs) }
    res._1
  }
  lazy val pairInIfSpec = fun { xs: Arr[Int] =>
    pairInIf(xs).arr
  }

  lazy val nestedPairInIf = fun { in: Arr[Int] =>
    val xs: Coll[Int] = CollectionOverArray(in)
    val ys = xs.map { x => x + 1 }
    val res = IF (xs.length > 10) THEN { Pair(Pair(xs, ys), xs) } ELSE { Pair(Pair(xs,xs),ys) }
    Pair(res._1._2, res._2)
  }
  lazy val nestedPairInIfSpec = fun { xs: Arr[Int] =>
    val res = nestedPairInIf(xs)
    (res._1.arr, res._2.arr)
  }

  lazy val nestedListPairInIf = fun { in: Lst[Int] =>
    val xs: Coll[Int] = CollectionOverList(in)
    val ys = xs.map { x => x + 1 }
    val res = IF (xs.length > 10) THEN { Pair(Pair(xs, ys), xs) } ELSE { Pair(Pair(xs,xs),ys) }
    Pair(res._1._2, res._2)
  }
  lazy val nestedListPairInIfSpec = fun { xs: Lst[Int] =>
    val res = nestedListPairInIf(xs)
    (res._1.arr, res._2.arr)
  }

  lazy val listCollectionPairZipWith = fun { in: Lst[Int] =>
    val xs: Coll[Int] = Collection.fromList(in)
    (xs zip xs).map(x => x._1 + x._2).arr
  }

}
