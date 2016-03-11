package scalan.collections

import scalan.{ScalanDslStd, Scalan}
import scalan.it.BaseItTests
import scalan.primitives.Functions

trait SimpleMapProg extends Scalan {
  lazy val mapEmpty = fun {_:Rep[Int] =>
    MMap.empty[Int,Int]
  }

  lazy val mapPutContains = fun { p:Rep[(Int,Double)] =>
    val m = MMap.empty[Int,Double]
    val m1 = m.update(p._1, p._2) | m
    m1.contains(p._1)
  }

  lazy val mapGetOrElse = fun { p:Rep[(Int,Double)] =>
    val m = MMap.empty[Int,Double]
    val m1 = m.update(p._1, p._2) | m
    m1.getOrElse(p._1, p._2)
  }

  lazy val mapAsSet = fun {in:Rep[Int] =>
    val m = MMap.empty[Int,Unit]
    val m1 = m.update(in,()) | m
    m1.contains(in)
  }
  lazy val unionMaps = fun { in: Rep[(Array[(Int, Double)], Array[(Int, Double)])] =>
    val map1 = MMap.fromArray[Int, Double](in._1)
    val map2 = MMap.fromArray[Int, Double](in._2)
    map1.union(map2).toArray.sort
  }
  lazy val differenceMaps = fun { in: Rep[(Array[(Int, Double)], Array[(Int, Double)])] =>
    val map1 = MMap.fromArray[Int, Double](in._1)
    val map2 = MMap.fromArray[Int, Double](in._2)
    map1.difference(map2).toArray.sort
  }
  lazy val joinMaps = fun { in: Rep[(Array[(Int, Double)], Array[(Int, Double)])] =>
    val map1 = MMap.fromArray[Int, Double](in._1)
    val map2 = MMap.fromArray[Int, Double](in._2)
    map1.join(map2).toArray.sort
  }
  lazy val reduceMaps = fun { in: Rep[(Array[(Int, Double)], Array[(Int, Double)])] =>
    val map1 = MMap.fromArray[Int, Double](in._1)
    val map2 = MMap.fromArray[Int, Double](in._2)
    map1.reduce(map2, fun2 { (a, b) => a + b}).toArray.sort
  }
  lazy val iterateMap = fun { in: Rep[Array[(Int, Double)]] =>
    val map = MMap.fromArray[Int, Double](in)
    loopUntil2(1, 0.0)(
    { (i, sum) => (!map.contains(i) && i > map.size)}, { (i, sum) => (i + 1, sum + map(i))}
    )
  }
  lazy val mapReduceByKey = fun { in: Rep[Array[Int]] =>
    in.mapReduce[Int, Int](a => (a, toRep(1)), (s1, s2) => s1 + s2).toArray.sort
  }
  lazy val compoundMapKey = fun { in: Rep[(Array[(Int, Double)], Array[Int])] =>
    val map = MMap.fromArray[(Int, Double), Int](in._1 zip in._2)
    loopUntil2(0, 0)(
    { (i, sum) => (i >= map.size)}, { (i, sum) => (i + 1, sum + map(in._1(i)))}
    )
  }
  lazy val compoundMapValue = fun { in: Rep[(Array[String], Array[(Int, Double)])] =>
    val map = MMap.fromArray[String, (Int, Double)](in._1 zip in._2)
    map("two")._2
  }
  lazy val groupByCount = fun { in: Rep[Array[(Int, Int)]] =>
    in.groupBy(fun { p => p._1}).mapValues(g => g.length).toArray.sortBy(fun { p => p._1})
  }
  lazy val groupBySum = fun { in: Rep[Array[(Int, Int)]] =>
    in.groupBy(fun { p => p._1}).mapValues(g => g.toArray.map(p => p._2).sum).toArray.sortBy(fun { p => p._1})
  }

}

abstract class MapItTests extends BaseItTests[SimpleMapProg](new ScalanDslStd with SimpleMapProg) {
  test("mapPutContains") {
    compareOutputWithStd(_.mapPutContains)(Tuple2(314,3.14))
  }
  test("mapGetOrElse") {
    compareOutputWithStd(_.mapGetOrElse)(Tuple2(314,3.14))
  }
  test("mapAsSet") {
    compareOutputWithStd(_.mapAsSet)(314)
  }
  test("unionMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithStd(_.unionMaps)(in)
  }
  test("differenceMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithStd(_.differenceMaps)(in)
  }
  test("iterateMap") {
    val in = Array((1, 1.1), (2, 2.2), (3, 3.3))
    compareOutputWithStd(_.iterateMap)(in)
  }
  test("mapReduce") {
    val in = Array(1, 2, 1, 1, 2, 3, 4, 1, 5, 4, 3, 2, 5, 2, 1)
    compareOutputWithStd(_.mapReduceByKey)(in)
  }
  test("joinMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithStd(_.joinMaps)(in)
  }
  test("compoundMapKey") {
    val in = (Array((2, 1.0), (3, 2.0), (1, 3.0), (5, 4.0), (4, 5.0)), Array(1, 2, 3, 4, 5))
    compareOutputWithStd(_.compoundMapKey)(in)
  }
  test("reduceMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithStd(_.reduceMaps)(in)
  }
  test("groupByCount") {
    val in = Array((2, 1), (3, 2), (2, 5), (1, 3), (5, 4), (1, 3), (4, 5), (2, 4))
    compareOutputWithStd(_.groupByCount)(in)
  }
  test("groupBySum") {
    val in = Array((2, 1), (3, 2), (2, 5), (1, 3), (5, 4), (1, 3), (4, 5), (2, 4))
    compareOutputWithStd(_.groupBySum)(in)
  }
  test("compoundMapValue") {
    val in = (Array("one", "two", "three"), Array((1, 1.1), (2, 2.2), (3, 3.3)))
    compareOutputWithStd(_.compoundMapValue)(in)
  }
}