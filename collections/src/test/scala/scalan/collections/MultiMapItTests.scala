package scalan.collections

import scalan._
import scalan.it.BaseItTests

trait MultiMapExamples extends MultiMapsDsl {
  lazy val ifTest = fun { in: Rep[(Int, Double)] =>
    val map = MMap.empty[Int, Double]
    IF(map.contains(in._1)) THEN {
      THROW("Key already exists")
    } ELSE {
      map.update(in._1, in._2)
    }
  }

  lazy val unionMultiMaps = fun { in: Rep[(Array[(Int, Double)], Array[(Int, Double)])] =>
    val map1 = MMultiMap.fromArray[Int, Double](in._1)
    val map2 = MMultiMap.fromArray[Int, Double](in._2)
    map1.union(map2).toArray.map(p => (p._1, p._2.toArray.sum)).sortBy(fun { p => p._1})
  }

  lazy val appendMultiMap = fun { in: Rep[Array[(Int, Double)]] =>
    val map = MMultiMap.make[Int, Double]("testMultiMap")
    val array = in.fold(map, (state: Rep[MMultiMap[Int, Double]], x: Rep[(Int, Double)]) => state.add(x._1, x._2)).toArray
    array.map(p => (p._1, p._2.toArray.sum)).sortBy(fun { p => p._1})
  }
}

abstract class MultiMapItTests extends BaseItTests[MultiMapExamples](new MultiMapsDslSeq with MultiMapExamples) {

  class MultiMapExamplesExp extends MultiMapsDslExp with JNIExtractorOpsExp with MultiMapExamples

  test("unionMultiMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (1, 3.3), (1, 4.4), (2, 5.5)), Array((0, 0.0), (2, 2.0), (1, 4.0), (1, 6.0)))
    compareOutputWithSequential(_.unionMultiMaps)(in)
  }
  test("appendMultiMap") {
    val in = Array((1, 1.1), (2, 2.2), (1, 3.3), (1, 4.4), (2, 5.5))
    compareOutputWithSequential(_.appendMultiMap)(in)
  }

}
