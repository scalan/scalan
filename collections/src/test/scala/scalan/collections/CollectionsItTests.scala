package scalan.collections

import scalan._
import scalan.it.BaseItTests

trait CollectionsProg extends CollectionsDsl with CollectionExamples {

  lazy val simpleConst = fun { x: Coll[Int] =>
    Collection.singleton(1)
  }

  lazy val seqsEmpty = fun {ignore: Rep[Int] => SSeq.empty[Int].wrappedValue}

  lazy val seqsSingle = fun {v:Rep[Int] => SSeq.single(v).wrappedValue}

  lazy val seqsFromArray = fun {arr: Rep[Array[Int]] =>
    SSeq(arr).wrappedValue
  }

  lazy val seqsArrayMap = fun {(arr: Rep[Array[Array[Int]]]) =>
    val f = {s: Rep[Array[Int]] => (SSeq(s).+:(10)).wrappedValue}
    arr.map(f)
  }

  def sSeqMap(x: Rep[SSeq[Int]]) = {
    x.map({i: Rep[Int] => i+1})
  }
  lazy val seqsSimpleMap = fun { x: Rep[Seq[Int]] =>
    val seqImp = SSeqImpl(x)
    val res = sSeqMap(seqImp)
    res.wrappedValue
  }
  lazy val expBaseArrays = fun { xss: Arr[Array[Int]] =>
    val pss1: Arr[Collection[Int]] = xss.map { xs: Rep[Array[Int]] => Collection(xs)}
    val res = pss1.map { ps: Coll[Int] =>
      ps.arr
    }
    res
  }

  lazy val makeMap = fun { in: Rep[Array[(Int, Double)]] =>
    val emptyMap = MMap.make[Int, Double]("testMMap")
    val map1 = MMap.fromArray[Int, Double](in)
    emptyMap.join(map1).toArray
  }

  lazy val convertPairCollectionSOA = fun { in: Rep[Array[Array[(Int, Double)]]] =>
    val items = NestedCollectionFlat.fromJuggedArray(in)
    items.map { coll =>
      // FIXME: convertTo does nor work
      val collPair = coll.convertTo[PairCollectionSOA[Int, Double]]
      (coll.as.reduce, coll.bs.reduce)
    }.arr
  }

  lazy val ifSpecialize = fun { in: Rep[Array[Int]] =>

    def trainStep(state: Rep[(Collection[Int], Collection[Int])]) : Rep[(Collection[Int], Collection[Int])] = {
      val Pair(v1, v2) = state
      val cond = v1.arr.reduce
      IF (cond > 0) THEN {
        val v: Rep[Collection[Int]] = Collection(array_replicate(cond,0))
        Pair(v, v2)
      } ELSE {
        Pair(v1, v2)
      }
    }

    def trainStop(in: Rep[(Collection[Int], Collection[Int])] ) = {
      val Pair(v1, v2) = in
      (v1.arr.reduce === 0)
    }

    val v: Rep[Collection[Int]] = Collection(in)
    val start = Pair(v,v)

    val res = from(start).until(trainStop)(trainStep)
    val Tuple(v1, v2) = res

    v1.arr(0) + v2.arr(0)
  }
}

/**
 *  Tests that very simple examples are run correctly
 */
abstract class CollectionsItTests extends BaseItTests[CollectionsProg](new CollectionsDslSeq with CollectionsProg) {

  class ProgCommunityExp extends CollectionsDslExp with JNIExtractorOpsExp with CollectionsProg

  test ("convertPairCollectionSOA")  {
    val in = Array(Array((1,2.0),(2,3.0)), Array((3,4.0), (5,6.0)))
    compareOutputWithSequential(_.convertPairCollectionSOA)(in)
  }

  test("expBaseArrays") {
    val in = Array(Array(2, 3), Array(4, 5))
    compareOutputWithSequential(_.expBaseArrays)(in)
  }

  // TODO
  //  test("test00simpleConst") {
//    val (in, out) = Array(0) -> Array(1)
//    progSeq.simpleConst(progSeq.PArray.fromArray(in)).arr should be(out)
//    checkRun(progSeq, progStaged)(progSeq.simpleConst, progStaged.simpleConst)("00simpleConst", progSeq.PArray.fromArray(in), progSeq.PArray.fromArray(out))
//  }

//
//  @Test def test01simpleMap {
//    val (in, out) = Array(1, 2, 3, 4, 5) -> Array(2, 3, 4, 5, 6)
//    Assert.assertArrayEquals(out, progSeq.simpleMap(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.simpleMap, progStaged.simpleMap)("01simpleMap", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//
//  @Test def test02simpleLoop {
//    val (in, out) = Array(1, 2, 3, 4, 5) -> Array(15)
//    Assert.assertArrayEquals(out, progSeq.simpleLoop(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.simpleLoop, progStaged.simpleLoop)("02simpleLoop", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  @Test def test03simpleFirstElement {
//    val (in, out) = Array(3, 4) -> Array(3)
//    Assert.assertArrayEquals(out, progSeq.simpleFirstElement(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.simpleFirstElement, progStaged.simpleFirstElement)("03simpleFirstElement", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//
//  @Test def test04simpleLoop {
//    val (in, out) = Array(1, 2, 3, 4) -> Array(4)
//    Assert.assertArrayEquals(out, progSeq.simpleLoop2(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.simpleLoop2, progStaged.simpleLoop2)("04simpleLoop", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  // fails since tabulate is not implemented in staging
//  @Test def test05simpleAppend {
//    val (in, out) = Array(1, 2) -> Array(1, 2, 1, 2)
//    Assert.assertArrayEquals(out, progSeq.simpleAppend(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.simpleAppend, progStaged.simpleAppend)("05simpleAppend", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  @Test def test06simpleAppend {
//    val (in, out) = Array(1, 2) -> Array(1, 2, 1, 2)
//    Assert.assertArrayEquals(out, progSeq.simpleAppend2(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.simpleAppend2, progStaged.simpleAppend2)("06simpleAppend", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  @Test def test07simpleId {
//    val (in, out) = Array(1, 2) -> Array(1, 2)
//    Assert.assertArrayEquals(out, progSeq.idPA(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.idPA, progStaged.idPA)("07simpleId", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  @Test def test08curredPlusMap {
//    val (in, out) = Array(1, 2) -> Array(2, 4)
//    Assert.assertArrayEquals(out, progSeq.curredPlusMap(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.curredPlusMap, progStaged.curredPlusMap)("08curredPlusMap", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  @Test def test09filterEven {
//    val (in, out) = Array(1, 2, 3, 4, 5, 6) -> Array(2, 4, 6)
//    Assert.assertArrayEquals(out, progSeq.filterEven(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.filterEven, progStaged.filterEven)("09filterEven", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  // fails - currently signed integers are not supported by runtime
//  @Test def test10filterPositive {
//    val (in, out) = Array(-1, -2, 0, 1, 2, -3, 3) -> Array(1,2,3)
//    Assert.assertArrayEquals(out, progSeq.filterPositive(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.filterPositive, progStaged.filterPositive)("10filterPositive", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  // fails - currently it is impossible to use your own monoid
//  @Test def test11reduceMonoid {
//    val (in, out) = Array(1, 2, 2) -> Array(1)
//    Assert.assertArrayEquals(out, progSeq.div2Reduce(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.div2Reduce, progStaged.div2Reduce)("11div2Reduce", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  @Test def test12for {
//    val (in, out) = Array(1, 2, 3, 4, 5) -> Array(3, 4, 5)
//    Assert.assertArrayEquals(out, progSeq.simpleFor(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.simpleFor, progStaged.simpleFor)("12for", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  @Test def test13reverse {
//    val (in, out) = Array(1, 2, 3, 4, 5) -> Array(5, 4, 3, 2, 1)
//    Assert.assertArrayEquals(out, progSeq.reverse(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.reverse, progStaged.reverse)("13reverse", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  // fails for staging
//  @Test def test14toFromArray {
//    val (in, out) = Array(1, 2, 3, 4, 5) -> Array(1, 2, 3, 4, 5)
//    Assert.assertArrayEquals(out, progSeq.toFromArray(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.toFromArray, progStaged.toFromArray)("14toFromArray", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  @Test def simple15flatMap1 {
//    val (in, out) = Array(1, 2, 3) -> Array(1, 2, 2, 3, 3, 3)
//    Assert.assertArrayEquals(out, progSeq.flatMap1(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.flatMap1, progStaged.flatMap1)("15flatMap", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  // compiled cpp program fails
//  @Test def simple16flatMap2 {
//    val (in, out) = Array(1, 2) -> Array(1, 2, 2, 2, 2)
//    Assert.assertArrayEquals(out, progSeq.flatMap2(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.flatMap2, progStaged.flatMap2)("16flatMap", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  // compiled cpp program fails
//  @Test def simple17flatMap2 {
//    val (in, out) = Array(1, 2, 3) -> Array(1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3)
//    Assert.assertArrayEquals(out, progSeq.flatMap2(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.flatMap2, progStaged.flatMap2)("17flatMap", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  @Test def simple18pairToArray {
//    val (in, out) = intPair -> Array(1, 2)
//    Assert.assertArrayEquals(out, progSeq.pairToArray(in).toArray)
//    checkRun(progSeq, progStaged)(progSeq.pairToArray, progStaged.pairToArray)("18pairToArray", in, progSeq.fromArray(out))
//  }
//
//  @Test def simple19valuesOfNA {
//    val (in, out) = nestedArray1 -> Array(1, 2, 3, 4, 5, 6)
//    Assert.assertArrayEquals(out, progSeq.valuesOfNA(in).toArray)
//    checkRun(progSeq, progStaged)(progSeq.valuesOfNA, progStaged.valuesOfNA)("19valuesOfNA", in, progSeq.fromArray(out))
//  }
//
//  @Test def simple20valuesOfNA2 {
//    val (in, out) = nestedArray3 -> Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
//    Assert.assertArrayEquals(out, progSeq.valuesOfNA2(in).toArray)
//    checkRun(progSeq, progStaged)(valuesOfNA2, progStaged.valuesOfNA2)("20valuesOfNA2", in, progSeq.fromArray(out))
//  }
//
//  @Test def simple21simpleLog {
//    val (in, out) = in1 -> out1
//    Assert.assertEquals(out._1, progSeq.logTest(in)._1, 0.01f)
//    Assert.assertEquals(out._2, progSeq.logTest(in)._2, 0.01f)
//    val stagedOut = testRun(progSeq, progStaged)(logTest, progStaged.logTest)("21simpleLog", in)
//    Assert.assertEquals(out._1, stagedOut._1, 0.01f)
//    Assert.assertEquals(out._2, stagedOut._2, 0.01f)
//  }
//
//  @Test def simple22doubleSwap {
//    val (in, out) = in2 -> out2
//    Assert.assertEquals(out._1, progSeq.doubleSwap(in)._1, 0.01)
//    Assert.assertEquals(out._2, progSeq.doubleSwap(in)._2, 0.01)
//    val stagedOut = testRun(progSeq, progStaged)(doubleSwap, progStaged.doubleSwap)("22doubleSwap", in)
//    Assert.assertEquals(out._1, stagedOut._1, 0.01)
//    Assert.assertEquals(out._2, stagedOut._2, 0.01)
//  }
//
//  @Test def simple23sMatMul {
//    val (in, out) = (smdv, null)
//    progSeq.sMatDVecMul(in)
//    val stagedOut = testRun(progSeq, progStaged)(sMatDVecMul, progStaged.sMatDVecMul)("23matMul", in)
//    //Assert.assertEquals(out._1, stagedOut._1, 0.01)
//    //Assert.assertEquals(out._2, stagedOut._2, 0.01)
//  }
//
//  @Test def simple24fusion {
//    val (in, out) = (pairsOfArrays, null)
//    progSeq.fusion(in)
//    val stagedOut = testRun(progSeq, progStaged)(fusion, progStaged.fusion)("24fusion", in)
//    //Assert.assertEquals(out._1, stagedOut._1, 0.01)
//    //Assert.assertEquals(out._2, stagedOut._2, 0.01)
//  }
//
//  @Test def simple25fusion {
//    val (in, out) = (intArray, null)
//    progSeq.fusion1(in)
//    val stagedOut = testRun(progSeq, progStaged)(fusion1, progStaged.fusion1)("25fusion", in)
//    //Assert.assertEquals(out._1, stagedOut._1, 0.01)
//    //Assert.assertEquals(out._2, stagedOut._2, 0.01)
//  }
//
////  @Test def simple26_expandScaledRanges {
////    val in = Pair(fromArray(Array(10, 20, 30)), 2)
////    val out = Array(20, 21, 40, 41, 60, 61)
////    Assert.assertArrayEquals(out, progSeq.expandScaledRangesFun(in).toArray)
////    checkRun(progSeq, progStaged)(expandScaledRangesFun, progStaged.expandScaledRangesFun)("simple26_expandScaledRanges", in, progSeq.fromArray(out))
////  }

  // override val progSeq: ProgCommunitySeq = new ProgCommunitySeq
}
