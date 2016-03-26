package scalan.collections

import java.io.File

import scalan.{BaseShouldTests, ScalanDsl, ScalanDslExp}
import scalan.compilation.{GraphVizConfig, GraphVizExport}

trait JoinTests extends CollectionsDsl with ScalanDsl {

  lazy val b = (_: Rep[(Int, Double)])._1
  lazy val f1 = (_: Rep[(Int, Double)])._2
  lazy val id = (x: DoubleRep) => x
  lazy val fm = (x: Rep[(Double, Double)]) => x._1 * x._2
  lazy val fa = (x: Rep[(Double, Double)]) => x._1 + x._2
  lazy val fmc = (x: Rep[(((Int, Double), (Int, Double)))]) => x._1._2 * x._3
  lazy val fac = (x: Rep[(((Int, Double), (Int, Double)))]) => x._1._2 + x._3

  def pairedInnerJoin(xys: Rep[(Array[(Int, Double)], Array[(Int, Double)])]) = {
    val Pair(xs, ys) = xys
    val res = PairCollectionAOS(Collection(xs)).innerJoin(PairCollectionAOS(Collection(ys)), fm)
    res.coll.arr
  }

  def pairedOuterJoin(xys: Rep[(Array[(Int, Double)], Array[(Int, Double)])]) = {
    val Pair(xs, ys) = xys
    val res = PairCollectionAOS(Collection(xs)).outerJoin(PairCollectionAOS(Collection(ys)), fa, id, id)
    res.coll.arr
  }

  def commonInnerJoin(xys: Rep[(Array[(Int, Double)], Array[(Int, Double)])]) = {
    val Pair(xs, ys) = xys
    val res = Collection(xs).innerJoin(Collection(ys), b, b, fmc)
    res.arr
  }

  def commonOuterJoin(xys: Rep[(Array[(Int, Double)], Array[(Int, Double)])]) = {
    val Pair(xs, ys) = xys
    val res = Collection(xs).outerJoin(Collection(ys), b, b, fac, f1, f1)
    res.arr
  }

  def commonInnerJoinFull(in: Rep[(Array[(Int, Double)], (Array[(Int, Double)],
                                   (((Int, Double)) => Int,
                                    (((Int, Double)) => Int,
                                     (((Int, Double), (Int, Double))) => Double))))]) = {
    val Tuple(xs, ys, a, b, f) = in
    val res = Collection(xs).innerJoin(Collection(ys), a, b, f)
    res.arr
  }

  def commonOuterJoinFull(in: Rep[(Array[(Int, Double)], (Array[(Int, Double)],
                                   (((Int, Double)) => Int,
                                    (((Int, Double)) => Int,
                                     ((((Int, Double), (Int, Double))) => Double,
                                      ((((Int, Double))) => Double,
                                       (((Int, Double))) => Double))))))]) = {
    val Tuple(xs, ys, a, b, f, f1, f2) = in
    val res = Collection(xs).outerJoin(Collection(ys), a, b, f, f1, f2)
    res.arr
  }

  lazy val innerJoin = fun { pairedInnerJoin }
  lazy val outerJoin = fun { pairedOuterJoin }
  lazy val innerJoinCommon = fun { commonInnerJoin }
  lazy val outerJoinCommon = fun { commonOuterJoin }
  lazy val innerJoinCommonFull = fun { commonInnerJoinFull }
  lazy val outerJoinCommonFull = fun { commonOuterJoinFull }
}

class JoinTestSuite extends BaseShouldTests {

  val b = (_: (Int, Double))._1
  val f1 = (_: (Int, Double))._2
  val fm = (x: (((Int, Double), (Int, Double)))) => x._1._2 * x._2._2
  val fa = (x: (((Int, Double), (Int, Double)))) => x._1._2 + x._2._2

  val xs = Array((1, 2.0), (2, 3.0), (3, 4.0))
  val ys = Array((1, 1.0), (3, 2.0), (5, 3.0))

  object result {
    val innerJoin = Array((1, 2.0), (3, 8.0))
    val outerJoin = Array((1, 3.0), (2, 3.0), (3, 6.0), (5, 3.0))
  }

  def testMethod(name: String) = {
    val ctx = new ScalanDslExp with CollectionsDslExp with JoinTests with GraphVizExport
    val f = ctx.getStagedFunc(name)
    ctx.emitDepGraph(f, new File(prefix, s"$name.dot"))(GraphVizConfig.default)
  }

  val ctx = new CollectionsDslStd with JoinTests

  "in staged context" should "innerJoin" beArgFor { testMethod(_) }
  "in staged context" should "outerJoin" beArgFor { testMethod(_) }
  "in staged context" should "innerJoinCommon" beArgFor { testMethod(_) }
  "in staged context" should "outerJoinCommon" beArgFor { testMethod(_) }
  "in staged context" should "innerJoinCommonFull" beArgFor { testMethod(_) }
  "in staged context" should "outerJoinCommonFull" beArgFor { testMethod(_) }

  "paired innerJoin" should "give correct result" in {
    ctx.innerJoin((xs, ys)) should be(result.innerJoin)
  }
  "paired outerJoin" should "give correct result" in {
    ctx.outerJoin((xs, ys)) should be(result.outerJoin)
  }
  "common innerJoin" should "give correct result" in {
    ctx.innerJoinCommon((xs, ys)) should be(result.innerJoin)
  }
  "common outerJoin" should "give correct result" in {
    ctx.outerJoinCommon((xs, ys)) should be(result.outerJoin)
  }
  "common innerJoin with input functions" should "give correct result" in {
    ctx.innerJoinCommonFull((xs, (ys, (b, (b, fm))))) should be(result.innerJoin)
  }
  "common outerJoin with input functions" should "give correct result" in {
    ctx.outerJoinCommonFull((xs, (ys, (b, (b, (fa, (f1, f1))))))) should be (result.outerJoin)
  }
}
