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

  object extensions {
    def innerMult1(in: Rep[(Array[(Int, Double)], Array[(Int, Double)])]) = {
      val Tuple(xs, ys) = in
      val (xPColl, yPColl) = (PairCollectionAOS(CollectionOverArray(xs)), PairCollectionAOS(CollectionOverArray(ys)))
      xPColl.innerMult(yPColl).arr
    }
    def outerSum1(in: Rep[(Array[(Int, Double)], Array[(Int, Double)])]) = {
      val Tuple(xs, ys) = in
      val (xPColl, yPColl) = (PairCollectionAOS(CollectionOverArray(xs)), PairCollectionAOS(CollectionOverArray(ys)))
      xPColl.outerSum(yPColl).arr
    }
    def outerSubtr1(in: Rep[(Array[(Int, Double)], Array[(Int, Double)])]) = {
      val Tuple(xs, ys) = in
      val (xPColl, yPColl) = (PairCollectionAOS(CollectionOverArray(xs)), PairCollectionAOS(CollectionOverArray(ys)))
      xPColl.outerSubtr(yPColl).arr
    }
    def innerMult2(in: Rep[(Array[(Int, Double)], Array[(Int, Double)])]) = {
      val Tuple(xs, ys) = in
      val (xColl, yColl) = (CollectionOverArray(xs), CollectionOverArray(ys))
      xColl.innerMult(yColl).arr
    }
    def outerSum2(in: Rep[(Array[(Int, Double)], Array[(Int, Double)])]) = {
      val Tuple(xs, ys) = in
      val (xColl, yColl) = (CollectionOverArray(xs), CollectionOverArray(ys))
      xColl.outerSum(yColl).arr
    }
    def outerSubtr2(in: Rep[(Array[(Int, Double)], Array[(Int, Double)])]) = {
      val Tuple(xs, ys) = in
      val (xColl, yColl) = (CollectionOverArray(xs), CollectionOverArray(ys))
      xColl.outerSubtr(yColl).arr
    }
    def innerMult3(in: Rep[(Array[(Int, Double)], Array[(Int, Double)])]) = {
      val Tuple(xs, ys) = in
      val (xPColl, yColl) = (PairCollectionAOS(CollectionOverArray(xs)), CollectionOverArray(ys))
      xPColl.innerMult(yColl).arr
    }
    def outerSum3(in: Rep[(Array[(Int, Double)], Array[(Int, Double)])]) = {
      val Tuple(xs, ys) = in
      val (xPColl, yColl) = (PairCollectionAOS(CollectionOverArray(xs)), CollectionOverArray(ys))
      xPColl.outerSum(yColl).arr
    }
    def outerSubtr3(in: Rep[(Array[(Int, Double)], Array[(Int, Double)])]) = {
      val Tuple(xs, ys) = in
      val (xPColl, yColl) = (PairCollectionAOS(CollectionOverArray(xs)), CollectionOverArray(ys))
      xPColl.outerSubtr(yColl).arr
    }
    def innerMult4(in: Rep[(Array[(Int, Double)], Array[(Int, Double)])]) = {
      val Tuple(xs, ys) = in
      val (xColl, yPColl) = (CollectionOverArray(xs), PairCollectionAOS(CollectionOverArray(ys)))
      xColl.innerMult(yPColl).arr
    }
    def outerSum4(in: Rep[(Array[(Int, Double)], Array[(Int, Double)])]) = {
      val Tuple(xs, ys) = in
      val (xColl, yPColl) = (CollectionOverArray(xs), PairCollectionAOS(CollectionOverArray(ys)))
      xColl.outerSum(yPColl).arr
    }
    def outerSubtr4(in: Rep[(Array[(Int, Double)], Array[(Int, Double)])]) = {
      val Tuple(xs, ys) = in
      val (xColl, yPColl) = (CollectionOverArray(xs), PairCollectionAOS(CollectionOverArray(ys)))
      xColl.outerSubtr(yPColl).arr
    }
  }

  object joins {
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
  }

  lazy val pairedInnerJoin = fun { joins.pairedInnerJoin }
  lazy val pairedOuterJoin = fun { joins.pairedOuterJoin }
  lazy val commonInnerJoin = fun { joins.commonInnerJoin }
  lazy val commonOuterJoin = fun { joins.commonOuterJoin }
  lazy val commonInnerJoinFull = fun { joins.commonInnerJoinFull }
  lazy val commonOuterJoinFull = fun { joins.commonOuterJoinFull }

  lazy val innerMult1 = fun { extensions.innerMult1 }
  lazy val innerMult2 = fun { extensions.innerMult2 }
  lazy val innerMult3 = fun { extensions.innerMult3 }
  lazy val innerMult4 = fun { extensions.innerMult4 }
  lazy val outerSum1 = fun { extensions.outerSum1 }
  lazy val outerSum2 = fun { extensions.outerSum2 }
  lazy val outerSum3 = fun { extensions.outerSum3 }
  lazy val outerSum4 = fun { extensions.outerSum4 }
  lazy val outerSubtr1 = fun { extensions.outerSubtr1 }
  lazy val outerSubtr2 = fun { extensions.outerSubtr2 }
  lazy val outerSubtr3 = fun { extensions.outerSubtr3 }
  lazy val outerSubtr4 = fun { extensions.outerSubtr4 }
}

class JoinTestSuite extends BaseShouldTests {

  val b = (_: (Int, Double))._1
  val f1 = (_: (Int, Double))._2
  val fm = (x: (((Int, Double), (Int, Double)))) => x._1._2 * x._2._2
  val fa = (x: (((Int, Double), (Int, Double)))) => x._1._2 + x._2._2

  val xs = Array((1, 2.0), (2, 3.0), (3, 4.0))
  val ys = Array((1, 1.0), (3, 2.0), (5, 3.0))

  val ctx = new CollectionsDslStd with JoinTests
  
  object result {
    val innerJoin = Array((1, 2.0), (3, 8.0))
    val outerJoin = Array((1, 3.0), (2, 3.0), (3, 6.0), (5, 3.0))
    val innerMult = innerJoin
    val outerSum = outerJoin
    val outerSubtr = Array((1, 1.0), (2, 3.0), (3, 2.0), (5, -3.0))
  }

  def testMethod(name: String) = {
    val ctx = new ScalanDslExp with CollectionsDslExp with JoinTests with GraphVizExport
    val f = ctx.getStagedFunc(name)
    ctx.emitDepGraph(f, new File(prefix, s"$name.dot"))(GraphVizConfig.default)
  }

  "in staged context" should "pairedInnerJoin" beArgFor { testMethod(_) }
  "in staged context" should "pairedOuterJoin" beArgFor { testMethod(_) }
  "in staged context" should "commonInnerJoin" beArgFor { testMethod(_) }
  "in staged context" should "commonOuterJoin" beArgFor { testMethod(_) }
  "in staged context" should "commonInnerJoinFull" beArgFor { testMethod(_) }
  "in staged context" should "commonOuterJoinFull" beArgFor { testMethod(_) }

  "join functions" should "give correct results" in {
    ctx.pairedInnerJoin((xs, ys)) should be(result.innerJoin)
    ctx.pairedOuterJoin((xs, ys)) should be(result.outerJoin)
    ctx.commonInnerJoin((xs, ys)) should be(result.innerJoin)
    ctx.commonOuterJoin((xs, ys)) should be(result.outerJoin)
    ctx.commonInnerJoinFull((xs, (ys, (b, (b, fm))))) should be(result.innerJoin)
    ctx.commonOuterJoinFull((xs, (ys, (b, (b, (fa, (f1, f1))))))) should be (result.outerJoin)
  }
  
  "extensions methods" should "give correct results" in {
    import ctx.extensions._
    innerMult1((xs, ys)) should be(result.innerMult)
    outerSum1((xs, ys)) should be(result.outerSum)
    outerSubtr1((xs, ys)) should be(result.outerSubtr)
    innerMult2((xs, ys)) should be(result.innerMult)
    outerSum2((xs, ys)) should be(result.outerSum)
    outerSubtr2((xs, ys)) should be(result.outerSubtr)
    innerMult3((xs, ys)) should be(result.innerMult)
    outerSum3((xs, ys)) should be(result.outerSum)
    outerSubtr3((xs, ys)) should be(result.outerSubtr)
    innerMult4((xs, ys)) should be(result.innerMult)
    outerSum4((xs, ys)) should be(result.outerSum)
    outerSubtr4((xs, ys)) should be(result.outerSubtr)
  }
}
