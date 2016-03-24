package scalan.collections

import java.io.File
import java.lang.reflect.Method

import scalan.{BaseShouldTests, ScalanDsl, ScalanDslExp}
import scalan.compilation.{GraphVizExport, GraphVizConfig}

trait CollectionJoins extends CollectionsDsl with ScalanDsl {

   lazy val b = (_: Rep[(Int, Double)])._1
   lazy val f1 = (_: Rep[(Int, Double)])._2
   lazy val f = (x: Rep[(Double, Double)]) => x._1 * x._2
   lazy val fg = (x: Rep[(((Int, Double), (Int, Double)))]) => x._1._2 * x._3

  lazy val innerJoin = fun { (xys: Rep[(Array[(Int, Double)], Array[(Int, Double)])]) =>
    val Pair(xs, ys) = xys
    val res = PairCollectionAOS(Collection(xs)).innerJoin(PairCollectionAOS(Collection(ys)), f)
    res.coll.arr
  }
  lazy val outerJoin = fun { (xys: Rep[(Array[(Int, Double)], Array[(Int, Double)])]) =>
    val Pair(xs, ys) = xys
    val res = PairCollectionAOS(Collection(xs)).innerJoin(PairCollectionAOS(Collection(ys)), f)
    res.coll.arr
  }
  lazy val innerJoinGeneric = fun { (xys: Rep[(Array[(Int, Double)], Array[(Int, Double)])]) =>
    val Pair(xs, ys) = xys
    val res = Collection(xs).innerJoin(Collection(ys), b, b, fg)
    res.arr
  }
  lazy val outerJoinGeneric = fun { (xys: Rep[(Array[(Int, Double)], Array[(Int, Double)])]) =>
    val Pair(xs, ys) = xys
    val res = Collection(xs).outerJoin(Collection(ys), b, b, fg, f1, f1)
    res.arr
  }
  lazy val innerJoinGenFull = fun { (in: Rep[(Array[(Int, Double)], (Array[(Int, Double)],
                                             (((Int, Double)) => Int,
                                               (((Int, Double)) => Int,
                                                 (((Int, Double), (Int, Double))) => Double))))]) =>
    val Tuple(xs, ys, a, b, f) = in
    val res = Collection(xs).innerJoin(Collection(ys), b, b, f)
    res.arr
  }
  lazy val outerJoinGenFull = fun { (in: Rep[(Array[(Int, Double)], (Array[(Int, Double)],
                                             (((Int, Double)) => Int,
                                               (((Int, Double)) => Int,
                                                 ((((Int, Double), (Int, Double))) => Double,
                                                   ((((Int, Double))) => Double,
                                                     (((Int, Double))) => Double))))))]) =>
    val Tuple(xs, ys, a, b, f, f1, f2) = in
    val res = Collection(xs).outerJoin(Collection(ys), a, b, f, f1, f2)
    res.arr
  }
}

class CollectionJoinTests extends BaseShouldTests {

  def testMethod(name: String) = {
    val ctx = new ScalanDslExp with CollectionsDslExp with CollectionJoins with GraphVizExport {
      override def isInvokeEnabled(d: Def[_], m: Method) = true //HACK: invoke all domain methods if possible //TODO this is not how it should be specified
    }
    val f = ctx.getStagedFunc(name)
    ctx.emitDepGraph(f, new File(prefix, s"$name.dot"))(GraphVizConfig.default)
  }

  "when staged" should "innerJoin" beArgFor { testMethod(_) }
  "when staged" should "innerJoinGeneric" beArgFor { testMethod(_) }
  "when staged" should "innerJoinGenFull" beArgFor { testMethod(_) }
  "when staged" should "outerJoin" beArgFor { testMethod(_) }
  "when staged" should "outerJoinGeneric" beArgFor { testMethod(_) }
  "when staged" should "outerJoinGenFull" beArgFor { testMethod(_) }
}
