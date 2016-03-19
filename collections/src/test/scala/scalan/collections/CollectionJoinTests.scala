package scalan.collections

import java.io.File
import java.lang.reflect.Method

import scalan.{BaseShouldTests, ScalanDsl, ScalanDslExp}
import scalan.compilation.{GraphVizExport, GraphVizConfig}

trait CollectionJoins extends CollectionsDsl with ScalanDsl {

  object internal {
    def int(x: IntRep) = x
    def b(x: Rep[(Int, Double)]) = x._1
    def f(x: Rep[(Double, Double)]) = x._1 * x._2
    def fg(x: Rep[((Int, (Int, Double)))]) = {
      val Tuple(a, k, b) = x
      a.toDouble * b
    }
  }
  import internal._
  lazy val innerJoin = fun { (xys: Rep[(Collection[(Int, Double)], Collection[(Int, Double)])]) =>
    val Pair(xs, ys) = xys
    val pcX = PairCollectionAOS(xs)
    val pcY = PairCollectionAOS(ys)
    pcX.innerJoin(pcY, f _) }
  lazy val innerJoin_generic = fun { (xys: Rep[(Collection[Int], Collection[(Int, Double)])]) =>
    val Pair(xs, ys) = xys
    xs.innerJoin(ys, int _, b _, fg _) }
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
  "when staged" should "innerJoin_generic" beArgFor { testMethod(_) }
}
