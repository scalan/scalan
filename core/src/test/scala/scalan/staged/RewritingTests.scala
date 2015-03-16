package scalan
package staged

import scala.language.reflectiveCalls
import scalan.compilation.Passes

class RewritingTests extends BaseTests {

  trait TestTransform { self: ScalanExp with Passes =>
    val pass = GraphTransformPass("testTransformPass", DefaultMirror, NoRewriting)

    def doTransform[A](e: Exp[A]): Exp[_] = {
      val g0 = new PGraph(e)
      val g = pass.apply(g0)
      g.roots.head
    }
  }

  trait Prog0 extends ScalanExp {

    lazy val mkRight = fun { x: Rep[Int] => toRightSum[Int, Int](x) }

    override def rewriteDef[T](d: Def[T]): Exp[_] = d match {
        // rewrite fun(x => Right) to fun(x => Left(x))
      case Lambda(lam: Lambda[a, |[_, b]] @unchecked, _, _, Def(r @ Right(_))) =>
        implicit val eA = r.eA.asInstanceOf[Elem[b]]
        fun { x: Rep[b] => toLeftSum[b, b](x) }

      case _ =>
        super.rewriteDef(d)
    }
  }

  val p0 = new TestContext(this, "RewritingRootLambda") with Prog0 with Passes with TestTransform {
    def testMkRight(): Unit = {
      emit("mkRight", mkRight)
      val newLambda = doTransform(mkRight)
      emit("mkRight'", newLambda)

      val Def(Lambda(_, _, x, Def(Left(l)))) = newLambda
      assert(x == l)
    }
  }

  trait Prog extends ScalanExp {
    lazy val ifFold = fun { pp: Rep[Boolean] =>
      val e1 = toLeftSum[Double, Int](1d)
      val e2 = toRightSum[Double, Int](2)
      val iff = __ifThenElse(pp, e1, e2)
      iff.fold(_ => 10, _ => 100)
    }

    lazy val ifIfFold = fun2 { (p1: Rep[Boolean], p2: Rep[Boolean]) =>
      val e1 = toLeftSum[Double, Int](1d)
      val e2 = toRightSum[Double, Int](2)
      val e3 = toRightSum[Double, Int](3)
      val iff = __ifThenElse(
        p1,
        __ifThenElse(p2, e1, e2),
        e3)
      iff.fold(_ => 10, _ => 100)
    }
  }

  val p = new TestContext(this, "RewritingRules") with Prog with Passes with TestTransform {

    def testIfFold(): Unit = {
      emit("ifFold", ifFold)
      val Def(Lambda(_, _, _, y)) = ifFold
      val Def(IfThenElse(cond, Def(Const(10)), Def(Const(100)))) = y
    }

    def testIfIfFold(): Unit = {
      emit("ifIfFold", ifIfFold)
      val Def(Lambda(_, _, _, y)) = ifIfFold
      val Def(IfThenElse(c1, Def(IfThenElse(c2, Def(Const(10)), Def(Const(100)))), Def(Const(100)))) = y
    }
  }

  test("root Lambda")(p0.testMkRight)

  test("SumFold(IfThenElse(p, t, e)) -> IfThenElse(p, SumFold, SumFold)")(p.testIfFold)
  test("SumFold(IfThenElse(p, IfThenElse, e) -> IfThenElse(p, IfThenElse(p, SumFold, SumFold), SumFold)")(p.testIfIfFold)
}
