package scalan
package staged

import scala.language.reflectiveCalls

class RewritingTests extends BaseTests {

  trait Prog extends ScalanExp {

    lazy val mkRight = fun { x: Rep[Int] => toRightSum[Int, Int](x) }

    override def rewriteDef[T](d: Def[T]): Exp[_] = d match {
        // rewrite fun(x => Right) to fun(x => Left(x))
      case Lambda(lam: Lambda[a, |[_, b]] @unchecked, _, _, Def(r @ Right(_))) =>
        implicit val eA = r.eA.asInstanceOf[Elem[b]]
        fun { x: Rep[b] => toLeftSum[b, b](x) }

      case x =>
        super.rewriteDef(d)
    }
  }

  val p = new TestContext(this, "rewriting") with Prog with compilation.Passes {
    def testMkRight = {
      val g0 = new PGraph(mkRight)
      val pass = GraphTransformPass("testTransformPass", DefaultMirror, NoRewriting)
      val g = pass.apply(g0)
      val newLambda = g.roots.head
      val Def(Lambda(lam, f, x, Def(Left(l)))) = newLambda
      assert(x == l)
      newLambda
    }
  }

  test("rewrite Lambda") {
    val mkLeft = p.testMkRight
    p.emit("mkRight", p.mkRight)
    p.emit("mkRight'", mkLeft)
  }
}
