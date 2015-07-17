package scalan.primitives

import scalan.staged.BaseExp
import scalan.{Scalan, ScalanSeq, ScalanExp}
import scala.reflect.runtime.universe._

trait RewriteRules { self: Scalan =>
  case class Rewrite[A](lhs: Rep[A], rhs: Rep[A])
  case class RewriteOp[A: Elem]() extends BinOp[A, Rewrite[A]]("==>", (x, y) => Rewrite[A](x, y))

  type RRewrite[A] = Rep[Rewrite[A]]

  case class RewriteElem[A](eA: Elem[A]) extends Element[Rewrite[A]] {
    protected def getDefaultRep = {
      val defA = eA.defaultRepValue
      rewr(defA, defA)(eA)
    }
    lazy val tag = {
      implicit val tag1 = eA.tag
      implicitly[WeakTypeTag[Rewrite[A]]]
    }
    def isEntityType = eA.isEntityType
  }

  implicit def rewrElement[A](implicit eA: Elem[A]): Elem[Rewrite[A]] = RewriteElem[A](eA)

  def rewr[A:Elem](x: Rep[A], y: Rep[A]): Rep[Rewrite[A]] = RewriteOp[A].apply(x, y)

  implicit class PropEqualityOps[A: Elem](x: Rep[A]) {
    def <=>(y: Rep[A]): Rep[Rewrite[A]] = self.rewr(x, y)
  }
}

trait RewriteRulesSeq extends RewriteRules { self: ScalanSeq =>
}

trait RewriteRulesExp extends RewriteRules with BaseExp { self: ScalanExp =>

  def postulate[A:Elem, R](p: Rep[A] => RRewrite[R]): RRewrite[R] = p(fresh[A])
  def postulate[A:Elem, B:Elem, R](p: (Rep[A], Rep[B]) => RRewrite[R]): RRewrite[R] = p(fresh[A], fresh[B])
  def postulate[A:Elem, B:Elem, C:Elem, R](p: (Rep[A], Rep[B], Rep[C]) => RRewrite[R]): RRewrite[R] =
    p(fresh[A], fresh[B], fresh[C])

  override def rewrite[T](s: Exp[T]): Exp[_] = {
    val eT = s.elem
    for (rule <- rewriteRules if rule.eA >:> eT) {
      rule.asInstanceOf[RewriteRule[T]](s) match {
        case Some(s1) => return s1
        case None =>
      }
    }

    super.rewrite(s)
  }

  var rewriteRules = List[RewriteRule[_]]()

  def addRewriteRules(rules: RewriteRule[_]*) {
    rewriteRules ++= rules
  }
  def removeRewriteRules(rules: RewriteRule[_]*) {
    rewriteRules = rewriteRules.diff(rules)
  }

  trait RewriteRule[A] {
    def eA: Elem[A]
    def apply(x: Exp[A]): Option[Exp[_]]
  }

  case class PatternRewriteRule[A](lhs: Rep[A], rhs: Rep[A], eA: Elem[A]) extends RewriteRule[A] {
    val g = new PGraph(rhs)

    def apply(s: Exp[A]) = patternMatch(lhs, s).map { subst =>
      val g1 = g.transform(DefaultMirror, NoRewriting, new MapTransformer(subst))
      g1.roots.head.asRep[A]
    }
  }

  def patternRewriteRule[A](rewrite: RRewrite[A]): PatternRewriteRule[A] =
    rewrite match {
      case Def(ApplyBinOp(_: RewriteOp[_], lhs, rhs)) =>
        PatternRewriteRule(lhs, rhs, rewrite.elem.asInstanceOf[RewriteElem[A]].eA)
    }
}
