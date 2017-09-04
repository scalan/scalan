package scalan.primitives

import scalan.Base
import scalan.{Scalan}
import scala.reflect.runtime.universe._
import scalan.util.Invariant

trait RewriteRules extends Base { self: Scalan =>
  case class Rewrite[A](lhs: Rep[A], rhs: Rep[A])
  case class RewriteOp[A: Elem]() extends BinOp[A, Rewrite[A]]("==>", (x, y) => Rewrite[A](x, y))

  type RRewrite[A] = Rep[Rewrite[A]]

  case class RewriteElem[A](eA: Elem[A]) extends Elem[Rewrite[A]] {
    protected def getDefaultRep = {
      val defA = eA.defaultRepValue
      mkRewrite(defA, defA)
    }
    lazy val tag = {
      implicit val tag1 = eA.tag
      implicitly[WeakTypeTag[Rewrite[A]]]
    }
    lazy val typeArgs = TypeArgs("A" -> (eA -> Invariant))
  }

  implicit def rewriteElement[A](implicit eA: Elem[A]): Elem[Rewrite[A]] =
    cachedElem[RewriteElem[A]](eA)

  def mkRewrite[A](x: Rep[A], y: Rep[A]): Rep[Rewrite[A]] = RewriteOp[A]()(x.elem).apply(x, y)

  implicit class PropEqualityOps[A](x: Rep[A]) {
    def <=>(y: Rep[A]): Rep[Rewrite[A]] = self.mkRewrite(x, y)
  }

  def postulate[A:Elem, R](p: Rep[A] => RRewrite[R]): RRewrite[R] = p(fresh[A])
  def postulate[A:Elem, B:Elem, R](p: (Rep[A], Rep[B]) => RRewrite[R]): RRewrite[R] = p(fresh[A], fresh[B])
  def postulate[A:Elem, B:Elem, C:Elem, R](p: (Rep[A], Rep[B], Rep[C]) => RRewrite[R]): RRewrite[R] =
    p(fresh[A], fresh[B], fresh[C])

  override def rewrite[T](s: Exp[T]): Exp[_] = {
    val result = rewriteWithRules(rewriteRules)(s)
    result.getOrElse {
      super.rewrite(s)
    }
  }

  def rewriteWithRules[T](rules: List[RewriteRule[_]])(s: Exp[T]): Option[Exp[_]] = {
    val eT = s.elem
    val iterator = rules.iterator
    var result: Option[Exp[_]] = None
    while (iterator.hasNext && result.isEmpty) {
      val rule = iterator.next()
      if (rule.eA >:> eT)
        result = rule.asInstanceOf[RewriteRule[T]](s)
    }
    result
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

  class RulesRewriter(rules: List[RewriteRule[_]]) extends Rewriter {
    def apply[T](x: Exp[T]) = {
      rewriteWithRules(rules)(x) match {
        case Some(y) => y.asRep[T]
        case None => x
      }
    }
  }
}
