package scalan.primitives

import scalan.staged.{BaseExp}
import scalan.{Scalan, ScalanSeq, ScalanExp}
import scala.reflect.runtime.universe._

trait ReflEquality { self: Scalan =>
  case class Refl[A](lhs: Rep[A], rhs: Rep[A])
  case class ReflOp[A: Elem]() extends BinOp[A, Refl[A]]("<=>", (x, y) => Refl[A](x, y))

  type Lemma[A,B] = Rep[A => Refl[B]]

  case class ReflElem[A](eA: Elem[A]) extends Element[Refl[A]] {
    protected def getDefaultRep = {
      val defA = eA.defaultRepValue
      refl(defA, defA)(eA)
    }
    lazy val tag = {
      implicit val tag1 = eA.tag
      implicitly[WeakTypeTag[Refl[A]]]
    }
    def isEntityType = eA.isEntityType
  }

  implicit def reflElement[A](implicit eA: Elem[A]): Elem[Refl[A]] = ReflElem[A](eA)

  def refl[A:Elem](x: Rep[A], y: Rep[A]): Rep[Refl[A]] = ReflOp[A].apply(x, y)

  implicit class PropEqualityOps[A: Elem](x: Rep[A]) {
    def <=>(y: Rep[A]): Rep[Refl[A]] = self.refl(x, y)
  }

  def postulate[A:Elem,B:Elem](p: Rep[A] => Rep[Refl[B]]): Lemma[A,B] = fun(p)
}

trait ReflEqualitySeq extends ReflEquality  { self: ScalanSeq =>
}

trait ReflEqualityExp extends ReflEquality with BaseExp { self: ScalanExp =>
  var rewriteRules = List[RewriteRule[Any,Any]]()

  def addRewriteRules(rules: RewriteRule[Any,Any]*) {
    rewriteRules ++= rules
  }

  trait RewriteRule[A,B] {
    def apply(x: Exp[B]): Option[Exp[B]]
  }

  case class LemmaRule[A,B](lemma: Lemma[A,B]) extends RewriteRule[A,B] {
    def apply(x: Exp[B]) = ???
  }

  def rewriteRuleFromLemma[A,B](l: Lemma[A,B]): RewriteRule[A,B] = {

    LemmaRule(l)
  }
}

