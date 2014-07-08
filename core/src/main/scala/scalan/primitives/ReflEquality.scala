package scalan.primitives

import scalan.common.Lazy
import scalan.staged.BaseExp
import scalan.{Scalan, ScalanSeq, ScalanExp}
import scala.reflect.runtime.universe._

trait ReflEquality { self: Scalan =>
  case class Refl[A](lhs: Rep[A], rhs: Rep[A])
  case class ReflOp[A: Elem]() extends BinOp[A, Refl[A]]("<=>", (x, y) => Refl[A](x, y))

  type EqLemma[A,B] = Rep[A => Refl[B]]

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

  def postulate[A:Elem,R:Elem](p: Rep[A] => Rep[Refl[R]]): EqLemma[A,R] = fun(p)
  def postulate[A:Elem,B:Elem,R:Elem](p: (Rep[A], Rep[B]) => Rep[Refl[R]]): EqLemma[(A,B),R] = fun{(x: Rep[(A,B)]) => p(x._1, x._2)}
  def postulate[A:Elem,B:Elem,C:Elem,R:Elem](p: (Rep[A], Rep[B], Rep[C]) => Rep[Refl[R]]): EqLemma[(A,(B,C)),R] = fun{(x: Rep[(A,(B,C))]) => p(x._1, x._2, x._3)}
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

  case class LemmaRule[A,B](lemma: Lambda[A,Refl[B]], pattern: Exp[A=>B], rhs: Exp[A=>B]) extends RewriteRule[A,B] {
    lazy val patternGraph: ExpGraph = {
      val Def(lam: Lambda[A,B]) = pattern
      lam.indGraph
    }

    def apply(x: Exp[B]) = ???
  }

  def rewriteRuleFromEqLemma[A,B](lemma: EqLemma[A,B]): LemmaRule[A,B] = {
    val Def(lam: Lambda[A,Refl[B]]) = lemma
    lam.y match {
      case Def(ApplyBinOp(_: ReflOp[_], lhs, rhs)) =>
        implicit val eA = lam.x.elem
        implicit val eB = rhs.elem.asElem[B]
        implicit val leFun = Lazy(element[A => B])

        val lLamSym = fresh[A => B]
        val rLamSym = fresh[A => B]
        val lLam: Exp[A => B] = new Lambda(None, lam.x, lhs, lLamSym, false)
        val rLam: Exp[A => B] = new Lambda(None, lam.x, rhs, rLamSym, false)

        LemmaRule(lam, lLam, rLam /*fun { (a: Rep[A]) =>  rLam(a) }*/)
    }
  }
}

