package scalan.primitives

import scalan.common.Lazy
import scalan.staged.{ExpInductiveGraphs, BaseExp}
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

trait ReflEqualityExp extends ReflEquality with BaseExp with ExpInductiveGraphs { self: ScalanExp =>

  override def rewrite[T](s: Exp[T]): Exp[_] = {
    for (rule <- rewriteRules) {
      rule(s) match {
        case Some(s1) => return s1
        case None =>
      }
    }

    super.rewrite(s)
  }

  var rewriteRules = List[RewriteRule[Any]]()

  def addRewriteRules(rules: RewriteRule[Any]*) {
    rewriteRules ++= rules
  }
  def removeRewriteRules(rules: RewriteRule[Any]*) {
    rewriteRules = rewriteRules.diff(rules)
  }

  trait RewriteRule[+T] {
    def apply[U >: T](x: Exp[U]): Option[Exp[T]]
  }

  class PatternMatcher[A,B](val pattern: Exp[A=>B]) {
    import graphs._

    val patternLam = pattern.getLambda
    lazy val patternGraph: ExpGraph = patternLam.indGraph

    def nodeCompare(x: Exp[_], y: Exp[_]): Boolean = (x, y) match {
      case (Def(dx), Def(dy)) =>
        val idx = dx.uniqueOpId
        val idy = dy.uniqueOpId
        //println(s"$idx == $idy")
        idx == idy
      case _ => false
    }

    def isVar(s: Exp[_]): Boolean = {
      patternLam.isLambdaBoundProjection(s)
    }

    def getBisimulatorFor[U](s: Exp[U]) = {
      val g = new PGraph(s)
      val ig = g.indGraph
      val b = new Bisimulator[Unit, Unit](ig, patternGraph, defaultContextCompare(nodeCompare, isVar))
      b
    }

    def matchWith[U](s: Exp[U]): Option[(SimilarityResult, Subst)] = {
      val b = getBisimulatorFor(s)
      val resState = b.genStates(List(s), patternLam.roots).toSeq.last

      resState.kind match {
        case SimilarityFailed => None
        case SimilarityEqual | SimilarityEmbeded =>
          Some(resState.kind, resState.fromSubst) // mapping between graphs  ig <- patternGraph
      }
    }

  }

  class FuncMatcher[A,B](pattern: Exp[A=>B]) extends PatternMatcher(pattern) {
    import graphs._
    def unapply(f: Exp[A => B]): Option[(SimilarityResult, Subst)] = matchWith(f.getLambda.y)
  }

  case class LemmaRule[A,B](lemma: Lambda[A,Refl[B]], override val pattern: Exp[A=>B], rhs: Exp[A=>B])
      extends PatternMatcher[A,B](pattern) with RewriteRule[B]
  {
    def apply[U >: B](s: Exp[U]) = matchWith(s).map { case (res, subst) =>
      val tree = patternLam.projectionTreeFrom(patternLam.x)
      val argTup = TupleTree.fromProjectionTree(tree, s => subst(s))
      val arg = argTup.root.asRep[A]
      val x1 = rhs(arg)
      x1
    }
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

        LemmaRule(lam, lLam, rLam)
    }
  }
}

