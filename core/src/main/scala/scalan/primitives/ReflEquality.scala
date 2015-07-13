package scalan.primitives

import scalan.common.Lazy
import scalan.staged.{ExpInductiveGraphs, BaseExp}
import scalan.{Scalan, ScalanSeq, ScalanExp}
import scala.reflect.runtime.universe._

trait ReflEquality { self: Scalan =>
  case class Refl[A](lhs: Rep[A], rhs: Rep[A])
  case class ReflOp[A: Elem]() extends BinOp[A, Refl[A]]("<=>", (x, y) => Refl[A](x, y))

  type EqLemma[A] = Rep[Refl[A]]

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
}

trait ReflEqualitySeq extends ReflEquality { self: ScalanSeq =>
}

trait ReflEqualityExp extends ReflEquality with BaseExp with ExpInductiveGraphs { self: ScalanExp =>

  def postulate[A:Elem, R](p: Rep[A] => Rep[Refl[R]]): EqLemma[R] = p(fresh[A])
  def postulate[A:Elem, B:Elem, R](p: (Rep[A], Rep[B]) => Rep[Refl[R]]): EqLemma[R] = p(fresh[A], fresh[B])
  def postulate[A:Elem, B:Elem, C:Elem, R](p: (Rep[A], Rep[B], Rep[C]) => Rep[Refl[R]]): EqLemma[R] =
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

    def matchWith[U](s: Exp[U]): Option[(SimilarityResult, graphs.Subst)] = {
      val b = getBisimulatorFor(s)
      val resState = b.genStates(List(s), patternLam.roots).toSeq.last

      resState.kind match {
        case SimilarityFailed => None
        case SimilarityEqual | SimilarityEmbeded =>
          Some((resState.kind, resState.fromSubst)) // mapping between graphs  ig <- patternGraph
      }
    }

  }

  class FuncMatcher[A,B](pattern: Exp[A=>B]) extends PatternMatcher(pattern) {
    import graphs._
    def unapply(f: Exp[A => B]): Option[(SimilarityResult, graphs.Subst)] = matchWith(f.getLambda.y)
  }

  case class LemmaRule[A](lhs: Rep[A], rhs: Rep[A], eA: Elem[A]) extends RewriteRule[A] {
    val g = new PGraph(rhs)

    def apply(s: Exp[A]) = patternMatch(lhs, s).map { subst =>
      val g1 = g.transform(DefaultMirror, NoRewriting, new MapTransformer(subst))
      g1.roots.head.asRep[A]
    }
  }

  def rewriteRuleFromEqLemma[A](lemma: EqLemma[A]): LemmaRule[A] =
    lemma match {
      case Def(ApplyBinOp(_: ReflOp[_], lhs, rhs)) =>
        LemmaRule(lhs, rhs, lemma.elem.asInstanceOf[ReflElem[A]].eA)
    }
}

