package scalan.staged

import scalan.common.OverloadHack
import scalan.ScalanStaged
import scala.language.{implicitConversions}

trait Transforming extends OverloadHack { self: ScalanStaged =>

  def mirror[A](d: Def[A], f: Transformer): Exp[_] = d.mirror(f)

  implicit class DefMirroringExtensions[A](d: Def[A]) {
    def mirrorWith[A](newArgs: Exp[_]*) = {
      val args: List[Exp[_]] = dep(d)
      if (newArgs.length != args.length) throw !!!(s"invalid mirroring of $d: different number of args")

      val subst = (args, newArgs).zipped.toMap
      val t = new MapTransformer(subst)
      mirror(d, t)
    }
  }

  class MapTransformer(val subst: Map[Exp[Any], Exp[Any]] = Map()) extends Transformer {
    def apply[A](x: Exp[A]): Exp[A] = subst.get(x) match {
      case Some(y) if y != x => apply(y.asRep[A])  // transitive closure
      case _ => x
    }
    def isDefinedAt(x: Rep[_]) = subst.contains(x)
    def domain: Set[Rep[_]] = subst.keySet
  }

  object MapTransformer {
    implicit val ops: TransformerOps[MapTransformer] = new TransformerOps[MapTransformer] {
      def add(t: MapTransformer, kv: (Rep[_], Rep[_])): MapTransformer =
        new MapTransformer(t.subst + kv)
    }
  }

  case class RuleRewriter[A](rule: RewriteRule[A]) extends Rewriter {
    def apply(x: Exp[_]): Exp[_] = rule.unapply(x) match {
      case Some(args) => rule(args)
      case _ => x
    }
  }
  implicit def toRuleRewriter[A](rule: RewriteRule[A]): Rewriter = new RuleRewriter(rule)

  class PartialRewriter(val pf: PartialFunction[Exp[_], Exp[_]]) extends Rewriter {
    def apply(x: Exp[_]): Exp[_] = pf.isDefinedAt(x) match {
      case true => pf(x)
      case _ => x
    }
  }
  implicit def toPartialRewriter(pf: Exp[_] :=> Exp[_]): Rewriter = new PartialRewriter(pf)

  object DecomposeRewriter extends Rewriter {
    def apply(x: Exp[_]): Exp[_] = x match {
      case Def(d) => d.decompose match {
        case None => x
        case Some(y) => y
      }
      case _ => x
    }
  }



  trait Rewriter extends (Exp[_] => Exp[_]) { self =>

    def orElse(other: Rewriter): Rewriter = (x: Exp[_]) => {
        val y = self(x)
        (x == y) match { case true => other(x) case _ => y }
    }
    def andThen(other: Rewriter): Rewriter = (x: Exp[_]) => {
      val y = self(x)
      val res = other(y)
      res
    }

    def |(other: Rewriter) = orElse(other)
    def ~(other: Rewriter) = andThen(other)

    //def ??(x: Exp[_] => Boolean): Rewriter = (x: Exp[_]) => other(self(x))
  }
  object Rewriter {
    implicit def functionToRewriter(f: Exp[_] => Exp[_]): Rewriter = new Rewriter { def apply(x: Exp[_]) = f(x) }
  }

  val NoRewriting: Rewriter = (x: Exp[_]) => x

  abstract class Mirror[Ctx <: Transformer : TransformerOps] {
    def apply(t: Ctx, rw: Rewriter, x: Exp[_]): (Ctx, Exp[_]) = (t, x.mirror(t))

    // every mirrorXXX methos should return a pair (t + (v -> v1), v1)
    protected def mirrorVar(t: Ctx, rewriter: Rewriter, v: Exp[_]): (Ctx, Exp[_]) = {
      val newVar = fresh(() => v.elem.asElem[Any])
      (t + (v -> newVar), newVar)
    }

    protected def mirrorDef(t: Ctx, rewriter: Rewriter, node: Exp[_], d: Def[_]): (Ctx, Exp[_]) = {
      val (t1, mirrored) = apply(t, rewriter, node)
      var res = mirrored
      var curr = res
      do {
        curr = res
        res = rewriter(curr)
      } while (res != curr)

      (t1 + (node -> res), res)
    }

    protected def getMirroredLambdaSym(node: Exp[_]): Exp[_] = fresh(() => node.elem.asElem[Any])

    // require: should be called after lam.schedule is mirrored
    private def getMirroredLambdaDef(t: Ctx, newLambdaSym: Exp[_], lam: Lambda[_,_]): Lambda[_,_] = {
      val newVar = t(lam.x)
      val newBody = t(lam.y)
      val newLambdaDef = new LambdaWrapper(None, newVar, newBody, newLambdaSym.asRep[Any=>Any])
      newLambdaDef
    }

    protected def mirrorLambda(t: Ctx, rewriter: Rewriter, node: Exp[_], lam: Lambda[_,_]): (Ctx, Exp[_]) = {
      val (t1, newVar) = mirrorNode(t, rewriter, lam.x)
      val newLambdaSym = getMirroredLambdaSym(node)

      lambdaStack.push(newLambdaSym)
      val schedule = lam.schedule map { case TP(sym, d) => sym }
      val (t2, _) = mirrorSymbols(t1, rewriter, schedule)
      lambdaStack.pop

      val newLambda = getMirroredLambdaDef(t2, newLambdaSym, lam)
      createDefinition(newLambdaSym, newLambda)

      (t2 + (node -> newLambdaSym), newLambdaSym)
    }

    protected def isMirrored(t: Ctx, node: Exp[_]): Boolean = t.isDefinedAt(node)

    protected def mirrorNode(t: Ctx, rewriter: Rewriter, node: Exp[_]): (Ctx, Exp[_]) = {
      isMirrored(t, node) match {         // cannot use 'if' because it becomes staged
        case true => (t, t(node))
        case _ =>
          node match {
            case Var(_) =>
              mirrorVar(t, rewriter, node)

            case Def(Lambda(lam, _, _, _)) =>
              mirrorLambda(t, rewriter, node, lam)

            case Def(d) =>
              mirrorDef(t, rewriter, node, d)
          }
      }
    }

    def mirrorSymbols(t0: Ctx, rewriter: Rewriter, nodes: List[Exp[_]]) =
      nodes.foldLeft((t0, Nil: List[Exp[_]])) { case ((t1, nodes), n) => {
        val (t2, n1) = mirrorNode(t1, rewriter, n)
        (t2, nodes ++ List(n1))
      }}

    def mirrorSymbol(startNode: Exp[_], rewriter: Rewriter, t: Ctx): (Ctx, Exp[_]) = {
      val (t1, ss) = mirrorSymbols(t, rewriter, List(startNode))
      (t1, ss.head)
    }
  }

  def mirror[Ctx <: Transformer : TransformerOps] = new Mirror[Ctx] {}
  val DefaultMirror = mirror[MapTransformer]

  object Transformer {
    val Id = new MapTransformer()
  }



}
