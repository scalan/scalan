package scalan.primitives

import scala.reflect.macros.blackbox
import scalan._
import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.staged.BaseExp
import scalan.util.ScalaNameUtil

trait PatternMatching { _: Scalan =>

  def patternMatchError(obj: Any): Nothing

  def MATCH[A, B: Elem](selector: Rep[A])(f: PartialFunction[Rep[A], Rep[B]]): Rep[B] =
    macro PatternMatchingMacro.patternMatchImpl[A, B]

  protected def patternMatch[A, B: Elem](selector: Rep[A])(branches: Branch[_ <: A, B]*)(default: Option[Rep[A => B]]): Rep[B]

  case class Branch[A, +B](elem: Elem[A], guard: Rep[A => Boolean], body: Rep[A => B])

  object Branch {
    def apply[A, B](elem: Elem[A], body: Rep[A => B]): Branch[A, B] = Branch(elem, constFun(true)(elem), body)
  }

  // for convenient branch creation
  case class MkBranch[A]()(implicit elem: Elem[A]) {
    // unfortunately can't be used as MkBranch[A] { block } if named apply, so we write
    // MkBranch[A].make { block } instead
    def make[B: Elem](body: Rep[A] => Rep[B]) = Branch(elem, constFun(true)(elem), fun(body))
    def make[B: Elem](guard: Rep[A] => Rep[Boolean])(body: Rep[A] => Rep[B]) = Branch(elem, fun(guard), fun(body))
  }
}

trait PatternMatchingSeq { _: ScalanSeq =>

  def patternMatchError(obj: Any) = throw new MatchError(obj)

  protected def patternMatch[A, B: Elem](selector: Rep[A])(branches: Branch[_ <: A, B]*)(default: Option[Rep[A => B]]) =
    branches.collectFirst {
      case Branch(elem, guard, f) if elem.runtimeClass.isInstance(selector) && guard.asInstanceOf[A => Boolean](selector) => f
    }.orElse(default.asInstanceOf[Option[_ => B]]) match {
      case Some(f) => f.asInstanceOf[A => B](selector)
      case None => patternMatchError(selector)
    }
}

trait PatternMatchingExp extends BaseExp with GraphVizExport { _: ScalanExp =>
  // TODO match branches need to be treated similarly to if branches for code motion etc.

  def patternMatchError(obj: Any): Nothing = throw new DelayInvokeException

  protected def patternMatch[A, B: Elem](selector: Rep[A])(branches: Branch[_ <: A, B]*)(default: Option[Rep[A => B]]) =
    reifyObject(Match[A, B](selector, branches.toList, default))

  case class Match[A, B: Elem](selector: Exp[A], branches: List[Branch[_ <: A, B]], default: Option[Exp[A => B]]) extends BaseDef[B] {
    def mirror(f: Transformer) = Match[A, B](f(selector),
      branches.map { case Branch(elem, guard, body) => Branch(elem, f(guard), f(body)) },
      default.map(f.apply(_)))
  }

  private def eDom(branchBody: Exp[_ => _]) = branchBody.elem.asInstanceOf[FuncElem[_, _]].eDom

  override def rewriteDef[A](d: Def[A]) = d match {
    case Match(selector: Exp[a], branches, defaultOpt) =>
      val selectorClass = selector.elem.runtimeClass
      branches match {
        case Nil => defaultOpt match {
          case Some(default) => default(selector)
          case None => super.rewriteDef(d) // TODO replace with Throw when we have better exceptions/effects support
        }
        // cheap check compared to TypeTag.<:<, we don't care about type arguments
        case Branch(elem, guard, body) :: branchesTail if elem.runtimeClass.isAssignableFrom(selectorClass) =>
          IF (guard(selector)) THEN {
            body(selector).asRep[A]
          } ELSE {
            implicit val eA = d.selfType
            patternMatch(selector)(branchesTail.asInstanceOf[List[Branch[a, A]]]: _*)(defaultOpt.asInstanceOf[Option[Rep[a => A]]])
          }
        case _ if selector.elem.isInstanceOf[ConcreteElem[_, _]] =>
          val possibleBranches = branches.filter { case Branch(elem, _, _) =>
            val branchClass = elem.runtimeClass
            // could be wrong if we match on mix-ins instead of the main hierarchy; currently they aren't supported
            branchClass.isAssignableFrom(selectorClass) || selectorClass.isAssignableFrom(branchClass)
          }
          if (possibleBranches.length < branches.length) {
            implicit val eA = d.selfType
            patternMatch(selector)(possibleBranches.asInstanceOf[List[Branch[a, A]]]: _*)(defaultOpt.asInstanceOf[Option[Rep[a => A]]])
          } else {
            super.rewriteDef(d)
          }
        case _ => super.rewriteDef(d)
      }
    case _ => super.rewriteDef(d)
  }

  override protected def formatDef(d: Def[_])(implicit config: GraphVizConfig) = d match {
    case Match(selector, branches, default) =>
      val branchString = branches.map { case Branch(_, guard, body) =>
        val e = eDom(body)
        val guardString = guard match {
          case Def(ConstantLambda(Def(Const(true)))) => ""
          case _ => s"if $guard"
        }
        // Doesn't quite correspond to Scala code, but should be clear enough
        s"case ${e.name}$guardString => $body"
      }.mkString("; ")
      val defaultString = default match {
        case Some(f) => s"; _ => $f"
        case None => ""
      }
      s"$selector match { $branchString$defaultString }"
    case _ => super.formatDef(d)
  }
}

class PatternMatchingMacro(val c: blackbox.Context) {
  import c.universe._

  def patternMatchImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](selector: c.Tree)(f: c.Tree)(eB: c.Tree): c.Tree = {
    // FIXME implementation is currently incomplete
    f match {
      case q"{ case ..$cases }" =>
        val fBranches = cases.map { case cq"$pat if $expr => $body" =>
          if (!expr.isEmpty) {
            c.abort(expr.pos, "Guard conditions are not supported in MATCH")
          }
          pat match {
              // TODO check that d is Def here
            case UnApply(d, List(pat1)) =>
//            case pq"$d(..$pats)" =>
//              val List(pat1) = pats
              pat1 match {
                  // FIXME
                case pq"$name @ (_: $tpt)" =>
                  (false, q"(classOf[$tpt], fun { ${name.toTermName}: Rep[$tpt] => $body })")
                case pq"$ref(..$pats)" => // TODO (false, )
                  c.abort(expr.pos, "Extractor patterns are not supported in MATCH yet")
              }
            case pq"_" => (true, q"fun { _: Rep[${weakTypeOf[A]}] => $body }")
              // ???
            case pq"$name @ $pat1" => (true, q"fun { ${name.toTermName}: Rep[${weakTypeOf[A]}] => $body }")
            case pq"_: $tpt" => ???
            case pq"$first │ ..$rest" => ???
            case pq"(..$pats)" => c.abort(pat.pos, s"Unexpected tuple pattern:\n$pat\n${pats.mkString("\n")}")
            case _ => c.abort(pat.pos, s"Unexpected pattern:\n$pat")
          }
        }

        val (defaultBranches0, otherBranches0) = fBranches.partition(_._1)
        val defaultTree = defaultBranches0 match {
          case Seq() => q"scala.None"
          case Seq((_, default)) => q"scala.Some($default)"
          case _ => c.abort(f.pos, "MATCH appears to have more than 1 default branch")
        }

        val branches = otherBranches0.map(_._2)
        q"patternMatch0($selector, List(..$branches), $defaultTree)($eB)"
    }
  }
}