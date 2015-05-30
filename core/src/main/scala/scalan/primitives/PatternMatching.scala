package scalan.primitives

import scala.reflect.macros.blackbox
import scalan._

trait PatternMatching { _: Scalan =>

  def patternMatchError(obj: Any): Nothing

  def MATCH[A, B: Elem](selector: Rep[A])(f: PartialFunction[Rep[A], Rep[B]]): Rep[B] =
    macro PatternMatchingMacro.patternMatchImpl[A, B]

  protected def patternMatch0[A, B: Elem](selector: Rep[A], branches: List[(Class[_], Rep[_ => B])], default: Option[Rep[A => B]]): Rep[B]
}

trait PatternMatchingSeq { _: ScalanSeq =>

  def patternMatchError(obj: Any) = throw new MatchError(obj)

  protected def patternMatch0[A, B: Elem](selector: Rep[A], branches: List[(Class[_], Rep[_ => B])], default: Option[Rep[A => B]]) =
    branches.collectFirst { case (clazz, f) if clazz.isInstance(selector) => f }.
      orElse(default.asInstanceOf[Option[_ => B]]) match {
      case Some(f) => f.asInstanceOf[A => B](selector)
      case None => patternMatchError(selector)
    }
}

trait PatternMatchingExp { _: ScalanExp =>

  def patternMatchError(obj: Any): Nothing = throw new DelayInvokeException

  protected def patternMatch0[A, B: Elem](selector: Rep[A], branches: List[(Class[_], Rep[_ => B])], default: Option[Rep[A => B]]) =
    reifyObject(Match(selector, branches.map(_._2), default))

  case class Match[A, B: Elem](selector: Exp[A], branches: List[Exp[_ => B]], default: Option[Exp[A => B]]) extends BaseDef[B] {
    def mirror(f: Transformer) = Match(f(selector), branches.map(f.apply(_)), default.map(f.apply(_)))
    def uniqueOpId = "match"
  }
}

class PatternMatchingMacro(val c: blackbox.Context) {
  import c.universe._

  def patternMatchImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](selector: c.Tree)(f: c.Tree)(eB: c.Tree): c.Tree = {
    f match {
      case q"{ case ..$cases }" =>
        val fBranches = cases.map { case cq"$pat if $expr => $body" =>
          if (!expr.isEmpty) {
            c.abort(expr.pos, "Guard conditions are not supported in MATCH")
          }
          pat match {
              // TODO check that d is Def here
            //case UnApply(d, List(pat1)) =>
            case pq"$d(..$pats)" =>
              val List(pat1) = pats
              pat1 match {
                  // FIXME
                case pq"$name @ (_: $tpt)" =>
                  (false, q"fun { ${name.toTermName}: Rep[$tpt] => $body }")
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