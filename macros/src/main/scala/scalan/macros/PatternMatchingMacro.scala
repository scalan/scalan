package scalan.macros

import scala.reflect.macros.blackbox

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
