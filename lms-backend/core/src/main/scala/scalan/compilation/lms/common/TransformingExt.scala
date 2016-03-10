package scalan.compilation.lms.common

import scala.lms.internal.Transforming
import scalan.compilation.language.Adjusted

trait TransformingExt extends Transforming {
  def transformAny(f: Transformer, x: Any): Any = x match {
    case e: Exp[_] => f(e)
    case s: Seq[_] => transformAny(f, s)
      // case b: Block[_] // no manifest available?
    case a: Adjusted[_] => transformAny(f, a)
    case m: Manifest[_] => m
    // default case?
  }

  // extracted for better types
  def transformAny(f: Transformer, e: Exp[_]): Exp[_] = f(e)
  def transformAny(f: Transformer, s: Seq[Any]): Seq[Any] = s.map(transformAny(f, _))
  def transformAny(f: Transformer, s: List[Any]): List[Any] = s.map(transformAny(f, _))
  def transformAny(f: Transformer, s: List[Adjusted[Any]])(implicit d: DummyImplicit): List[Adjusted[Any]] = s.map(transformAny(f, _))
  def transformAny(f: Transformer, a: Adjusted[_]): Adjusted[_] = a.map(transformAny(f, _))
}
