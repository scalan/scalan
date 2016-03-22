package scalan.primitives

import scala.io.StdIn
import scalan.staged.Expressions
import scalan.{ScalanDslExp, ScalanExp, ScalanStd, Scalan}
import scalan.compilation.Compiler

trait Effectful { self: Scalan =>
  def console_println(i: Rep[Int], s: Rep[String]): Rep[(Int, Unit)]
  def console_readline(i: Rep[Int]): Rep[(Int, String)]
  def console_eval[A:Elem](i: Rep[Int], v: Rep[A]): Rep[(Int, A)]
  def console_printlnE(s: Rep[String]): Rep[Unit]
  def console_readlineE(): Rep[String]
}

trait EffectfulStd extends Effectful { self: ScalanStd =>
  def console_println(i: Rep[Int], s: Rep[String]): Rep[(Int, Unit)] = (i + 1, println(s))
  def console_readline(i: Rep[Int]): Rep[(Int, String)] = (i + 1, StdIn.readLine())
  def console_eval[A:Elem](i: Rep[Int], v: Rep[A]): Rep[(Int, A)] = (i + 1, v)
  def console_printlnE(s: Rep[String]): Rep[Unit] = println(s)
  def console_readlineE(): Rep[String] = StdIn.readLine()
}

trait EffectfulExp extends Effectful with Expressions { self: ScalanExp =>
  def console_println(i: Rep[Int], s: Rep[String]): Rep[(Int, Unit)] = Println(i, s)
  def console_readline(i: Rep[Int]): Rep[(Int, String)] = ReadLine(i)
  def console_eval[A:Elem](i: Rep[Int], v: Rep[A]): Rep[(Int, A)] = Eval(i, v)

  def console_printlnE(s: Rep[String]): Rep[Unit] = reflectEffect(PrintlnE(s))
  def console_readlineE(): Rep[String] = reflectEffect(ReadLineE())

  case class Println(i: Rep[Int], s: Rep[String]) extends BaseDef[(Int, Unit)]

  case class ReadLine(i: Rep[Int]) extends BaseDef[(Int, String)]

  case class Eval[A:Elem](i: Rep[Int], v: Rep[A]) extends BaseDef[(Int, A)]

  case class PrintlnE(s: Rep[String]) extends BaseDef[Unit]

  case class ReadLineE() extends BaseDef[String]
}

trait EffectfulCompiler[ScalanCake <: ScalanDslExp with EffectfulExp] extends Compiler[ScalanCake] {
  import scalan._

  object EffectfulRewriter extends Rewriter {
    def apply[T](x: Exp[T]): Exp[T] = (x match {
      case Def(Println(i, s)) =>
        Pair(i + 1, console_printlnE(s))

      case Def(ReadLine(i)) =>
        Pair(i + 1, console_readlineE())

      case Def(Eval(i,v)) =>
        Pair(i + 1, v)

      case _ => x
    }).asRep[T]
  }

  override def graphPasses(compilerConfig: CompilerConfig) = {
    val name = "effects"
    super.graphPasses(compilerConfig) :+
    constantPass[GraphPass](name, b => new GraphTransformPass(b, name, DefaultMirror, EffectfulRewriter))
  }
}
