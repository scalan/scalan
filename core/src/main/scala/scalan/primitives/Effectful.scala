package scalan.primitives

import scalan.staged.Expressions
import scalan.{ScalanExp, ScalanSeq, Scalan}
import scalan.compilation.Compiler

trait Effectful { self: Scalan =>
  def console_println(i: Rep[Int], s: Rep[String]): Rep[(Int, Unit)]
  def console_readline(i: Rep[Int]): Rep[(Int, String)]
  def console_eval[A:Elem](i: Rep[Int], v: Rep[A]): Rep[(Int, A)]
  def console_printlnE(s: Rep[String]): Rep[Unit]
  def console_readlineE(): Rep[String]
}

trait EffectfulSeq extends Effectful { self: ScalanSeq =>
  def console_println(i: Rep[Int], s: Rep[String]): Rep[(Int, Unit)] = (i + 1, Predef.println(s))
  def console_readline(i: Rep[Int]): Rep[(Int, String)] = (i + 1, Predef.readLine())
  def console_eval[A:Elem](i: Rep[Int], v: Rep[A]): Rep[(Int, A)] = (i + 1, v)
  def console_printlnE(s: Rep[String]): Rep[Unit] = Predef.println(s)
  def console_readlineE(): Rep[String] = Predef.readLine()
}

trait EffectfulExp extends Effectful with Expressions { self: ScalanExp =>
  def console_println(i: Rep[Int], s: Rep[String]): Rep[(Int, Unit)] = Println(i, s)
  def console_readline(i: Rep[Int]): Rep[(Int, String)] = ReadLine(i)
  def console_eval[A:Elem](i: Rep[Int], v: Rep[A]): Rep[(Int, A)] = Eval(i, v)

  def console_printlnE(s: Rep[String]): Rep[Unit] = reflectEffect(PrintlnE(s))
  def console_readlineE(): Rep[String] = reflectEffect(ReadLineE())

  case class Println(i: Rep[Int], s: Rep[String]) extends BaseDef[(Int, Unit)]  {
    override def uniqueOpId = name(selfType)
    override def mirror(t: Transformer) = Println(t(i), t(s))
  }

  case class ReadLine(i: Rep[Int]) extends BaseDef[(Int, String)]  {
    override def uniqueOpId = name(selfType)
    override def mirror(t: Transformer) = ReadLine(t(i))
  }

  case class Eval[A:Elem](i: Rep[Int], v: Rep[A]) extends BaseDef[(Int, A)]  {
    override def uniqueOpId = name(selfType)
    override def mirror(t: Transformer) = Eval(t(i), t(v))
  }

  case class PrintlnE(s: Rep[String]) extends BaseDef[Unit]  {
    override def uniqueOpId = name(selfType)
    override def mirror(t: Transformer) = PrintlnE(t(s))
  }

  case class ReadLineE() extends BaseDef[String]  {
    override def uniqueOpId = name(selfType)
    override def mirror(t: Transformer) = ReadLineE()
  }

}

trait EffectfulCompiler extends EffectfulExp with Expressions with Compiler { self: ScalanExp =>

  object EffectfulRewriter extends Rewriter {
    def apply[T](x: Exp[T]): Exp[T] = (x match {
      case Def(Println(i, s)) =>
        Pair(i + 1, reifyEffectsExp(console_printlnE(s)))

      case Def(ReadLine(i)) =>
        Pair(i + 1, reifyEffectsExp(console_readlineE()))

      case Def(Eval(i,v)) =>
        Pair(i + 1, v)

      case _ => x
    }).asRep[T]
  }

  override def graphPasses(compilerConfig: CompilerConfig) =
    super.graphPasses(compilerConfig) :+
      constantPass(GraphTransformPass("io", DefaultMirror, EffectfulRewriter))
}
