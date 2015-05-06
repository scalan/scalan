package scalan.io

import scalan.staged.Expressions
import scalan.{ScalanExp, ScalanSeq, Scalan}

trait Console { self: Scalan =>
  def console_println(i: Rep[Int], s: Rep[String]): Rep[(Int, Unit)]
  def readline(i: Rep[Int]): Rep[(Int, String)]
}

trait ConsoleSeq extends Console { self: ScalanSeq =>
  def console_println(i: Rep[Int], s: Rep[String]): Rep[(Int, Unit)] = (i + 1, Predef.println(s))
  def readline(i: Rep[Int]): Rep[(Int, String)] = (i + 1, Predef.readLine())
}

trait ConsoleExp extends Console with Expressions { self: ScalanExp =>
  def console_println(i: Rep[Int], s: Rep[String]): Rep[(Int, Unit)] = Println(i, s)
  def readline(i: Rep[Int]): Rep[(Int, String)] = ReadLine(i)

  def console_printlnE(s: Rep[String]): Rep[Unit] = reflectEffect(PrintlnE(s))
  def readlineE(): Rep[String] = reflectEffect(ReadLineE())

  case class Println(i: Rep[Int], s: Rep[String]) extends BaseDef[(Int, Unit)]  {
    override def uniqueOpId = name(selfType)
    override def mirror(t: Transformer) = Println(t(i), t(s))
  }

  case class ReadLine(i: Rep[Int]) extends BaseDef[(Int, String)]  {
    override def uniqueOpId = name(selfType)
    override def mirror(t: Transformer) = ReadLine(t(i))
  }

  case class PrintlnE(s: Rep[String]) extends BaseDef[Unit]  {
    override def uniqueOpId = name(selfType)
    override def mirror(t: Transformer) = PrintlnE(t(s))
  }

  case class ReadLineE() extends BaseDef[String]  {
    override def uniqueOpId = name(selfType)
    override def mirror(t: Transformer) = ReadLineE()
  }

  override def rewriteDef[T](d: Def[T]) = (d match {
    case Println(i, s) =>
      Pair(i + 1, console_printlnE(s))

    case ReadLine(i) =>
      Pair(i + 1, readlineE())

    case _ => super.rewriteDef(d)
  }).asInstanceOf[Exp[_]]
}
