package scalan.io

import scalan.{ScalanExp, ScalanSeq, Scalan}

trait Console { self: Scalan =>
  def println(i: Rep[Int], s: Rep[String]): Rep[(Int, Unit)]
  def readLine(i: Rep[Int]): Rep[(Int, String)]
}

trait ConsoleSeq extends Console { self: ScalanSeq =>
  def println(i: Rep[Int], s: Rep[String]): Rep[(Int, Unit)] = (i + 1, Predef.println(s))
  def readLine(i: Rep[Int]): Rep[(Int, String)] = (i + 1, Predef.readLine())
}

trait ConsoleExp extends Console { self: ScalanExp =>
  def println(i: Rep[Int], s: Rep[String]): Rep[(Int, Unit)] = Println(i, s)
  def readLine(i: Rep[Int]): Rep[(Int, String)] = ReadLine(i)

  case class Println(i: Rep[Int], s: Rep[String]) extends BaseDef[(Int, Unit)]  {
    override def uniqueOpId = name(selfType)
    override def mirror(t: Transformer) = Println(t(i), t(s))
  }

  case class ReadLine(i: Rep[Int]) extends BaseDef[(Int, String)]  {
    override def uniqueOpId = name(selfType)
    override def mirror(t: Transformer) = ReadLine(t(i))
  }
}
