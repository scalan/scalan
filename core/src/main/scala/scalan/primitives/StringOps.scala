package scalan.primitives

import scalan.staged.BaseExp
import scalan.{ ScalanExp, Scalan, ScalanSeq }
import scalan.Scalan

trait StringOps extends UnBinOps { self: Scalan =>
  implicit class StringOpsCls(lhs: Rep[String]) {
    def toInt = StringToInt(lhs)
    def toDouble = StringToDouble(lhs)
    def length = string_length(lhs)
    def apply(index: Rep[Int]) = string_apply(lhs, index)
    def substring(start: Rep[Int], end: Rep[Int]) = string_substring(lhs, start, end)
    def +(rhs: Rep[String]) = StringConcat(lhs, rhs)
    def startsWith(rhs: Rep[String]) = StringStartsWith(lhs, rhs)
    def endsWith(rhs: Rep[String]) = StringEndsWith(lhs, rhs)
    def contains(rhs: Rep[String]) = StringContains(lhs, rhs)
    def matches(rhs: Rep[String]) = StringMatches(lhs, rhs)
  }

  object StringObject {
    lazy val empty = toRep("")
  }

  def string_length(str: Rep[String]): Rep[Int]
  def string_substring(str: Rep[String], start: Rep[Int], end: Rep[Int]): Rep[String]
  def string_apply(str: Rep[String], index: Rep[Int]): Rep[Char]

  val StringToInt = new UnOp[String, Int]("toInt", _.toInt)
  val StringToDouble = new UnOp[String, Double]("toDouble", _.toDouble)

  val StringConcat = new EndoBinOp[String]("+", _ + _)
  val StringContains = new BinOp[String, Boolean]("contains", _.contains(_))
  val StringStartsWith = new BinOp[String, Boolean]("startsWith", _.startsWith(_))
  val StringEndsWith = new BinOp[String, Boolean]("endsWith", _.endsWith(_))
  val StringMatches = new BinOp[String, Boolean]("matches", _.matches(_))
}


trait StringOpsSeq extends StringOps { self: ScalanSeq =>
  def string_length(str: Rep[String]): Rep[Int] = str.length
  def string_substring(str: Rep[String], start: Rep[Int], end: Rep[Int]): Rep[String] = str.substring(start, end)
  def string_apply(str: Rep[String], index: Rep[Int]): Rep[Char] = str.charAt(index)
}


trait StringOpsExp extends StringOps with BaseExp { self: ScalanExp =>

  case class StringSubstring(str: Rep[String], start: Rep[Int], end: Rep[Int]) extends BaseDef[String]
  case class StringLength(str: Rep[String]) extends BaseDef[Int]
  case class StringCharAt(str: Rep[String], index: Rep[Int]) extends BaseDef[Char]

  def string_length(str: Rep[String]): Rep[Int] = StringLength(str)
  def string_substring(str: Rep[String], start: Rep[Int], end: Rep[Int]): Rep[String] = StringSubstring(str, start, end)
  def string_apply(str: Rep[String], index: Rep[Int]): Rep[Char] = StringCharAt(str, index)
}

