package scalan.primitives

import scalan.staged.BaseExp
import scalan.{ ScalanExp, Scalan, ScalanSeq }
import scalan.Scalan

trait StringOps extends UnBinOps { self: Scalan =>
  implicit class StringOpsCls(lhs: Rep[String]) {
    def toInt = StringToInt().apply(lhs)
    def toDouble = StringToDouble().apply(lhs)
    def apply(index: Rep[Int]) = string_apply(lhs, index)
    def substring(start: Rep[Int], end: Rep[Int]) = string_substring(lhs, start, end)
    def +(rhs: Rep[String]) = StringConcat().apply(lhs, rhs)
    def startsWith(rhs: Rep[String]) = StringStartsWith().apply(lhs, rhs)
    def endsWith(rhs: Rep[String]) = StringEndsWith().apply(lhs, rhs)
    def contains(rhs: Rep[String]) = StringContains().apply(lhs, rhs)
    def matches(rhs: Rep[String]) = StringMatches().apply(lhs, rhs)
  }

  def string_substring(str: Rep[String], start: Rep[Int], end: Rep[Int]): Rep[String]
  def string_apply(str: Rep[String], index: Rep[Int]): Rep[Char]

  case class StringToInt() extends UnOp[String, Int]("toInt", _.toInt)
  case class StringToDouble() extends UnOp[String, Double]("toDouble", _.toDouble)

  case class StringConcat extends EndoBinOp[String]("+", _ + _)
  case class StringContains extends BinOp[String, Boolean]("contains", _.contains(_))
  case class StringStartsWith extends BinOp[String, Boolean]("startsWith", _.startsWith(_))
  case class StringEndsWith extends BinOp[String, Boolean]("endsWith", _.endsWith(_))
  case class StringMatches extends BinOp[String, Boolean]("matches", _.matches(_))
}


trait StringOpsSeq extends StringOps { self: ScalanSeq =>
  def string_substring(str: Rep[String], start: Rep[Int], end: Rep[Int]): Rep[String] = str.substring(start, end)

  def string_apply(str: Rep[String], index: Rep[Int]): Rep[Char] = str.charAt(index)
}


trait StringOpsExp extends StringOps with BaseExp { self: ScalanExp =>

  case class StringSubstr(str: Rep[String], start: Rep[Int], end: Rep[Int]) extends BaseDef[String] {
    override def mirror(t: Transformer) = StringSubstr(t(str), t(start), t(end))
    lazy val uniqueOpId = name(selfType)
  }
  case class StringApply(str: Rep[String], index: Rep[Int]) extends BaseDef[Char] {
    override def mirror(t: Transformer) = StringApply(t(str), t(index))
    lazy val uniqueOpId = name(selfType)
  }

  def string_substring(str: Rep[String], start: Rep[Int], end: Rep[Int]): Rep[String] = StringSubstr(str, start, end)

  def string_apply(str: Rep[String], index: Rep[Int]): Rep[Char] = StringApply(str, index)
}

