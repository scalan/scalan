package scalan.primitives

import scalan.Base
import scalan.{ Scalan }

trait StringOps extends Base { self: Scalan =>
  implicit class StringOpsCls(lhs: Rep[String]) {
    def toInt = StringToInt(lhs)
    def toDouble = StringToDouble(lhs)
    def length = StringLength(lhs)
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

  val StringToInt = new UnOp[String, Int]("toInt", _.toInt)
  val StringToDouble = new UnOp[String, Double]("toDouble", _.toDouble)
  val StringLength = new UnOp[String, Int]("length", _.length)

  val StringConcat = new EndoBinOp[String]("+", _ + _)
  val StringContains = new BinOp[String, Boolean]("contains", _.contains(_))
  val StringStartsWith = new BinOp[String, Boolean]("startsWith", _.startsWith(_))
  val StringEndsWith = new BinOp[String, Boolean]("endsWith", _.endsWith(_))
  val StringMatches = new BinOp[String, Boolean]("matches", _.matches(_))

  case class StringSubstring(str: Rep[String], start: Rep[Int], end: Rep[Int]) extends BaseDef[String]
  case class StringCharAt(str: Rep[String], index: Rep[Int]) extends BaseDef[Char]

  def string_substring(str: Rep[String], start: Rep[Int], end: Rep[Int]): Rep[String] = StringSubstring(str, start, end)
  def string_apply(str: Rep[String], index: Rep[Int]): Rep[Char] = StringCharAt(str, index)

  override def rewriteDef[T](d: Def[T]) = d match {
    case ApplyBinOp(op, x, Def(Const(""))) if op == StringConcat =>
      x
    case ApplyBinOp(op, Def(Const("")), x) if op == StringConcat =>
      x
    case ApplyBinOp(op, x, Def(Const(""))) if op == StringStartsWith || op == StringEndsWith =>
      toRep(true)
    case _ => super.rewriteDef(d)
  }
}
