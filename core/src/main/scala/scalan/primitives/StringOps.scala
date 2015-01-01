package scalan.primitives

import scalan.{  Scalan, ScalanSeq }

trait StringOps extends UnBinOps { self: Scalan =>
  implicit class StringOpsCls(lhs: Rep[String]) {
    def +(rhs: Rep[String]) = StringConcat().apply(lhs, rhs)
    def startsWith(rhs: Rep[String]) = StringStartsWith().apply(lhs, rhs)
    def endsWith(rhs: Rep[String]) = StringEndsWith().apply(lhs, rhs)
    def contains(rhs: Rep[String]) = StringContains().apply(lhs, rhs)
    def matches(rhs: Rep[String]) = StringMatches().apply(lhs, rhs)
  }


  case class StringConcat extends EndoBinOp[String]("+", _ + _)
  case class StringContains extends BinOp[String, Boolean]("contains", _.contains(_))
  case class StringStartsWith extends BinOp[String, Boolean]("startsWith", _.startsWith(_))
  case class StringEndsWith extends BinOp[String, Boolean]("endsWith", _.endsWith(_))
  case class StringMatches extends BinOp[String, Boolean]("matches", _.matches(_))
}

