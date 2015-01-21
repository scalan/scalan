package scalan.primitives

import scalan.Scalan

trait StringOps extends UnBinOps { self: Scalan =>
  implicit class StringOpsCls(lhs: Rep[String]) {
    def +(rhs: Rep[String]) = StringConcat(lhs, rhs)
    def startsWith(rhs: Rep[String]) = StringStartsWith(lhs, rhs)
    def endsWith(rhs: Rep[String]) = StringEndsWith(lhs, rhs)
    def contains(rhs: Rep[String]) = StringContains(lhs, rhs)
    def matches(rhs: Rep[String]) = StringMatches(lhs, rhs)
  }

  val StringConcat = new EndoBinOp[String]("+", _ + _)
  val StringContains = new BinOp[String, Boolean]("contains", _.contains(_))
  val StringStartsWith = new BinOp[String, Boolean]("startsWith", _.startsWith(_))
  val StringEndsWith = new BinOp[String, Boolean]("endsWith", _.endsWith(_))
  val StringMatches = new BinOp[String, Boolean]("matches", _.matches(_))
}
