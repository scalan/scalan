/**
 * Author: Alexander Slesarenko
 * Date: 9/17/12
 */
package scalan.primitives

import scalan.{ScalanStaged, ScalanSeq, Scalan}

trait LogicalOps { self: Scalan =>
  def logical_and(x: Rep[Boolean], y: Rep[Boolean]): Rep[Boolean]
  def logical_or(x: Rep[Boolean], y: Rep[Boolean]): Rep[Boolean]
  def logical_not(x: Rep[Boolean]): Rep[Boolean]
  def boolean_toInt(lhs: Rep[Boolean]): Rep[Int]

  implicit class RepBooleanOps(value: Rep[Boolean]) {
    def &&(y: Rep[Boolean]): Rep[Boolean] = logical_and(value, y)
    def ||(y: Rep[Boolean]): Rep[Boolean] = logical_or(value, y)
    def unary_!() : Rep[Boolean] = logical_not(value)
    def toInt: Rep[Int] = boolean_toInt(value)
  }
}

trait LogicalOpsSeq extends LogicalOps { self: ScalanSeq =>
  def logical_and(x: Rep[Boolean], y: Rep[Boolean]): Rep[Boolean] = x && y
  def logical_or(x: Rep[Boolean], y: Rep[Boolean]): Rep[Boolean] = x || y
  def logical_not(x: Rep[Boolean]): Rep[Boolean] = !x
  def boolean_toInt(lhs: Rep[Boolean]): Rep[Int] = if (lhs) 1 else 0
}

trait LogicalOpsExp extends LogicalOps { self: ScalanStaged =>
  abstract class LogicalBinOp(val opName: String) extends BinOp[Boolean] {
    val selfType = element[Boolean]
  }
  case class And(lhs: Exp[Boolean], rhs: Exp[Boolean]) extends LogicalBinOp("&&") {
    def copyWith(l: Rep[Boolean], r: Rep[Boolean]) = this.copy(lhs = l, rhs = r)
  }
  case class Or (lhs: Exp[Boolean], rhs: Exp[Boolean]) extends LogicalBinOp("||") {
    def copyWith(l: Rep[Boolean], r: Rep[Boolean]) = this.copy(lhs = l, rhs = r)
  }
  abstract class LogicalUnOp(val opName: String) extends UnOp[Boolean] {
    val selfType = element[Boolean]
  }
  case class Not(arg: Exp[Boolean]) extends LogicalUnOp("!") {
    def copyWith(a: Rep[Boolean]) = this.copy(arg = a)
  }
  case class BooleanToInt(arg: Exp[Boolean]) extends Def[Int] with UnOpBase[Boolean,Int] {
    lazy val uniqueOpId = name(arg.elem)
    val selfType = element[Int]
    override def mirror(t: Transformer) = {
      copyWith(t(arg))
    }
    def copyWith(a: Rep[Boolean]) = this.copy(arg = a)
    def opName = "ToInt"
  }

  def logical_and(x: Exp[Boolean], y: Exp[Boolean]): Exp[Boolean] = And(x, y)
  def logical_or(x: Exp[Boolean], y: Exp[Boolean]): Exp[Boolean] = Or(x, y)
  def logical_not(x: Rep[Boolean]): Rep[Boolean] = Not(x)
  def boolean_toInt(lhs: Rep[Boolean]): Rep[Int] = BooleanToInt(lhs)
}