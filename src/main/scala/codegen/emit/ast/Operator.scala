/**
 * Author: Alexander Slesarenko
 * Date: 10/16/12
 */
package scalan.codegen.emit.ast

case class Operator(name: String) extends AST { }

object Operator {
  val assignOp = Operator("=")
  val divOp = Operator("/")
  val geqOp = Operator(">=")
  val gtOp = Operator(">")
  val lshiftOp = Operator("<<")
  val ltOp = Operator("<")
  val lteqOp = Operator("<=")
  val logOrOp = Operator("||")
  val minusOp = Operator("-")
  val modOp = Operator("%")
  val mulOp = Operator("*")
  val plusOp = Operator("+")
  val rshiftOp = Operator(">>")
  val notOp = Operator("!")
  val eqOp = Operator("==")
}
