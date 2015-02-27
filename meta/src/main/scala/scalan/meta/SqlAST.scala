package scalan.meta

/**
 * Created by knizhnik on 1/13/15.
 */
trait SqlAST {

  abstract sealed class Statement

  case class SqlException(msg: String) extends Exception(msg)

  type Script = Array[Statement]
  type Schema = Array[Column]
  type ColumnList = List[String]
  type ExprList = List[Expression]

  case class Table(name: String, schema: Schema)

  case class Column(name: String, ctype: ColumnType)

  case class ColumnType(sqlName: String, scalaName: String)

  val IntType = ColumnType("integer", "Int")
  val DoubleType = ColumnType("real", "Double")
  val LongType = ColumnType("bigint", "Long")
  val StringType = ColumnType("varchar", "String")
  val CharType = ColumnType("char", "Char")
  val BoolType = ColumnType("bit", "Boolean")
  val DateType = ColumnType("date", "Int")
  val NullType = ColumnType("null", "Any")

  sealed abstract class JoinType
  case object Inner extends JoinType
  case object LeftOuter extends JoinType
  case object RightOuter extends JoinType
  case object FullOuter extends JoinType
  case object LeftSemi extends JoinType

  abstract sealed class SortDirection
  case object Ascending extends SortDirection
  case object Descending extends SortDirection

  abstract sealed class Operator

  case class Scan(table: Table) extends Operator

  case class Distinct(table: Operator) extends Operator
  case class Union(left: Operator, right: Operator) extends Operator
  case class Except(left: Operator, right: Operator) extends Operator
  case class Intersect(left: Operator, right: Operator) extends Operator

  case class TableAlias(table: Operator, alias: String) extends Operator

  case class Project(parent: Operator, columns: ExprList) extends Operator

  case class Filter(parent: Operator, predicate: Expression) extends Operator

  case class GroupBy(parent: Operator, columns: ExprList) extends Operator

  case class OrderBy(parent: Operator, columns: ExprList) extends Operator

  case class Limit(parent: Operator, limit: Expression) extends Operator

  case class SubSelect(parent: Operator) extends Operator

  case class Join(outer: Operator, inner: Operator, on: Expression) extends Operator
  case class CrossJoin(outer: Operator, inner: Operator) extends Operator

  case class SelectStmt(operator: Operator) extends Statement


  case class CreateTableStmt(table: Table) extends Statement

  case class CreateIndexStmt(name: String, table: Table, key: ColumnList) extends Statement

  sealed abstract class Expression {
    var alias = ""
  }

  case class StarExpr() extends Expression

  case class SelectExpr(stmt: SelectStmt) extends Expression

  case class AddExpr(left: Expression, right: Expression) extends Expression

  case class SubExpr(left: Expression, right: Expression) extends Expression

  case class MulExpr(left: Expression, right: Expression) extends Expression

  case class DivExpr(left: Expression, right: Expression) extends Expression

  case class EqExpr(left: Expression, right: Expression) extends Expression

  case class NeExpr(left: Expression, right: Expression) extends Expression

  case class GtExpr(left: Expression, right: Expression) extends Expression

  case class GeExpr(left: Expression, right: Expression) extends Expression

  case class LtExpr(left: Expression, right: Expression) extends Expression

  case class LeExpr(left: Expression, right: Expression) extends Expression

  case class LikeExpr(left: Expression, right: Expression) extends Expression

  case class AndExpr(left: Expression, right: Expression) extends Expression

  case class OrExpr(left: Expression, right: Expression) extends Expression

  case class InListExpr(left: Expression, right: ExprList) extends Expression

  case class InExpr(left: Expression, right: SelectStmt) extends Expression

  case class ExistsExpr(query: Expression) extends Expression

  case class NegExpr(opd: Expression) extends Expression

  case class NotExpr(opd: Expression) extends Expression

  case class AvgExpr(opd: Expression) extends Expression

  case class SumExpr(opd: Expression) extends Expression

  case class SumDistinctExpr(opd: Expression) extends Expression

  case class MaxExpr(opd: Expression) extends Expression

  case class MinExpr(opd: Expression) extends Expression

  case class SubstrExpr(str: Expression, from: Expression, len: Expression) extends Expression

  case class CountExpr() extends Expression

  case class CountNotNullExpr(exp: Expression) extends Expression

  case class CountDistinctExpr(exps: ExprList) extends Expression

  case class IsNullExpr(opd: Expression) extends Expression

  case class FuncExpr(name: String, params: ExprList) extends Expression

  case class CastExpr(expr: Expression, to: ColumnType) extends Expression

  case class Literal(value: Any, tp: ColumnType) extends Expression

  case class CaseWhenExpr(list: ExprList) extends Expression

  case class ColumnRef(table: String, name: String) extends Expression

  def ColumnList(list: String*): ColumnList = list.toList

  def Schema(list: Column*): Schema = list.toArray

  def Script(stmts: Statement*): Script = stmts.toArray

  def ExprList(exprs: Expression*): ExprList = exprs.toList
}
