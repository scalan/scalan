package scalan.meta

object SqlAST {

  abstract sealed class Statement

  case class SqlException(msg: String) extends Exception(msg)

  type Script = List[Statement]
  type Schema = List[Column]
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

  abstract sealed class Operator

  case class Scan(table: Table) extends Operator

  case class Distinct(table: Operator) extends Operator
  case class Union(left: Operator, right: Operator) extends Operator
  case class Except(left: Operator, right: Operator) extends Operator
  case class Intersect(left: Operator, right: Operator) extends Operator

  case class TableAlias(table: Operator, alias: String) extends Operator

  case class ProjectionColumn(expr: Expression, alias: Option[String])
  case class Project(parent: Operator, columns: List[ProjectionColumn]) extends Operator

  case class Filter(parent: Operator, predicate: Expression) extends Operator

  case class GroupBy(parent: Operator, columns: ExprList) extends Operator

  abstract sealed class SortDirection
  case object Ascending extends SortDirection
  case object Descending extends SortDirection

  sealed trait NullsOrdering
  case object NullsFirst extends NullsOrdering
  case object NullsLast extends NullsOrdering
  case object NullsOrderingUnspecified extends NullsOrdering

  case class SortSpec(expr: Expression, direction: SortDirection, nulls: NullsOrdering)

  case class OrderBy(parent: Operator, columns: List[SortSpec]) extends Operator

  case class Limit(parent: Operator, limit: Expression) extends Operator

  case class SubSelect(parent: Operator) extends Operator

  sealed trait JoinSpec
  case class On(condition: Expression) extends JoinSpec
  case class Using(columns: ColumnList) extends JoinSpec
  case object Natural extends JoinSpec

  case class Join(outer: Operator, inner: Operator, joinType: JoinType, spec: JoinSpec) extends Operator
  case class CrossJoin(outer: Operator, inner: Operator) extends Operator
  case class UnionJoin(outer: Operator, inner: Operator) extends Operator

  case class SelectStmt(operator: Operator) extends Statement

  case class CreateTableStmt(table: Table) extends Statement

  case class CreateIndexStmt(name: String, table: Table, key: ColumnList) extends Statement

  sealed trait Expression

  case class SelectExpr(stmt: SelectStmt) extends Expression

  sealed trait BinOp

  sealed trait ArithOp extends BinOp
  case object Plus extends ArithOp
  case object Minus extends ArithOp
  case object Times extends ArithOp
  case object Divide extends ArithOp
  case object Modulo extends ArithOp

  sealed trait LogicOp extends BinOp
  case object And extends LogicOp
  case object Or extends LogicOp

  sealed trait ComparisonOp extends BinOp
  case object Eq extends ComparisonOp
  case object Greater extends ComparisonOp
  case object GreaterEq extends ComparisonOp
  case object Less extends ComparisonOp
  case object LessEq extends ComparisonOp
  case object Is extends ComparisonOp

  case object Concat extends BinOp

  case class BinOpExpr(op: BinOp, left: Expression, right: Expression) extends Expression

  sealed trait AggregateOp
  case object Count extends AggregateOp
  case object Sum extends AggregateOp
  case object Avg extends AggregateOp
  case object Max extends AggregateOp
  case object Min extends AggregateOp
//  case object Every extends AggregateOp
//  case object Any extends AggregateOp
//  case object Some extends AggregateOp

  case class AggregateExpr(op: AggregateOp, distinct: Boolean, value: Expression) extends Expression {
    def isCountAll = this == CountAllExpr
  }
  val CountAllExpr = AggregateExpr(Count, false, Literal(1, IntType))

  case class LikeExpr(left: Expression, right: Expression, escape: Option[Expression]) extends Expression

  case class InListExpr(left: Expression, right: ExprList) extends Expression

  case class InExpr(left: Expression, right: SelectStmt) extends Expression

  case class ExistsExpr(query: Expression) extends Expression

  case class NegExpr(opd: Expression) extends Expression

  case class NotExpr(opd: Expression) extends Expression

  case class SubstrExpr(str: Expression, from: Expression, len: Expression) extends Expression

  case class IsNullExpr(opd: Expression) extends Expression

  case class FuncExpr(name: String, params: ExprList) extends Expression

  case class CastExpr(expr: Expression, to: ColumnType) extends Expression

  case class Literal(value: Any, tp: ColumnType) extends Expression

  // FIXME CaseWhenExpr(operand: Option[Expression], cases: List[(Expression, Expression)], default: Option[Expression])
  case class CaseWhenExpr(list: ExprList) extends Expression

  case class ColumnRef(table: Option[String], name: String) extends Expression {
    def asString = (table match {
      case Some(table) => table + "."
      case None => ""
    }) + name
  }

  def Schema(list: Column*): Schema = list.toList

  def Script(stmts: Statement*): Script = stmts.toList

  def ExprList(exprs: Expression*): ExprList = exprs.toList
}
