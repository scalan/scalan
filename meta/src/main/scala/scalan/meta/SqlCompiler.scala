package scalan.meta

/**
 * Created by knizhnik on 1/14/15.
 */
trait SqlCompiler extends SqlAST with ScalanAst with SqlParser {
  case class Scope(var ctx: Context, outer: Option[Scope], nesting: Int, name: String) {
    def lookup(col: ColumnRef): Binding = {
      ctx.resolve(col.table, col.name) match {
        case Some(b) => b
        case None => {
          outer match {
            case Some(s: Scope) => s.lookup(col)
            case _ => throw SqlException( s"""Failed to lookup column ${col.table}.${col.name}""")
          }
        }
      }
    }
  }

  var currScope: Scope = Scope(new GlobalContext, None, 0, "scalan")
  var indent = "\t"

  def pushContext(opd: Operator) = {
    currScope = Scope(currScope.ctx, Some(currScope), currScope.nesting + 1, if (currScope.nesting == 0) "r" else "r" + currScope.nesting.toString)
    currScope.ctx = buildContext(opd)
  }

  def popContext() = {
    currScope = currScope.outer.get
  }

  def lookup(col: ColumnRef): Binding = currScope.lookup(col)
  
  def generateSchema(sql: String): String = {
    val statements = parseDDL(sql)

    statements.map(stmt => {
      stmt match {
        case s: CreateIndexStmt => generateIndex(s)
        case s: CreateTableStmt => generateTable(s.table)
      }
    }).mkString("\n\n")
  }

  val currMethod:SMethodDef = throw new IllegalStateException("Selet can be used only inside method")

  def generateQuery(m: SMethodDef): String = {
    val args = m.allArgs.map(arg => arg.name + ": " + arg.tpe).mkString(", ")
    val sql = m.body.get.asInstanceOf[SApply].args(0).asInstanceOf[SLiteral].value
    val select = parseSelect(sql)
    val op = select.operator
    s"""type ${m.name}_Result = ${resultType(select)}
      |
      | override def ${m.name}(${args}) = ${generateOperator(op)}${tableToArray(op)}""".stripMargin
  }

  def tablesInNestedSelects(e: Expression): Set[Table] = {
    e match {
      case AndExpr(l, r) => tablesInNestedSelects(l) ++ tablesInNestedSelects(r)
      case OrExpr(l, r) => tablesInNestedSelects(l) ++ tablesInNestedSelects(r)
      case AddExpr(l, r) => tablesInNestedSelects(l) ++ tablesInNestedSelects(r)
      case SubExpr(l, r) => tablesInNestedSelects(l) ++ tablesInNestedSelects(r)
      case MulExpr(l, r) => tablesInNestedSelects(l) ++ tablesInNestedSelects(r)
      case DivExpr(l, r) => tablesInNestedSelects(l) ++ tablesInNestedSelects(r)
      case EqExpr(l, r) => tablesInNestedSelects(l) ++ tablesInNestedSelects(r)
      case NeExpr(l, r) => tablesInNestedSelects(l) ++ tablesInNestedSelects(r)
      case LeExpr(l, r) => tablesInNestedSelects(l) ++ tablesInNestedSelects(r)
      case LtExpr(l, r) => tablesInNestedSelects(l) ++ tablesInNestedSelects(r)
      case GtExpr(l, r) => tablesInNestedSelects(l) ++ tablesInNestedSelects(r)
      case GeExpr(l, r) => tablesInNestedSelects(l) ++ tablesInNestedSelects(r)
      case ExistsExpr(q) => tablesInNestedSelects(q)
      case LikeExpr(l, r) => tablesInNestedSelects(l) ++ tablesInNestedSelects(r)
      case NegExpr(opd) => tablesInNestedSelects(opd)
      case NotExpr(opd) => tablesInNestedSelects(opd)
      case CastExpr(exp, typ) => tablesInNestedSelects(exp)
      case SelectExpr(s) => tables(s.operator)
      case InExpr(s, q) => tablesInNestedSelects(s) ++ tables(q.operator)
      case _ => Set()
    }
  }

  def tables(op: Operator): Set[Table] = {
    op match {
      case Join(outer, inner, on) => tables(outer) ++ tables(inner)
      case Scan(t) => Set(t)
      case OrderBy(p, by) => tables(p)
      case GroupBy(p, by) => tables(p)
      case Filter(p, predicate) => tables(p) ++ tablesInNestedSelects(predicate)
      case Project(p, columns) => tables(p)
      case TableAlias(t, a) => tables(t)
      case SubSelect(p) => tables(p)
    }
  }

  def indexToPath(i: Int, n: Int) = {
    if (n > 1 && (n <= 8 || i < 7)) "._" + (i+1)
    else {
      val path = ".tail" * i
      if (i == n - 1) path else path + ".head"
    }
  }

  case class Binding(scope: String, path: String, column: Column)

  abstract class Context {
    def resolve(schema: String, name: String): Option[Binding]
    val scope = currScope
  }

  class GlobalContext() extends Context {
    def resolve(schema: String, name: String): Option[Binding] = None
  }
  
  case class TableContext(table: Table) extends Context {
    def resolve(schema: String, name: String): Option[Binding] = {
      if (schema.isEmpty || schema == table.name) {
        val i = table.schema.indexWhere(c => c.name == name)
        if (i >= 0) {
          //val path = indexToPath(i, table.schema.length);
          val path = "." + name
          Some(Binding(scope.name, path, table.schema(i)))
        } else None
      } else None
    }
  }

  case class JoinContext(outer: Context, inner: Context) extends Context {
    def resolve(schema: String, name: String): Option[Binding] = {
      (outer.resolve(schema, name), inner.resolve(schema, name)) match {
        case (Some(b), None) => Some(Binding(b.scope, ".head" + b.path, b.column))
        case (None, Some(b)) => Some(Binding(b.scope, ".tail" + b.path, b.column))
        case (Some(_), Some(_)) => throw SqlException( s"""Ambiguous reference to $schema.$name""")
        case _ => None

      }
    }
  }

  case class AliasContext(parent: Context, alias: String) extends Context {
    def resolve(scope: String, name: String): Option[Binding] = {
      if (scope == alias) {
        parent.resolve("", name) match {
          case Some(b) => Some(Binding(b.scope, b.path, b.column))
          case None => None
        }
      } else parent.resolve(scope, name)
    }
  }

  case class ProjectContext(parent: Context, columns: ExprList) extends Context {
    def resolve(table: String, name: String): Option[Binding] = {
      val i = columns.indexWhere(c => (table.isEmpty && c.alias == name) || c == ColumnRef(table, name))
      if (i >= 0) {
        val saveScope = currScope
        currScope = Scope(parent, scope.outer, scope.nesting, scope.name)
        val cType =  getExprType(columns(i))
        currScope = saveScope
        Some(Binding(scope.name, indexToPath(i, columns.length), Column(name, cType)))
      }
      else None
    }
  }

  def buildContext(op: Operator): Context = {
    op match {
      case Join(outer, inner, on) => JoinContext(buildContext(outer), buildContext(inner))
      case Scan(t) => TableContext(t)
      case OrderBy(p, by) => buildContext(p)
      case GroupBy(p, by) => buildContext(p)
      case Filter(p, predicate) => buildContext(p)
      case Project(p, columns) => ProjectContext(buildContext(p), columns)
      case TableAlias(p, a) => AliasContext(buildContext(p), a)
      case SubSelect(p) => buildContext(p)
    }

  }

  def divOp(expr: Expression) = if (getExprType(expr) == IntType) "/!" else "/"

 
  def castTo(from: Expression, to: Expression): Expression = {
    val fromType = getExprType(from)
    val toType = getExprType(to)
    if (fromType != toType && (toType == StringType || toType == DoubleType)) {
      from match {
        case Literal(v,t) => Literal(v.asInstanceOf[java.lang.Number].doubleValue(), DoubleType)
        case _ => CastExpr(from, toType)
      }
    } else from
  }

  def patternMatch(text: Expression, pattern: Expression): String = {
    val left = generateExpr(text)
    pattern match {
      case Literal(v, t) if (t == StringType) =>
        val p = v.toString
        if (p.indexOf('%') < 0 && p.indexOf('_') < 0) "(" + left + " == \"" + p + "\")"
        else if (p.lastIndexOf('%') == 0 && p.indexOf('_') < 0) left + ".startsWith(\"" + p.substring(1) + "\")"
        else if (p.indexOf('%') == p.length - 1 && p.indexOf('_') < 0) left + ".endsWith(\"" + p.substring(0, p.length - 1) + "\")"
        else if (p.lastIndexOf('%', p.length - 2) == 0 && p.indexOf('%', 1) == p.length - 1 && p.indexOf('_') < 0) left + ".contains(\"" + p.substring(1, p.length - 1) + "\")"
        else left + ".matches(\"" + p.replace("%", ".*").replace('_', '.') + "\")"
    }
  }

  def generateCaseWhen(list: ExprList, i: Int): String = {
    if (i == list.length) ""
    else if (i == list.length - 1) " ELSE (" + generateExpr(list(i)) + ")"
    else (if (i == 0) "IF (" else " ELSEIF (") + generateExpr(list(i)) + ") THEN (" + generateExpr(list(i + 1)) + ")" + generateCaseWhen(list, i + 2)
  }

  def printValue(value: Any, tp: ColumnType): String = {
    if (tp == StringType) "\"" + value.toString + "\""
    else value.toString
  }

  def generateExpr(expr: Expression): String = {
    expr match {
      case AndExpr(l, r) => "(" + generateExpr(l) + " && " + generateExpr(r) + ")"
      case OrExpr(l, r) => "(" + generateExpr(l) + " || " + generateExpr(r) + ")"
      case AddExpr(l, r) => "(" + generateExpr(castTo(l, r)) + " + " + generateExpr(castTo(r, l)) + ")"
      case SubExpr(l, r) => "(" + generateExpr(castTo(l, r)) + " - " + generateExpr(castTo(r, l)) + ")"
      case MulExpr(l, r) => "(" + generateExpr(castTo(l, r)) + " * " + generateExpr(castTo(r, l)) + ")"
      case DivExpr(l, r) => "(" + generateExpr(castTo(l, r)) + divOp(expr) + generateExpr(castTo(r, l)) + ")"
      case EqExpr(l, r) => "(" + generateExpr(castTo(l, r)) + " === " + generateExpr(castTo(r, l)) + ")"
      case NeExpr(l, r) => "(" + generateExpr(castTo(l, r)) + " !== " + generateExpr(castTo(r, l)) + ")"
      case LeExpr(l, r) => "(" + generateExpr(castTo(l, r)) + " <= " + generateExpr(castTo(r, l)) + ")"
      case LtExpr(l, r) => "(" + generateExpr(castTo(l, r)) + " < " + generateExpr(castTo(r, l)) + ")"
      case GtExpr(l, r) => "(" + generateExpr(castTo(l, r)) + " > " + generateExpr(castTo(r, l)) + ")"
      case GeExpr(l, r) => "(" + generateExpr(castTo(l, r)) + " >= " + generateExpr(castTo(r, l)) + ")"
      case ExistsExpr(q) => "(" + generateExpr(q) + ".count !== 0)"
      case LikeExpr(l, r) => patternMatch(l, r)
      case NegExpr(opd) => "-" + generateExpr(opd)
      case NotExpr(opd) => "!" + generateExpr(opd)
      case Literal(v, t) => "toRep(" + printValue(v, t) + ")"
      case CastExpr(exp, typ) => generateExpr(exp) + (if (typ == StringType) ".toStr" else ".to" + typ.scalaName)
      case c: ColumnRef => {
        val binding = lookup(c)
        binding.scope + binding.path
      }
      case SelectExpr(s) => {
        val saveIndent = indent
        indent += "\t"
        val subselect = "(" + generateOperator(s.operator) + ")"
        indent = saveIndent
        subselect
      }
      case CountExpr() => "result.count"
      case CountDistinctExpr(_) => "result.count" // TODO: exclude duplicates
      case CountNotNullExpr(_) => "result.count"  // TODO: NULLs are not supported now
      case AvgExpr(opd) => "result.avg(" + currScope.name + " => " + generateExpr(opd) + ")"
      case SumExpr(opd) => "result.sum(" + currScope.name + " => " + generateExpr(opd) + ")"
      case MaxExpr(opd) => "result.max(" + currScope.name + " => " + generateExpr(opd) + ")"
      case MinExpr(opd) => "result.min(" + currScope.name + " => " + generateExpr(opd) + ")"
      case SubstrExpr(str, from, len) => generateExpr(str) + ".substring(" + generateExpr(from) + ", " + generateExpr(from) + " + " + generateExpr(len) + ")"
      case CaseWhenExpr(list) => generateCaseWhen(list, 0)
      case InListExpr(sel, lst) => "(" + lst.map(alt => (generateExpr(sel) + " === " + generateExpr(alt))).mkString(" || ") + ")"
      case InExpr(sel, query) => {
        val saveIndent = indent
        indent += "\t"
        val subselect ="(" + generateOperator(query.operator) + ".where(e => e == " + generateExpr(sel) + ").count !== 0)"
        indent = saveIndent
        subselect
      }
      case FuncExpr(name, args) => name + "(" + args.map(p => generateExpr(p)).mkString(", ") + ")"
    }
  }

  implicit class TypeImplicitCasts(left: ColumnType) {
    def |(right: ColumnType): ColumnType = {
      if (left == right) left
      else if (left == StringType || right == StringType) StringType
      else if (left == DoubleType || right == DoubleType) DoubleType
      else if (left == IntType || right == IntType) IntType
      else throw SqlException("Incompatible types " + left.sqlName + " and " + right.sqlName)
    }
  }

  def getExprType(expr: Expression): ColumnType = {
    expr match {
      case AndExpr(l, r) => BoolType
      case OrExpr(l, r) => BoolType
      case AddExpr(l, r) => getExprType(l) | getExprType(r)
      case SubExpr(l, r) => getExprType(l) | getExprType(r)
      case MulExpr(l, r) => getExprType(l) | getExprType(r)
      case DivExpr(l, r) => getExprType(l) | getExprType(r)
      case NeExpr(l, r) => BoolType
      case EqExpr(l, r) => BoolType
      case LeExpr(l, r) => BoolType
      case LtExpr(l, r) => BoolType
      case GtExpr(l, r) => BoolType
      case GeExpr(l, r) => BoolType
      case LikeExpr(l, r) => BoolType
      case InExpr(l, r) => BoolType
      case ExistsExpr(_) => BoolType
      case NegExpr(opd) => getExprType(opd)
      case NotExpr(_) => BoolType
      case CountExpr() => IntType
      case CountNotNullExpr(_) => IntType
      case CountDistinctExpr(_) => IntType
      case AvgExpr(_) => DoubleType
      case SubstrExpr(str,from,len) => StringType
      case SumExpr(agg) => getExprType(agg)
      case MaxExpr(agg) => getExprType(agg)
      case MinExpr(agg) => getExprType(agg)
      case CaseWhenExpr(list) => getExprType(list(1))
      case Literal(v, t) => t
      case CastExpr(e, t) => t
      case c: ColumnRef => lookup(c).column.ctype
      case SelectExpr(s) => DoubleType
      case FuncExpr(nume, args) => DoubleType
    }
  }

  def buildTree(elems: Seq[String], lpar: String = "(", rpar: String = ")", i: Int = 0): String = {
    val n = elems.length
    if (i < n - 2) lpar + elems(i) + ", " + buildTree(elems, lpar, rpar, i + 1)
    else if (n >= 2) lpar + elems(i) + ", " + elems(i + 1) + (rpar * (n - 1))
    else if (n != 0) elems(i)
    else "()"
  }

  def generateExprList(list: ExprList): String = buildTree(list.map(expr => generateExpr(expr)), "Pair(")

  def resolveKey(key: Expression): Option[Binding] = {
    key match {
      case ColumnRef(table, name) => currScope.ctx.resolve(table, name)
      case _ => throw SqlException("Unsupported join condition")
    }
  }

  def extractKey(on: Expression): String = {
    on match {
      case AndExpr(l, r) => "Pair(" + extractKey(l) + ", " + extractKey(r) + ")"
      case EqExpr(l, r) => (resolveKey(l), resolveKey(r)) match {
        case (Some(_), Some(_)) => throw SqlException("Ambiguous reference to column")
        case (Some(b), None) => b.scope + b.path
        case (None, Some(b)) => b.scope + b.path
        case (None, None) => throw SqlException("Failed to locate column in join condition")
      }
    }
  }

  def generateJoinKey(table: Operator, on: Expression): String = {
    pushContext(table)
    val result = currScope.name + " => " + extractKey(on)
    popContext()
    result
  }

  def generateLambdaExpr(table: Operator, exp: Expression): String = {
    pushContext(table)
    val result = currScope.name + " => " + generateExpr(exp)
    popContext()
    result
  }

  def generateLambdaExprList(table: Operator, exps: ExprList): String = {
    pushContext(table)
    val result = currScope.name + " => " + generateExprList(exps)
    popContext()
    result
  }


  def isAggregate(agg: Expression): Boolean = {
    agg match {
      case CountExpr() => true
      case CountNotNullExpr(_) => true
      case CountDistinctExpr(_) => true
      case AvgExpr(_) => true
      case SumExpr(_) => true
      case MaxExpr(_) => true
      case MinExpr(_) => true
      case AddExpr(l, r) => isAggregate(l) || isAggregate(r)
      case SubExpr(l, r) => isAggregate(l) || isAggregate(r)
      case MulExpr(l, r) => isAggregate(l) || isAggregate(r)
      case DivExpr(l, r) => isAggregate(l) || isAggregate(r)
      case NegExpr(opd) => isAggregate(opd)
      case _ => false
    }
  }


  def generateAggOperand(agg: Expression): String = {
    agg match {
      case CountExpr() => "1"
      case CountNotNullExpr(_) => "1"
      case CountDistinctExpr(_) => "1"
      case AvgExpr(opd) => generateExpr(opd)
      case SumExpr(opd) => generateExpr(opd)
      case MaxExpr(opd) => generateExpr(opd)
      case MinExpr(opd) => generateExpr(opd)
    }
  }

  def aggCombine(agg: Expression, s1: String, s2: String): String = {
    agg match {
      case CountExpr() => s"""$s1 + $s2"""
      case CountNotNullExpr(_) => s"""$s1 + $s2"""
      case CountDistinctExpr(_) => s"""$s1 + $s2"""
      case AvgExpr(opd) => s"""$s1 + $s2"""
      case SumExpr(opd) => s"""$s1 + $s2"""
      case MaxExpr(opd) => s"""if ($s1 > $s2) $s1 else $s2"""
      case MinExpr(opd) => s"""if ($s1 < $s2) $s1 else $s2"""
    }
  }

  def getAggPath(columns: ExprList, aggregates: ExprList, n: Int): String = {
    var aggIndex = 0
    for (i <- 0 until n) {
      if (isAggregate(columns(i))) aggIndex += 1
    }
    currScope.name + ".tail" + indexToPath(aggIndex, aggregates.length)
  }

  def matchExpr(col: Expression, exp: Expression): Boolean = {
    col == exp || (exp match {
      case ColumnRef(table, name) if table.isEmpty => name == col.alias
      case _ => false
    })
  }

  def generateAggResult(columns: ExprList, aggregates: ExprList, gby: ExprList, i: Int, count: Int): String = {
    columns(i) match {
      case CountExpr() => getAggPath(columns, aggregates, i)
      case CountNotNullExpr(_) => getAggPath(columns, aggregates, i)
      case CountDistinctExpr(_) => getAggPath(columns, aggregates, i)
      case AvgExpr(opd) => s"""(${getAggPath(columns, aggregates, i)}.toDouble / ${currScope.name}.tail${indexToPath(count, aggregates.length)}.toDouble)"""
      case SumExpr(opd) => getAggPath(columns, aggregates, i)
      case MaxExpr(opd) => getAggPath(columns, aggregates, i)
      case MinExpr(opd) => getAggPath(columns, aggregates, i)
      case c: ColumnRef => {
        val keyIndex = gby.indexWhere(k => matchExpr(c, k))
        if (keyIndex < 0) throw SqlException("Unsupported group-by clause")
        currScope.name + ".head" + indexToPath(keyIndex, gby.length)
      }
    }
  }

  def ref(e: Expression): ColumnRef = {
    e match {
      case c: ColumnRef => c
      case _ => throw SqlException("Column reference expected")
    }
  }

  def groupBy(agg: Operator, gby: ExprList): String = {
    agg match {
      case Project(p, columns)  => {
        var aggregates = columns.filter(e => isAggregate(e))
        var countIndex = aggregates.indexWhere(e => e.isInstanceOf[CountExpr])
        if (countIndex < 0 && aggregates.exists(e => e.isInstanceOf[AvgExpr])) {
          countIndex = aggregates.length
          aggregates = aggregates :+ CountExpr()
        }
        pushContext(p)
        val aggTypes = buildTree(aggregates.map(agg => getExprType(agg).scalaName))
        val groupBy = buildTree(gby.map(col => {
          val binding = lookup(ref(col))
          binding.scope + binding.path }), "Pair(")
        val map = buildTree(aggregates.map(agg => generateAggOperand(agg)), "Pair(")
        val reduce = if (aggregates.length == 1) aggCombine(aggregates(0), "s1", "s2") else Array.tabulate(aggregates.length)(i => aggCombine(aggregates(i), "s1._" + (i+1), "s2._" + (i+1))).mkString(",")
        val aggResult = buildTree(Array.tabulate(columns.length)(i => generateAggResult(columns, aggregates, gby, i, countIndex)), "Pair(")
        val result = s"""ReadOnlyTable(${generateOperator(p)}
           |$indent.mapReduce(${currScope.name} => Pair(${groupBy}, ${map}),
           |$indent\t(s1: Rep[${aggTypes}], s2: Rep[${aggTypes}]) => (${reduce})).toArray.map(${currScope.name} => ${aggResult}))""".stripMargin
        popContext()
        result
      }
      case _ => throw SqlException("Unsupported group-by clause")
    }
  }

  def isGrandAggregate(columns: ExprList): Boolean = {
    !columns.exists(e => !isAggregate(e))
  }

  def and(left: Expression, right: Expression) = {
    (left, right) match { 
      case (l:Literal, r:Expression) => r
      case (l:Expression, r:Literal) => l
      case (l:Expression, r:Expression) => AndExpr(l, r)
    }
  }

  def depends(on: Operator, subquery: Operator): Boolean = {
    subquery match { 
      case Join(outer, inner, cond) => depends(on, outer) || depends(on, inner)
      case Scan(t) => false
      case OrderBy(p, by) => depends(on, p) || using(on, by)
      case GroupBy(p, by) => depends(on, p) || using(on, by)
      case Filter(p, predicate) => depends(on, p) || using(on, predicate)
      case Project(p, columns) => depends(on, p) || using(on, columns)
      case TableAlias(t, a) =>  depends(on, t)
      case SubSelect(p) => depends(on, p)
      case _ => false
    }
  }

  def using(op: Operator, list: ExprList): Boolean = list.exists(e => using(op, e))

  def using(op: Operator, predicate: Expression): Boolean = {
    predicate match {
      case AndExpr(l, r) => using(op, l) || using(op, r)
      case OrExpr(l, r) =>  using(op, l) || using(op, r)
      case AddExpr(l, r) => using(op, l) || using(op, r)
      case SubExpr(l, r) => using(op, l) || using(op, r)
      case MulExpr(l, r) => using(op, l) || using(op, r)
      case DivExpr(l, r) => using(op, l) || using(op, r)
      case EqExpr(l, r) => using(op, l) || using(op, r)
      case NeExpr(l, r) => using(op, l) || using(op, r)
      case LeExpr(l, r) => using(op, l) || using(op, r)
      case LtExpr(l, r) => using(op, l) || using(op, r)
      case GtExpr(l, r) => using(op, l) || using(op, r)
      case GeExpr(l, r) => using(op, l) || using(op, r)
      case ExistsExpr(q) => using(op, q)
      case LikeExpr(l, r) => using(op, l) || using(op, r)
      case NegExpr(opd) => using(op, opd)
      case NotExpr(opd) => using(op, opd)
      case Literal(v, t) => false
      case CastExpr(exp, typ) => using(op, exp)
      case ColumnRef(table, name) => buildContext(op).resolve(table, name).isDefined
      case SelectExpr(s) => depends(op, s.operator)
      case CountExpr() => false
      case CountDistinctExpr(opd) => using(op, opd)
      case CountNotNullExpr(opd) => using(op, opd)
      case AvgExpr(opd) => using(op, opd)
      case SumExpr(opd) => using(op, opd)
      case MaxExpr(opd) => using(op, opd)
      case MinExpr(opd) => using(op, opd)
      case SubstrExpr(str, from, len) => using(op, str) || using(op, from) || using(op, len)
      case CaseWhenExpr(list) => using(op, list)
      case InListExpr(sel, lst) => using(op, sel) || using(op, lst)
      case InExpr(sel, query) => using(op, sel) || depends(op, query.operator)
      case FuncExpr(name, args) => using(op, args)
      case _ => false
    }
  }


  def optimize(op: Operator, predicate: Expression): (Operator,Expression) = {
    op match { 
      case Join(outer, inner, on) => {
        if (!using(inner, predicate)) (Join(Filter(outer, predicate), inner, on), Literal(true, BoolType))
        else predicate match {
          case AndExpr(l, r) => {
            val (jr, cr) = optimize(op, r) 
            val (jl, cl) = optimize(jr, l) 
            (jl, and(cl, cr))
          }
          case _ => (op, predicate)
        }
      }
      case _ => (op, predicate)      
    }
  }

  def generateOperator(op:Operator): String = {
    op match {
      case Join(outer, inner, on) => generateOperator(outer) + s"""\n$indent.join(${generateOperator(inner)})(${generateJoinKey(outer, on)}, ${generateJoinKey(inner, on)})"""
      case Scan(t) =>
        currMethod.explicitArgs.find(arg => arg.tpe.toString ==  t.name) match { // TODO: global lookup
          case Some(arg) => arg.name
          case _ => t.name.toLowerCase
        }
      case OrderBy(p, by) => generateOperator(p) + s"""\n$indent.orderBy(${generateLambdaExprList(p, by)})"""
      case GroupBy(p, by) => groupBy(p, by)
      case Filter(p, predicate) => {
        val (joins, conjuncts) = optimize(p, predicate)
        conjuncts match {
          case Literal(_, _) => generateOperator(joins)
          case _ => generateOperator(joins) + s"""\n$indent.where(${generateLambdaExpr(p, conjuncts)})"""
        }
      }
      case Project(p, columns) =>
        if (isGrandAggregate(columns)) {
          pushContext(p)
          val agg = s"""{ val result = ${generateOperator(p)} ; (${generateExprList(columns)}) }"""
          popContext()
          agg
        } else {
          generateOperator(p) + s"""\n$indent.select(${generateLambdaExprList(p, columns)})"""
        }
      case TableAlias(t, a) => generateOperator(t)
      case SubSelect(p) =>
        val saveIdent = indent
        indent = indent + "\t"
        val subquery = generateOperator(p)
        indent = saveIdent
        subquery
    }
  }

  def tableToArray(op:Operator):String = {
    op match {
      case OrderBy(p, by) => ""
      case Project(p, c) if (isGrandAggregate(c)) => ""
      case _ => ".toArray"
    }
  }

  def operatorType(op: Operator): String = {
    op match {
      case Join(outer, inner, on) => "(" + operatorType(outer) + ", " + operatorType(inner) + ")"
      case Scan(t) => buildTree(t.schema.map(c => c.ctype.scalaName))
      case OrderBy(p, by) => operatorType(p)
      case GroupBy(p, by) => operatorType(p)
      case Filter(p, predicate) => operatorType(p)
      case Project(p, columns) => {
        pushContext(p)
        val projection = buildTree(columns.map(c => getExprType(c).scalaName))
        popContext()
        projection
      }
      case TableAlias(t, a) => generateOperator(t)
      case SubSelect(p) => operatorType(p)
    }
  }

  def resultType(query: SelectStmt): String = {
    query.operator match {
      case OrderBy(p, by) => "Arr[" + operatorType(p) + "]"
      case Project(p, c) if (isGrandAggregate(c)) => "Rep[" + operatorType(p) + "]"
      case _ => "Arr[" + operatorType(query.operator) + "]"
    }
  }

  def parseType(t: ColumnType): String = {
    t match {
      case StringType => ""
      case DateType => ".toDate"
      case _ => ".to" + t.scalaName
    }
  }

  def generateTable(table:Table): String = {
    val columns = table.schema
    val n_columns = columns.length
    val typeName = table.name.capitalize
    val typeDef = buildTree(columns.map(c => c.ctype.scalaName))
    val classDef = Array.tabulate(n_columns)(i => "  def " + columns(i).name + " = self" + indexToPath(i, n_columns)).mkString("\n")
    val parse = buildTree(Array.tabulate(n_columns)(i => "c(" + i + ")" + parseType(columns(i).ctype)), "Pair(")
    val pairTableDef = buildTree(columns.map(c => "Table.create[" + c.ctype.scalaName + "](tableName + \"." + c.name + "\")"), "PairTable.create(")
    s"""type $typeName = $typeDef
       |
       |def create$typeName(tableName: Rep[String]) = $pairTableDef
       |
       |def parse$typeName(c: Arr[String]): Rep[$typeName] = $parse
       |
       |implicit class ${typeName}_class(self: Rep[$typeName]) {
       |$classDef
       |}
       |
       |""".stripMargin
  }

  def generateIndex(index:CreateIndexStmt): String = {
    currScope = Scope(TableContext(index.table), Some(currScope), 0, "r")
    val result = s"""def ${index.name}(${currScope.name}: Rep[${index.table.name.capitalize}]) = ${buildTree(index.key.map(part => {currScope.name} + lookup(ColumnRef("", part)).path), "Pair(")}"""
    popContext()
    result
  }
}
