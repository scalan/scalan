package scalan.meta
/**
 * Created by knizhnik on 1/14/15.
 */
import ScalanAst._
import SqlAST._

trait SqlCompiler extends SqlParser {
  case class Scope(var ctx: Context, outer: Option[Scope], nesting: Int, name: String) {
    def lookup(col: ColumnRef): Binding = {
      ctx.resolve(col) match {
        case Some(b) => b
        case None => {
          outer match {
            case Some(s: Scope) => s.lookup(col)
            case _ =>
              throw SqlException( s"""Failed to lookup column ${col.asString}""")
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

    statements.map {
      case s: CreateIndexStmt => generateIndex(s)
      case s: CreateTableStmt => generateTable(s.table)
      case s => throw new NotImplementedError(s"Cannot generate schema for statement $s")
    }.mkString("\n\n")
  }

  var currMethod:SMethodDef = null

  def generateQuery(m: SMethodDef): String = {
    val args = m.allArgs.map(arg => arg.name + ": " + arg.tpe).mkString(", ")
    val apply = m.body.get.asInstanceOf[SApply]
    val sql = apply.argss(0)(0).asInstanceOf[SLiteral].value
    apply.fun match {
      case SLiteral("sql") =>
        val select = parseSelect(sql)
        val op = select.operator
        currMethod = m
        s"""    type ${m.name}_Result = ${resultType(select)}
           |
           |    override def ${m.name}(${args}) = ${generateOperator(op)}${tableToArray(op)}""".stripMargin
      case SLiteral("ddl") =>
        generateSchema(sql)
    }
  }

  def tablesInNestedSelects(e: Expression): Set[Table] = {
    e match {
      case BinOpExpr(op, l, r) => tablesInNestedSelects(l) ++ tablesInNestedSelects(r)
      case ExistsExpr(q) => tablesInNestedSelects(q)
      case LikeExpr(l, r, _) => tablesInNestedSelects(l) ++ tablesInNestedSelects(r)
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
      case Join(outer, inner, _, _) => tables(outer) ++ tables(inner)
      case Scan(t) => Set(t)
      case OrderBy(p, by) => tables(p)
      case GroupBy(p, by) => tables(p)
      case Filter(p, predicate) => tables(p) ++ tablesInNestedSelects(predicate)
      case Project(p, columns) => tables(p)
      case TableAlias(t, a) => tables(t)
      case SubSelect(p) => tables(p)
      case _ => throw new NotImplementedError(s"tables($op)")
    }
  }

  def indexToPath(i: Int, n: Int) = {
    if (n > 1 && (n <= 8 || i < 7)) List("_" + (i+1))
    else {
      val path = List.fill(i)("tail")
      if (i == n - 1) path else path :+ "head"
    }
  }

  def pathString(path: List[String]): String = pathString(currScope.name, path)
  def pathString(scope: String, path: List[String]): String = scope + "." + path.mkString(".")

  case class Binding(scope: String, path: List[String], column: Column) {
    def asScalaCode = pathString(scope, path)
  }

  abstract class Context {
    def resolve(ref: ColumnRef): Option[Binding]
    val scope = currScope
  }

  class GlobalContext() extends Context {
    def resolve(ref: ColumnRef): Option[Binding] = None
  }
  
  case class TableContext(table: Table) extends Context {
    def resolve(ref: ColumnRef): Option[Binding] = {
      if (ref.table.isEmpty || ref.table == Some(table.name)) {
        val i = table.schema.indexWhere(c => c.name == ref.name)
        if (i >= 0) {
          //val path = indexToPath(i, table.schema.length);
          val path = List(ref.name)
          Some(Binding(scope.name, path, table.schema(i)))
        } else None
      } else None
    }
  }

  case class JoinContext(outer: Context, inner: Context) extends Context {
    def resolve(ref: ColumnRef): Option[Binding] = {
      (outer.resolve(ref), inner.resolve(ref)) match {
        case (Some(b), None) =>
          Some(Binding(b.scope, "head" :: b.path, b.column))
        case (None, Some(b)) =>
          Some(Binding(b.scope, "tail" :: b.path, b.column))
        case (Some(_), Some(_)) =>
          throw SqlException(s"""Ambiguous reference to ${ref.asString}""")
        case _ => None
      }
    }
  }

  case class AliasContext(parent: Context, alias: String) extends Context {
    def resolve(ref: ColumnRef): Option[Binding] =
      ref.table match {
        case None =>
          parent.resolve(ref)
        case Some(`alias`) =>
          parent.resolve(ColumnRef(None, ref.name))
        case _ =>
          None
      }
  }

  case class ProjectContext(parent: Context, columns: List[ProjectionColumn]) extends Context {
    def resolve(ref: ColumnRef): Option[Binding] = {
      val i = columns.indexWhere {
        case ProjectionColumn(c, alias) => matchExpr(c, alias, ref)
      }
      if (i >= 0) {
        val saveScope = currScope
        currScope = Scope(parent, scope.outer, scope.nesting, scope.name)
        val cType =  getExprType(columns(i).expr)
        currScope = saveScope
        Some(Binding(scope.name, indexToPath(i, columns.length), Column(ref.name, cType)))
      }
      else None
    }
  }

  def buildContext(op: Operator): Context = {
    op match {
      case Join(outer, inner, _, _) => JoinContext(buildContext(outer), buildContext(inner))
      case Scan(t) => TableContext(t)
      case OrderBy(p, by) => buildContext(p)
      case GroupBy(p, by) => buildContext(p)
      case Filter(p, predicate) => buildContext(p)
      case Project(p, columns) => ProjectContext(buildContext(p), columns)
      case TableAlias(p, a) => AliasContext(buildContext(p), a)
      case SubSelect(p) => buildContext(p)
      case _ => throw new NotImplementedError(s"tables($op)")
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

  def patternMatch(text: Expression, pattern: Expression, escape: Option[Expression]): String = {
    val left = generateExpr(text)
    pattern match {
      case Literal(v, t) if (t == StringType) =>
        assert(escape.isEmpty, "escape currently not supported")
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
      case BinOpExpr(op, l, r) =>
        // Ignore behavior on nulls currently
        val opStr = op match {
          case And => "&&"
          case Or => "||"
          case Plus => "+"
          case Minus => "-"
          case Times => "*"
          case Divide => divOp(expr)
          case Modulo => "%"
          case Is => "==="
          case Eq => "==="
          case Less => "<"
          case LessEq => "<="
          case Greater => ">"
          case GreaterEq => ">="
          case Concat => "concat"
        }
        val lCode = generateExpr(l)
        val rCode = generateExpr(r)
        s"($lCode $opStr $rCode)"
      case ExistsExpr(q) => "(" + generateExpr(q) + ".count !== 0)"
      case LikeExpr(l, r, escape) =>
        patternMatch(l, r, escape)
      case NegExpr(opd) => "-" + generateExpr(opd)
      case NotExpr(opd) => "!" + generateExpr(opd)
      case Literal(v, t) => "toRep(" + printValue(v, t) + ")"
      case CastExpr(exp, typ) => generateExpr(exp) + (if (typ == StringType) ".toStr" else ".to" + typ.scalaName)
      case c: ColumnRef => {
        val binding = lookup(c)
        binding.asScalaCode
      }
      case SelectExpr(s) => {
        val saveIndent = indent
        indent += "\t"
        val subselect = "(" + generateOperator(s.operator) + ")"
        indent = saveIndent
        subselect
      }
      // TODO currently ignores distinct
      case AggregateExpr(op, _, opd) =>
        op match {
          case Count => "result.count"
          case Avg => "result.avg(" + currScope.name + " => " + generateExpr(opd) + ")"
          case Sum => "result.sum(" + currScope.name + " => " + generateExpr(opd) + ")"
          case Max => "result.max(" + currScope.name + " => " + generateExpr(opd) + ")"
          case Min => "result.min(" + currScope.name + " => " + generateExpr(opd) + ")"
        }
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
      case _ => throw new NotImplementedError(s"generateExpr($expr)")
    }
  }

  def commonType(left: ColumnType, right: ColumnType): ColumnType = {
    if (left == right)
      left
    else if (left == DoubleType || right == DoubleType)
      DoubleType
    else if (left == IntType || right == IntType)
      IntType
    else if (left == StringType || right == StringType)
      StringType
    else throw SqlException("Incompatible types " + left.sqlName + " and " + right.sqlName)
  }

  def getExprType(expr: Expression): ColumnType = {
    expr match {
      case BinOpExpr(op, l, r) =>
        op match {
          case _: LogicOp | _: ComparisonOp =>
            BoolType
          case _: ArithOp =>
            commonType(getExprType(l), getExprType(r))
          case Concat => StringType
        }
      case LikeExpr(l, r, escape) => BoolType
      case InExpr(l, r) => BoolType
      case ExistsExpr(_) => BoolType
      case NegExpr(opd) => getExprType(opd)
      case NotExpr(_) => BoolType
      case AggregateExpr(Count, _, _) => IntType
      case AggregateExpr(Avg, _, _) => DoubleType
      case AggregateExpr(Sum | Max | Min, _, opd) => getExprType(opd)
      case SubstrExpr(str,from,len) => StringType
      case CaseWhenExpr(list) => getExprType(list(1))
      case Literal(v, t) => t
      case CastExpr(e, t) => t
      case c: ColumnRef => lookup(c).column.ctype
      case SelectExpr(s) => DoubleType
      case FuncExpr(name, args) => funcType(name, args)
      case _ => throw new NotImplementedError(s"getExprType($expr)")
    }
  }

  /** Returns the type of `FuncExpr(name, args)`. Override if the type depends on args, use
    * `registerFunctionType` otherwise. Default is `DoubleType`
    */
  def funcType(name: String, args: List[Expression]) = name match {
    case "abs" | "trunc" | "truncate" | "round" | "power" | "mod" | "sign" | "ceiling" | "ceil" | "floor" | "nullif" =>
      getExprType(args.head)
    case "coalesce" =>
      args.map(getExprType).reduce(commonType)
    case _ =>
      funcTypes.getOrElse(name,
        throw new IllegalArgumentException(s"Unknown return type for $name(${args.mkString(", ")}). Override `SqlCompiler.funcType` or call `registerFunctionType` if the type doesn't depend on arguments"))
  }

  def registerFunctionType(name: String, tpe: ColumnType) =
    funcTypes.update(name, tpe)

  private val funcTypes = collection.mutable.Map.empty[String, ColumnType]

  // https://en.wikibooks.org/wiki/SQL_Dialects_Reference
  Seq(
    IntType -> Seq(
      "length", "bit_length", "char_length", "octet_length", "width_bucket", "ascii", "instr"
    ),
    DoubleType -> Seq(
      "asin", "acos", "atan", "atan2", "sin", "cos", "tan", "cot", "sinh", "cosh", "tanh", "atanh",
      "sqrt", "exp", "ln", "log", "log10", "rand"
    ),
    StringType -> Seq(
      "chr", "char", "concat", "hex", "lower", "upper", "lcase", "ucase", "lpad", "rpad",
      "trim", "ltrim", "rtrim", "left", "right", "repeat", "reverse", "space", "substring", "substr",
      "replace", "initcap", "translate", "quote", "soundex", "md5", "sha1"
    ),
    DateType -> Seq("current_date", "date"),
    TimeType -> Seq("current_time", "time"),
    TimestampType -> Seq("current_timestamp", "now")
  ).foreach {
    case (tpe, funs) => funs.foreach(registerFunctionType(_, tpe))
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
      case ref: ColumnRef => currScope.ctx.resolve(ref)
      case _ => throw SqlException("Unsupported join condition")
    }
  }

  def extractKey(spec: JoinSpec): String = spec match {
    case On(on) => extractKey(on)
    case _ => throw new NotImplementedError(s"extractKey($spec)")
  }

  def extractKey(on: Expression): String = {
    on match {
      case BinOpExpr(And, l, r) => "Pair(" + extractKey(l) + ", " + extractKey(r) + ")"
      case BinOpExpr(Eq, l, r) => (resolveKey(l), resolveKey(r)) match {
        case (Some(_), Some(_)) => throw SqlException("Ambiguous reference to column")
        case (Some(b), None) => b.asScalaCode
        case (None, Some(b)) => b.asScalaCode
        case (None, None) => throw SqlException("Failed to locate column in join condition")
      }
      case _ => throw new NotImplementedError(s"extractKey($on)")
    }
  }

  def generateJoinKey(table: Operator, spec: JoinSpec): String = {
    pushContext(table)
    val result = currScope.name + " => " + extractKey(spec)
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

  def isAggregate(column: ProjectionColumn): Boolean = isAggregate(column.expr)

  def isAggregate(expr: Expression): Boolean =
    expr match {
      case _: AggregateExpr =>
        true
      case BinOpExpr(_, l, r) =>
        isAggregate(l) || isAggregate(r)
      case NegExpr(opd) =>
        isAggregate(opd)
      case NotExpr(opd) =>
        isAggregate(opd)
      case _ => false
    }

  def generateAggOperand(agg: Expression): String = {
    agg match {
      case AggregateExpr(op, _, opd) =>
        op match {
          case Count => "1"
          case _ => generateExpr(opd)
        }
      case _ => throw new NotImplementedError(s"generateAggOperand($agg)")
    }
  }

  def aggCombine(agg: Expression, s1: String, s2: String): String = {
    agg match {
      case AggregateExpr(op, _, opd) =>
        op match {
          case Count | Avg | Sum =>
            s"""$s1 + $s2"""
          case Max =>
            s"""if ($s1 > $s2) $s1 else $s2"""
          case Min =>
            s"""if ($s1 < $s2) $s1 else $s2"""
        }
      case _ => throw new NotImplementedError(s"aggCombine($agg, $s1, $s2)")
    }
  }

  def getAggPath(columns: List[ProjectionColumn], length: Int, n: Int): String = {
    var aggIndex = 0
    for (i <- 0 until n) {
      if (isAggregate(columns(i))) aggIndex += 1
    }
    pathString("tail" :: indexToPath(aggIndex, length))
  }

  def matchExpr(col: Expression, alias: Option[String], exp: Expression): Boolean = {
    col == exp || (exp match {
      case ColumnRef(None, name) =>
        alias == Some(name)
      case _ => false
    })
  }

  def generateAggResult(columns: List[ProjectionColumn], length: Int, gby: ExprList, i: Int, count: Int, expr: Expression): String = {
    lazy val aggPath = getAggPath(columns, length, i)
    expr match {
      case AggregateExpr(op, _, opd) =>
        op match {
          case Count | Sum | Max | Min =>
            aggPath
          case Avg =>
            val countPath = pathString("tail" :: indexToPath(count, length))
            s"""($aggPath.toDouble / $countPath.toDouble)"""
        }
      case c: ColumnRef => {
        val keyIndex = gby.indexWhere {
          k => matchExpr(c, None, k)
        }
        if (keyIndex < 0)
          throw SqlException("Unsupported group-by clause")
        else
          pathString("head" :: indexToPath(keyIndex, gby.length))
      }
      case _ => throw new NotImplementedError(s"generateAggResult($columns, $length, $gby, $i, $count)")
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
        var aggregates = columns.filter(isAggregate)
        var countIndex = aggregates.indexWhere {
          case ProjectionColumn(CountAllExpr, _) => true
          case _ => false
        }
        if (countIndex < 0 && aggregates.exists {
          case ProjectionColumn(AggregateExpr(Avg, _, _), _) => true
          case _ => false
        }) {
          countIndex = aggregates.length
          aggregates = aggregates :+ ProjectionColumn(CountAllExpr, None)
        }
        val numberOfAggregates = aggregates.length
        pushContext(p)
        val aggTypes = buildTree(aggregates.map(agg => getExprType(agg.expr).scalaName))
        val groupBy = buildTree(gby.map(col => {
          val binding = lookup(ref(col))
          binding.asScalaCode
        }), "Pair(")
        val map = buildTree(aggregates.map(agg => generateAggOperand(agg.expr)), "Pair(")
        val reduce = if (numberOfAggregates == 1)
          aggCombine(aggregates(0).expr, "s1", "s2")
        else
          aggregates.indices.map(i => aggCombine(aggregates(i).expr, "s1._" + (i+1), "s2._" + (i+1))).mkString(",")
        val aggResult = buildTree(columns.indices.map(i => generateAggResult(columns, numberOfAggregates, gby, i, countIndex, columns(i).expr)), "Pair(")
        val result = s"""ReadOnlyTable(${generateOperator(p)}
           |$indent.mapReduce(${currScope.name} => Pair(${groupBy}, ${map}),
           |$indent\t(s1: Rep[${aggTypes}], s2: Rep[${aggTypes}]) => (${reduce})).toArray.map(${currScope.name} => ${aggResult}))""".stripMargin
        popContext()
        result
      }
      case _ => throw SqlException("Unsupported group-by clause")
    }
  }

  def isGrandAggregate(columns: List[ProjectionColumn]): Boolean = {
    columns.forall(isAggregate)
  }

  def depends(on: Operator, subquery: Operator): Boolean = {
    subquery match {
      case Join(outer, inner, _, _) => depends(on, outer) || depends(on, inner)
      case Scan(t) => false
      case OrderBy(p, by) => depends(on, p) || using(on, by.map(_.expr))
      case GroupBy(p, by) => depends(on, p) || using(on, by)
      case Filter(p, predicate) => depends(on, p) || using(on, predicate)
      case Project(p, columns) => depends(on, p) || using(on, columns.map(_.expr))
      case TableAlias(t, a) =>  depends(on, t)
      case SubSelect(p) => depends(on, p)
      case _ => false
    }
  }

  def using(op: Operator, list: ExprList): Boolean = list.exists(e => using(op, e))

  def using(op: Operator, predicate: Expression): Boolean = {
    predicate match {
      case BinOpExpr(_, l, r) => using(op, l) || using(op, r)
      case ExistsExpr(q) => using(op, q)
      case LikeExpr(l, r, escape) =>
        using(op, l) || using(op, r) || escape.exists(using(op, _))
      case NegExpr(opd) => using(op, opd)
      case NotExpr(opd) => using(op, opd)
      case Literal(v, t) => false
      case CastExpr(exp, typ) => using(op, exp)
      case ref: ColumnRef => buildContext(op).resolve(ref).isDefined
      case SelectExpr(s) => depends(op, s.operator)
      case AggregateExpr(_, _, opd) => using(op, opd)
      case SubstrExpr(str, from, len) => using(op, str) || using(op, from) || using(op, len)
      case CaseWhenExpr(list) => using(op, list)
      case InListExpr(sel, lst) => using(op, sel) || using(op, lst)
      case InExpr(sel, query) => using(op, sel) || depends(op, query.operator)
      case FuncExpr(name, args) => using(op, args)
      case _ => false
    }
  }

  def and(left: Expression, right: Expression) = {
    if (left == Literal(true, BoolType) || right == Literal(false, BoolType))
      right
    else if (right == Literal(true, BoolType) || left == Literal(false, BoolType))
      left
    else
      BinOpExpr(And, left, right)
  }

  def optimize(op: Operator, predicate: Expression): (Operator, Expression) = {
    op match { 
      case Join(outer, inner, joinType, joinSpec) =>
        if (!using(inner, predicate))
          (Join(Filter(outer, predicate), inner, joinType, joinSpec), Literal(true, BoolType))
        else
          predicate match {
            case BinOpExpr(And, l, r) =>
              val (jr, cr) = optimize(op, r)
              val (jl, cl) = optimize(jr, l)
              (jl, and(cl, cr))
            case _ => (op, predicate)
          }
      case _ => (op, predicate)
    }
  }

  def generateOperator(op:Operator): String = {
    op match {
      case Join(outer, inner, joinType, spec) =>
        generateOperator(outer) + s"""\n$indent.join(${generateOperator(inner)}, $joinType)(${generateJoinKey(outer, spec)}, ${generateJoinKey(inner, spec)})"""
      case Scan(t) =>
        currMethod.explicitArgs.find(arg =>arg.tpe match { 
          case STraitCall(n1, List(STraitCall(n2, List(p)))) if n1 == "Rep" && n2 == "Table" && p.toString == t.name.capitalize => true
          case _ => false
        }) match { // TODO: global lookup
          case Some(arg) => arg.name
          case _ => t.name.toLowerCase
        }
      case OrderBy(p, by) =>
        // TODO ignores null ordering
        val expressions = by.map {
          case SortSpec(expr, Ascending, _) =>
            expr
          case SortSpec(expr, Descending, _) =>
            // TODO doesn't handle strings properly, but this requires changes in scalan-sql
            NegExpr(expr)
        }
        generateOperator(p) + s"""\n$indent.orderBy(${generateLambdaExprList(p, expressions)})"""
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
          val agg = s"""{ val result = ${generateOperator(p)} ; (${generateExprList(columns.map(_.expr))}) }"""
          popContext()
          agg
        } else {
          generateOperator(p) + s"""\n$indent.select(${generateLambdaExprList(p, columns.map(_.expr))})"""
        }
      case TableAlias(t, a) => generateOperator(t)
      case SubSelect(p) =>
        val saveIdent = indent
        indent = indent + "\t"
        val subquery = generateOperator(p)
        indent = saveIdent
        subquery
      case _ => throw new NotImplementedError(s"generateOperator($op)")
    }
  }

  def tableToArray(op:Operator):String = {
    op match {
      case OrderBy(p, by) => ""
      case Project(p, c) if isGrandAggregate(c) => ""
      case _ => ".toArray"
    }
  }

  def operatorType(op: Operator): String = {
    op match {
      case Join(outer, inner, _, _) => "(" + operatorType(outer) + ", " + operatorType(inner) + ")"
      case Scan(t) => buildTree(t.schema.map(c => c.ctype.scalaName))
      case OrderBy(p, by) => operatorType(p)
      case GroupBy(p, by) => operatorType(p)
      case Filter(p, predicate) => operatorType(p)
      case Project(p, columns) => {
        pushContext(p)
        val projection = buildTree(columns.map(c => getExprType(c.expr).scalaName))
        popContext()
        projection
      }
      case TableAlias(t, a) => generateOperator(t)
      case SubSelect(p) => operatorType(p)
      case _ => throw new NotImplementedError(s"operatorType($op)")
    }
  }

  def resultType(query: SelectStmt): String = {
    query.operator match {
      case OrderBy(p, by) => "Arr[" + operatorType(p) + "]"
      case Project(p, c) if isGrandAggregate(c) => "Rep[" + operatorType(p) + "]"
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
//    val typeDef = buildTree(columns.map(c => c.ctype.scalaName))
    val classDef = Array.tabulate(n_columns)(i => s"  def ${columns(i).name} = ${pathString("self", indexToPath(i, n_columns))}").mkString("\n")
    val parse = buildTree(Array.tabulate(n_columns)(i => "c(" + i + ")" + parseType(columns(i).ctype)), "Pair(")
    val pairTableDef = buildTree(columns.map(c => "Table.create[" + c.ctype.scalaName + "](tableName + \"." + c.name + "\")"), "PairTable.create(")
    s"""
       |    def create$typeName(tableName: Rep[String]) =
       |      $pairTableDef
       |
       |    def parse$typeName(c: Arr[String]): Rep[${typeName}Data] =
       |      $parse
       |
       |""".stripMargin
  }

  def generateIndex(index:CreateIndexStmt): String = {
    currScope = Scope(TableContext(index.table), Some(currScope), 0, "r")
    val body = buildTree(index.key.map(part => pathString(lookup(ColumnRef(None, part)).path)), "Pair(")
    val result = s"""def ${index.name}(${currScope.name}: Rep[${index.table.name.capitalize}]) = $body"""
    popContext()
    result
  }
}
