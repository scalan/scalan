package scalan.meta

import scala.collection.mutable.Map
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.CharArrayReader.EofCh
import scalan.meta.SqlAST._

// see http://savage.net.au/SQL/sql-2003-2.bnf.html for full SQL grammar
trait SqlParser {

  def parseDDL(sql: String) = Grammar.schema(sql)
  def parseSelect(sql: String) = Grammar.select(sql)

  class SqlLexical(val keywords: Seq[String]) extends StdLexical {

    case class FloatLit(chars: String) extends Token {
      override def toString = chars
    }

    reserved ++= keywords.flatMap(w => allCaseVersions(w))

    delimiters +=(
      "@", "*", "+", "-", "<", "=", "<>", "!=", "<=", ">=", ">", "/", "(", ")",
      ",", ";", "%", "{", "}", ":", "[", "]", ".", "&", "|", "^", "~", "<=>"
      )

    override lazy val token: Parser[Token] =
      (identChar ~ (identChar | digit).* ^^ { case first ~ rest => processIdent((first :: rest).mkString)}
        | rep1(digit) ~ ('.' ~> digit.*).? ^^ {
        case i ~ None => NumericLit(i.mkString)
        case i ~ Some(d) => FloatLit(i.mkString + "." + d.mkString)
      }
        | '\'' ~> chrExcept('\'', '\n', EofCh).* <~ '\'' ^^ { case chars => StringLit(chars mkString "")}
        | '"' ~> chrExcept('"', '\n', EofCh).* <~ '"' ^^ { case chars => StringLit(chars mkString "")}
        | '`' ~> chrExcept('`', '\n', EofCh).* <~ '`' ^^ { case chars => Identifier(chars mkString "")}
        | EofCh ^^^ EOF
        | '\'' ~> failure("unclosed string literal")
        | '"' ~> failure("unclosed string literal")
        | delim
        | failure("illegal character")
        )

    override def identChar = letter | elem('_')

    override def whitespace: Parser[Any] =
      (whitespaceChar
        | '/' ~ '*' ~ comment
        | '/' ~ '/' ~ chrExcept(EofCh, '\n').*
        | '#' ~ chrExcept(EofCh, '\n').*
        | '-' ~ '-' ~ chrExcept(EofCh, '\n').*
        | '/' ~ '*' ~ failure("unclosed comment")
        ).*

    /** Generate all variations of upper and lower case of a given string */
    def allCaseVersions(s: String, prefix: String = ""): Stream[String] = {
      if (s == "") {
        Stream(prefix)
      } else {
        allCaseVersions(s.tail, prefix + s.head.toLower) ++
          allCaseVersions(s.tail, prefix + s.head.toUpper)
      }
    }
  }

  object Grammar extends StandardTokenParsers with PackratParsers {
    val builtinTypes = Array(IntType, DoubleType, LongType, StringType, CharType, BoolType, DateType)
    val types = builtinTypes.map(t => (t.sqlName, t)).toMap
    val tables = Map.empty[String, Table]

    def schema(sql: String): Script = phrase(script)(new lexical.Scanner(sql)) match {
      case Success(res, _) => res
      case res => throw SqlException(res.toString)
    }

    def select(sql: String): SelectStmt = phrase(selectStmt)(new lexical.Scanner(sql)) match {
      case Success(res, _) => res
      case res => throw SqlException(res.toString)
    }

    lazy val script: Parser[Script] =
      rep(statement <~ ";") ^^ (list => Script(list: _*))

    lazy val statement: Parser[Statement] = createTableStmt | createIndexStmt | selectStmt

    lazy val createTableStmt: Parser[Statement] =
      CREATE ~> TABLE ~> ident ~ ("(" ~> columns <~ ")") ^^ {
        case name ~ schema => {
          val table = Table(name, schema)
          tables.update(name, table)
          CreateTableStmt(table)
        }
      }

    lazy val createIndexStmt: Parser[Statement] =
      CREATE ~> INDEX ~> ident ~ (ON ~> table) ~ ("(" ~> fieldList <~ ")") ^^ {
        case name ~ table ~ key => CreateIndexStmt(name, table, key)
      }

    lazy val table: Parser[Table] =
      ident ^^ { name => if (tables.contains(name)) tables(name) else throw SqlException("Unknown table " + name)}

    lazy val columns: Parser[Schema] =
      repsep(column, ",") ^^ (list => Schema(list: _*))

    lazy val column: Parser[Column] =
      ident ~ fieldType ^^ { case i ~ t => Column(i, t)}

    lazy val fieldType: Parser[ColumnType] = ident ^^ { t => if (!types.contains(t)) throw SqlException("Not supported type " + t) else types(t)}

    //lazy val ident: Parser[String] = """[\w]+""".r

    lazy val fieldList: Parser[ColumnList] =
      repsep(ident, ",")

    protected case class Keyword(str: String) {
      // TODO better case insensitivity (using RegexParsers?)
      lazy val parser = lexical.allCaseVersions(str).map(x => x: Parser[String]).reduce(_ | _) ^^^ this
    }

    protected implicit def asParser(k: Keyword): Parser[Keyword] = k.parser

    protected val ABS = Keyword("ABS")
    protected val ALL = Keyword("ALL")
    protected val AND = Keyword("AND")
    protected val APPROXIMATE = Keyword("APPROXIMATE")
    protected val AS = Keyword("AS")
    protected val ASC = Keyword("ASC")
    protected val AVG = Keyword("AVG")
    protected val BETWEEN = Keyword("BETWEEN")
    protected val BY = Keyword("BY")
    protected val CACHE = Keyword("CACHE")
    protected val CASE = Keyword("CASE")
    protected val CAST = Keyword("CAST")
    protected val COUNT = Keyword("COUNT")
    protected val CREATE = Keyword("CREATE")
    protected val CROSS = Keyword("CROSS")
    protected val DECIMAL = Keyword("DECIMAL")
    protected val DESC = Keyword("DESC")
    protected val DISTINCT = Keyword("DISTINCT")
    protected val DOUBLE = Keyword("DOUBLE")
    protected val ELSE = Keyword("ELSE")
    protected val END = Keyword("END")
    protected val ESCAPE = Keyword("ESCAPE")
    protected val EXCEPT = Keyword("EXCEPT")
    protected val EXISTS = Keyword("EXISTS")
    protected val FALSE = Keyword("FALSE")
    protected val FIRST = Keyword("FIRST")
    protected val FROM = Keyword("FROM")
    protected val FULL = Keyword("FULL")
    protected val GROUP = Keyword("GROUP")
    protected val HAVING = Keyword("HAVING")
    protected val IF = Keyword("IF")
    protected val IN = Keyword("IN")
    protected val INNER = Keyword("INNER")
    protected val INSERT = Keyword("INSERT")
    protected val INTERSECT = Keyword("INTERSECT")
    protected val INTO = Keyword("INTO")
    protected val INDEX = Keyword("INDEX")
    protected val IS = Keyword("IS")
    protected val JOIN = Keyword("JOIN")
    protected val LAST = Keyword("LAST")
    protected val LEFT = Keyword("LEFT")
    protected val LIKE = Keyword("LIKE")
    protected val LIMIT = Keyword("LIMIT")
    protected val LOWER = Keyword("LOWER")
    protected val MAX = Keyword("MAX")
    protected val MIN = Keyword("MIN")
    protected val NATURAL = Keyword("NATURAL")
    protected val NOT = Keyword("NOT")
    protected val NULL = Keyword("NULL")
    protected val NULLS = Keyword("NULLS")
    protected val ON = Keyword("ON")
    protected val OR = Keyword("OR")
    protected val ORDER = Keyword("ORDER")
    protected val OUTER = Keyword("OUTER")
    protected val OVERWRITE = Keyword("OVERWRITE")
    protected val REGEXP = Keyword("REGEXP")
    protected val RIGHT = Keyword("RIGHT")
    protected val RLIKE = Keyword("RLIKE")
    protected val SELECT = Keyword("SELECT")
    protected val SEMI = Keyword("SEMI")
    protected val SQRT = Keyword("SQRT")
    protected val STRING = Keyword("STRING")
    protected val SUBSTR = Keyword("SUBSTR")
    protected val SUBSTRING = Keyword("SUBSTRING")
    protected val SUM = Keyword("SUM")
    protected val TABLE = Keyword("TABLE")
    protected val THEN = Keyword("THEN")
    protected val TIMESTAMP = Keyword("TIMESTAMP")
    protected val TRUE = Keyword("TRUE")
    protected val UNION = Keyword("UNION")
    protected val UPPER = Keyword("UPPER")
    protected val USING = Keyword("USING")
    protected val WHEN = Keyword("WHEN")
    protected val WHERE = Keyword("WHERE")

    // Use reflection to find the reserved words defined in this class.
    protected val reservedWords =
      this
        .getClass
        .getMethods
        .filter(_.getReturnType == classOf[Keyword])
        .map(_.invoke(this).asInstanceOf[Keyword].str)

    override val lexical = new SqlLexical(reservedWords)

    protected lazy val selectStmt: Parser[SelectStmt] =
      (select * setOp) ^^ { o => SelectStmt(o)}

    protected lazy val setOp: Parser[(Operator, Operator) => Operator] =
      UNION ~ ALL ^^^ { Union(_, _) } |
        INTERSECT ^^^ { Intersect(_, _) } |
        EXCEPT ^^^ { Except(_, _) } |
        UNION ~ DISTINCT.? ^^^ { (q1, q2) => Distinct(Union(q1, q2)) }

    def selectAll(columns: List[Expression]): Boolean = columns match {
      case List(StarExpr()) => true
      case _ => false
    }

    protected lazy val select: Parser[Operator] =
      SELECT ~> DISTINCT.? ~
        repsep(projection, ",") ~
        (FROM ~> relations) ~
        (WHERE ~> expression).? ~
        (GROUP ~ BY ~> rep1sep(expression, ",")).? ~
        (HAVING ~> expression).? ~
        (ORDER ~ BY ~> ordering).? ~
        (LIMIT ~> expression).? ^^ {
        case d ~ p ~ r ~ f ~ g ~ h ~ o ~ l =>
          val base = r
          val withFilter = f.map(Filter(base, _)).getOrElse(base)
          val withProjection = if (selectAll(p)) withFilter else Project(withFilter, p)
          val withGroupBy = g.map(GroupBy(withProjection, _)).getOrElse(withProjection)
          val withDistinct = d.map(_ => Distinct(withGroupBy)).getOrElse(withGroupBy)
          val withHaving = h.map(Filter(withDistinct, _)).getOrElse(withDistinct)
          val withOrder = o.map(OrderBy(withHaving, _)).getOrElse(withHaving)
          val withLimit = l.map(Limit(withOrder, _)).getOrElse(withOrder)
          withLimit
      }

    protected lazy val projection: Parser[Expression] =
      expression ~ (AS.? ~> ident.?) ^^ {
        case e ~ a =>
          a match {
            case None => e
            case Some(alias) => ExprAlias(e, alias)
          }
      }

    // Based very loosely on the MySQL Grammar.
    // http://dev.mysql.com/doc/refman/5.0/en/join.html
    protected lazy val relations: Parser[Operator] =
      (relation ~ rep1("," ~> relation) ^^ {
        case r1 ~ joins => joins.foldLeft(r1) { case (lhs, r) => CrossJoin(lhs, r)}
      }
        | relation
        )

    protected lazy val relation: Parser[Operator] =
      joinedRelation | relationFactor

    protected lazy val relationFactor: Parser[Operator] =
      (ident ~ (opt(AS) ~> opt(ident))) ^^ {
        case name ~ aliasOpt =>
          tables.get(name) match {
            case Some(table) =>
              val t = Scan(table)
              aliasOpt match {
                case Some(alias) => TableAlias(t, alias)
                case _ => t
              }
            case None =>
              throw SqlException("Table " + name + " not found")
          }
      } |
        ("(" ~> selectStmt <~ ")") ~ (AS.? ~> ident) ^^ {
          case s ~ a => TableAlias(SubSelect(s.operator), a)
        }

    protected lazy val joinedRelation: Parser[Operator] =
      relationFactor ~ rep1(joiner) ^^ {
        case r1 ~ joins =>
          joins.foldLeft(r1) { case (lhs, joiner) =>
            joiner(lhs)
          }
        }

    protected lazy val joiner: Parser[Operator => Operator] =
      (joinType.? ~ (JOIN ~> relationFactor) ~ joinSpec) ^^ {
        case jtOpt ~ rhs ~ joinSpec =>
          l: Operator => Join(l, rhs, jtOpt.getOrElse(Inner), joinSpec)
      } |
        (CROSS ~> JOIN ~> relation) ^^ {
          rhs =>
            l: Operator => CrossJoin(l, rhs)
        } |
        (NATURAL ~> joinType.? ~ (JOIN ~> relation)) ^^ {
          case jtOpt ~ rhs =>
            l: Operator => Join(l, rhs, jtOpt.getOrElse(Inner), Natural)
        } |
        (UNION ~> JOIN ~> relation) ^^ {
          rhs =>
            l: Operator => UnionJoin(l, rhs)
        }

    protected lazy val joinSpec: Parser[JoinSpec] =
      ON ~> expression ^^ { On(_) } |
      USING ~> fieldList ^^ { Using(_) }

    protected lazy val joinType: Parser[JoinType] =
      (INNER ^^^ Inner
        | LEFT ~ SEMI ^^^ LeftSemi
        | LEFT ~ OUTER.? ^^^ LeftOuter
        | RIGHT ~ OUTER.? ^^^ RightOuter
        | FULL ~ OUTER.? ^^^ FullOuter
        )

    protected lazy val ordering: Parser[List[SortSpec]] =
      rep1sep(sortSpec, ",")

    protected lazy val sortSpec: Parser[SortSpec] =
      expression ~ (ASC ^^^ Ascending | DESC ^^^ Descending).? ~ (NULLS ~> (FIRST ^^^ NullsFirst | LAST ^^^ NullsLast)).? ^^ {
        case e ~ o ~ n => SortSpec(e, o.getOrElse(Ascending), n.getOrElse(NullsOrderingUnspecified))
      }

    protected lazy val expression: Parser[Expression] =
      orExpression

    protected lazy val orExpression: Parser[Expression] =
      andExpression * (OR ^^^ { (e1: Expression, e2: Expression) => OrExpr(e1, e2)})

    protected lazy val andExpression: Parser[Expression] =
      comparisonExpression * (AND ^^^ { (e1: Expression, e2: Expression) => AndExpr(e1, e2)})

    private def notOpt(not: Option[_], expr: Expression) =
      not match {
        case None => expr
        case Some(_) => NotExpr(expr)
      }

    protected lazy val comparisonExpression: Parser[Expression] =
      termExpression ~ ("=" ~> termExpression) ^^ { case e1 ~ e2 => EqExpr(e1, e2) } |
        termExpression ~ ("<" ~> termExpression) ^^ { case e1 ~ e2 => LtExpr(e1, e2) } |
        termExpression ~ ("<=" ~> termExpression) ^^ { case e1 ~ e2 => LeExpr(e1, e2) } |
        termExpression ~ (">" ~> termExpression) ^^ { case e1 ~ e2 => GtExpr(e1, e2) } |
        termExpression ~ (">=" ~> termExpression) ^^ { case e1 ~ e2 => GeExpr(e1, e2) } |
        termExpression ~ ("!=" ~> termExpression) ^^ { case e1 ~ e2 => NeExpr(e1, e2) } |
        termExpression ~ ("<>" ~> termExpression) ^^ { case e1 ~ e2 => NeExpr(e1, e2) } |
        termExpression ~ NOT.? ~ (BETWEEN ~> termExpression) ~ (AND ~> termExpression) ^^ {
          case e ~ not ~ el ~ eu =>
            val betweenExpr = AndExpr(GeExpr(e, el), LeExpr(e, eu))
            notOpt(not, betweenExpr)
        } |
        termExpression ~ NOT.? ~ (LIKE ~> termExpression) ~ (ESCAPE ~> termExpression).? ^^ {
          case e1 ~ not ~ e2 ~ escapeOpt =>
            notOpt(not, LikeExpr(e1, e2, escapeOpt))
        } |
        termExpression ~ (NOT.? <~ IN) ~ ("(" ~> rep1sep(termExpression, ",") <~ ")") ^^ {
          case e1 ~ not ~ e2 =>
            notOpt(not, InListExpr(e1, e2))
        } |
        termExpression ~ (NOT.? <~ IN) ~ ("(" ~> selectStmt <~ ")") ^^ {
          case e1 ~ not ~ e2 =>
            notOpt(not, InExpr(e1, e2))
        } |
        (termExpression <~ IS) ~ (NOT.? <~ NULL) ^^ {
          case e ~ not =>
            notOpt(not, IsNullExpr(e))
        } |
        NOT ~> termExpression ^^ { e => NotExpr(e) } |
        NOT.? ~ (EXISTS ~> termExpression) ^^ {
          case not ~ e =>
            notOpt(not, ExistsExpr(e))
          } |
        termExpression

    protected lazy val termExpression: Parser[Expression] =
      productExpression *
        ("+" ^^^ { (e1: Expression, e2: Expression) => AddExpr(e1, e2)}
          | "-" ^^^ { (e1: Expression, e2: Expression) => SubExpr(e1, e2)}
          )

    protected lazy val productExpression: Parser[Expression] =
      baseExpression *
        ("*" ^^^ { (e1: Expression, e2: Expression) => MulExpr(e1, e2)}
          | "/" ^^^ { (e1: Expression, e2: Expression) => DivExpr(e1, e2)}
          )

    protected lazy val function: Parser[Expression] =
      (SUM ~> "(" ~> expression <~ ")" ^^ { case exp => SumExpr(exp)}
        | SUM ~> "(" ~> DISTINCT ~> expression <~ ")" ^^ { case exp => SumDistinctExpr(exp)}
        | COUNT ~ "(" ~> "*" <~ ")" ^^ { case _ => CountExpr()}
        | COUNT ~ "(" ~> expression <~ ")" ^^ { case exp => CountNotNullExpr(exp)}
        | COUNT ~> "(" ~> DISTINCT ~> repsep(expression, ",") <~ ")" ^^ { case exps => CountDistinctExpr(exps)}
        | AVG ~ "(" ~> expression <~ ")" ^^ { case exp => AvgExpr(exp)}
        | MIN ~ "(" ~> expression <~ ")" ^^ { case exp => MinExpr(exp)}
        | MAX ~ "(" ~> expression <~ ")" ^^ { case exp => MaxExpr(exp)}
        | CASE ~> expression.? ~ (WHEN ~> expression ~ (THEN ~> expression)).* ~
        ( ELSE ~> expression).? <~ END ^^ {
        case casePart ~ altPart ~ elsePart =>
          val altExprs = altPart.flatMap { case whenExpr ~ thenExpr =>
            Seq(casePart.fold(whenExpr)(EqExpr(_, whenExpr)), thenExpr)
          }
          CaseWhenExpr(altExprs ++ elsePart.toList)
      }
        | (SUBSTR | SUBSTRING) ~ "(" ~> expression ~ ("," ~> expression) ~ ("," ~> expression) <~ ")" ^^ { case s ~ p ~ l => SubstrExpr(s, p, l)}
        | ident ~ ("(" ~> repsep(expression, ",")) <~ ")" ^^ { case func ~ exprs => FuncExpr(func, exprs)}
        )

    protected lazy val cast: Parser[Expression] =
      CAST ~ "(" ~> expression ~ (AS ~> fieldType) <~ ")" ^^ { case exp ~ t => CastExpr(exp, t)}

    protected lazy val literal: Parser[Literal] =
      (numericLiteral
        | booleanLiteral
        | stringLit ^^ { case s => Literal(s, StringType)}
        | NULL ^^^ Literal(null, NullType)
        )

    protected lazy val booleanLiteral: Parser[Literal] =
      (TRUE ^^^ Literal(true, BoolType)
        | FALSE ^^^ Literal(false, BoolType)
        )

    protected lazy val numericLiteral: Parser[Literal] =
      signedNumericLiteral | unsignedNumericLiteral

    protected lazy val sign: Parser[String] =
      "+" | "-"

    protected lazy val signedNumericLiteral: Parser[Literal] =
      (sign ~ numericLit ^^ { case s ~ l => Literal(toNarrowestIntegerType(s + l), IntType)}
        | sign ~ floatLit ^^ { case s ~ f => Literal((s + f).toDouble, DoubleType)}
        )

    protected lazy val unsignedNumericLiteral: Parser[Literal] =
      (numericLit ^^ { n => Literal(toNarrowestIntegerType(n), IntType)}
        | floatLit ^^ { f => Literal(f.toDouble, DoubleType)}
        )

    private def toNarrowestIntegerType(value: String) = {
      val bigIntValue = BigDecimal(value)

      bigIntValue match {
        case v if bigIntValue.isValidInt => v.toIntExact
        case v if bigIntValue.isValidLong => v.toLongExact
        case v => v
      }
    }

    protected lazy val floatLit: Parser[String] =
      ("." ~> unsignedNumericLiteral ^^ { u => "0." + u}
        | elem("decimal", _.isInstanceOf[lexical.FloatLit]) ^^ (_.chars)
        )

    protected lazy val baseExpression: Parser[Expression] =
      ("*" ^^^ StarExpr()
        | primary
        )

    protected lazy val signedPrimary: Parser[Expression] =
      sign ~ primary ^^ { case s ~ e => if (s == "-") NegExpr(e) else e}

    protected lazy val primary: PackratParser[Expression] =
      (literal
        | cast
        | "(" ~> expression <~ ")"
        | function
        | (ident <~ ".").? ~ ident ^^ { case tableOpt ~ name => ColumnRef(tableOpt, name)}
        | signedPrimary
        | "(" ~> selectStmt <~ ")" ^^ { stmt => SelectExpr(stmt)}
        )
  }

}
