package scalan.meta

import scala.collection.mutable
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.CharArrayReader.EofCh
import scalan.meta.SqlAST._

// see http://savage.net.au/SQL/sql-2003-2.bnf.html for full SQL grammar
class SqlParser {
  val grammar = new SqlGrammar

  def parseDDL(sql: String) = grammar.schema(sql)
  def parseSelect(sql: String) = grammar.select(sql)

  class SqlLexical(val keywords: Seq[String]) extends StdLexical {

    case class FloatLit(chars: String) extends Token {
      override def toString = chars
    }

    reserved ++= keywords

    // SQL identifiers and keywords are case-insensitive, thus convert them to a specific case
    // TODO support quoted identifiers
    override protected def processIdent(name: String) =
      if (reserved contains name.toUpperCase) Keyword(name.toUpperCase) else Identifier(name.toLowerCase)

    delimiters +=(
      "@", "*", "+", "-", "<", "=", "==", "<>", "!=", "<=", ">=", ">", "/", "(", ")",
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
  }

  class SqlGrammar extends StandardTokenParsers with PackratParsers {
    protected case class Keyword(str: String) {
      lazy val parser = str ^^^ this
    }

    protected implicit def asParser(k: Keyword): Parser[Keyword] = k.parser

    protected val ABORT = Keyword("ABORT")
    protected val ABS = Keyword("ABS")
    protected val ALL = Keyword("ALL")
    protected val AND = Keyword("AND")
    protected val APPROXIMATE = Keyword("APPROXIMATE")
    protected val AS = Keyword("AS")
    protected val ASC = Keyword("ASC")
    protected val AUTOINCREMENT = Keyword("AUTOINCREMENT")
    protected val AVG = Keyword("AVG")
    protected val BETWEEN = Keyword("BETWEEN")
    protected val BIGINT = Keyword("BIGINT")
    protected val BIT = Keyword("BIT")
    protected val BOOL = Keyword("BOOL")
    protected val BY = Keyword("BY")
    protected val BYTE = Keyword("BYTE")
    protected val CACHE = Keyword("CACHE")
    protected val CASE = Keyword("CASE")
    protected val CAST = Keyword("CAST")
    protected val CHAR = Keyword("CHAR")
    protected val CHECK = Keyword("CHECK")
    protected val COLLATE = Keyword("COLLATE")
    protected val CONFLICT = Keyword("CONFLICT")
    protected val COUNT = Keyword("COUNT")
    protected val CREATE = Keyword("CREATE")
    protected val CROSS = Keyword("CROSS")
    protected val CURRENT_DATE = Keyword("CURRENT_DATE")
    protected val CURRENT_TIME = Keyword("CURRENT_TIME")
    protected val CURRENT_TIMESTAMP = Keyword("CURRENT_TIMESTAMP")
    protected val DATE = Keyword("DATE")
    protected val DECIMAL = Keyword("DECIMAL")
    protected val DEFAULT = Keyword("DEFAULT")
    protected val DESC = Keyword("DESC")
    protected val DISTINCT = Keyword("DISTINCT")
    protected val DOUBLE = Keyword("DOUBLE")
    protected val DOUBLE_PRECISION = Keyword("DOUBLE_PRECISION")
    protected val ELSE = Keyword("ELSE")
    protected val END = Keyword("END")
    protected val ENUM = Keyword("ENUM")
    protected val ESCAPE = Keyword("ESCAPE")
    protected val EXCEPT = Keyword("EXCEPT")
    protected val EXISTS = Keyword("EXISTS")
    protected val FAIL = Keyword("FAIL")
    protected val FALSE = Keyword("FALSE")
    protected val FIRST = Keyword("FIRST")
    protected val FOREIGN = Keyword("FOREIGN")
    protected val FROM = Keyword("FROM")
    protected val FULL = Keyword("FULL")
    protected val GROUP = Keyword("GROUP")
    protected val HAVING = Keyword("HAVING")
    protected val IF = Keyword("IF")
    protected val IGNORE = Keyword("IGNORE")
    protected val IN = Keyword("IN")
    protected val INDEX = Keyword("INDEX")
    protected val INNER = Keyword("INNER")
    protected val INSERT = Keyword("INSERT")
    protected val INT = Keyword("INT")
    protected val INTEGER = Keyword("INTEGER")
    protected val INTERSECT = Keyword("INTERSECT")
    protected val INTO = Keyword("INTO")
    protected val IS = Keyword("IS")
    protected val JOIN = Keyword("JOIN")
    protected val KEY = Keyword("KEY")
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
    protected val NUMERIC = Keyword("NUMERIC")
    protected val ON = Keyword("ON")
    protected val OR = Keyword("OR")
    protected val ORDER = Keyword("ORDER")
    protected val OUTER = Keyword("OUTER")
    protected val OVERWRITE = Keyword("OVERWRITE")
    protected val PRIMARY = Keyword("PRIMARY")
    protected val REAL = Keyword("REAL")
    protected val REFERENCES = Keyword("REFERENCES")
    protected val REGEXP = Keyword("REGEXP")
    protected val REPLACE = Keyword("REPLACE")
    protected val RIGHT = Keyword("RIGHT")
    protected val RLIKE = Keyword("RLIKE")
    protected val ROLLBACK = Keyword("ROLLBACK")
    protected val ROWID = Keyword("ROWID")
    protected val SELECT = Keyword("SELECT")
    protected val SEMI = Keyword("SEMI")
    protected val SMALLINT = Keyword("SMALLINT")
    protected val SQRT = Keyword("SQRT")
    protected val STRING = Keyword("STRING")
    protected val SUBSTR = Keyword("SUBSTR")
    protected val SUBSTRING = Keyword("SUBSTRING")
    protected val SUM = Keyword("SUM")
    protected val TABLE = Keyword("TABLE")
    protected val TEMP = Keyword("TEMP")
    protected val TEMPORARY = Keyword("TEMPORARY")
    protected val TEXT = Keyword("TEXT")
    protected val THEN = Keyword("THEN")
    protected val TIME = Keyword("TIME")
    protected val TIMESTAMP = Keyword("TIMESTAMP")
    protected val TINYINT = Keyword("TINYINT")
    protected val TRUE = Keyword("TRUE")
    protected val UNION = Keyword("UNION")
    protected val UNIQUE = Keyword("UNIQUE")
    protected val UPPER = Keyword("UPPER")
    protected val USING = Keyword("USING")
    protected val VARCHAR = Keyword("VARCHAR")
    protected val WHEN = Keyword("WHEN")
    protected val WHERE = Keyword("WHERE")
    protected val WITHOUT = Keyword("WITHOUT")

    override val lexical: SqlLexical = {
      val keywords =
        this.getClass.getMethods
          .filter(_.getReturnType == classOf[Keyword])
          .map(_.invoke(this).asInstanceOf[Keyword].str)

      new SqlLexical(keywords)
    }

    val tables = mutable.Map.empty[String, Table]

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
      CREATE ~> (TEMP | TEMPORARY).? ~> TABLE ~> (IF ~ NOT ~ EXISTS).? ~> ident ~ ("(" ~> repsep(columnDef, ",") ~ ("," ~> tableConstraint).* <~ ")") ~ (WITHOUT ~ ROWID).? ^^ {
        case name ~ (columns ~ constraints) ~ withoutRowidOpt =>
          val table = Table(name, columns, constraints, withoutRowidOpt.isDefined)
          tables.update(name, table)
          CreateTableStmt(table)
      }

    lazy val tableConstraint: Parser[TableConstraint] =
      PRIMARY ~> KEY ~> ("(" ~> repsep(indexedColumn, ",") <~ ")") ~ conflictClause ^^ {
        case cols ~ onConflict => PrimaryKeyT(cols, onConflict)
      } |
        UNIQUE ~> ("(" ~> repsep(indexedColumn, ",") <~ ")") ~ conflictClause ^^ {
          case cols ~ onConflict => UniqueT(cols, onConflict)
        } |
        checkClause |
        FOREIGN ~> KEY ~> fieldList ~ foreignKeyClause ^^ {
          case key ~ ((parentTable, parentKey)) =>
            if (key.length == parentKey.length)
              ForeignKeyT(parentTable, key.zip(parentKey))
            else
              throw new SqlException(s"Different number of columns in parent key ${parentKey.mkString("(", ",", ")")} and child key ${key.mkString("(", ",", ")")}")
        }

    lazy val indexedColumn = ident ~ collationClause.? ~ direction ^^ {
      case name ~ optCollSeq ~ dir => IndexedColumn(name, optCollSeq.getOrElse("BINARY"), dir)
    }

    lazy val createIndexStmt: Parser[Statement] =
      CREATE ~> INDEX ~> ident ~ (ON ~> table) ~ fieldList ^^ {
        case name ~ table ~ key => CreateIndexStmt(name, table, key)
      }

    lazy val table: Parser[Table] =
      ident ^^ { name => if (tables.contains(name)) tables(name) else throw SqlException("Unknown table " + name)}

    // fieldType is optional in SQLite only
    lazy val columnDef: Parser[Column] =
      ident ~ columnType.? ~ repsep(columnConstraint, ",") ^^ { case i ~ t ~ cs => Column(i, t.getOrElse(BasicStringType), cs) }

    lazy val columnType: Parser[ColumnType] =
      (TINYINT | BYTE) ^^^ TinyIntType |
        SMALLINT ^^^ SmallIntType |
        (INTEGER | INT) ^^^ IntType |
        BIGINT ^^^ BigIntType |
        (DECIMAL | NUMERIC) ~> ("(" ~> (int <~ ",") ~ int <~ ")").? ^^ {
          case None => DecimalType(None, None)
          case Some(p1 ~ p2) => DecimalType(Some(p1), Some(p2))
        } |
        (REAL | DOUBLE | DOUBLE_PRECISION) ^^^ DoubleType |
        (BOOL | BIT) ^^^ BoolType |
        (CHAR | VARCHAR | TEXT) ~ ("(" ~> int <~ ")").? ^^ {
          case kw ~ optLength => StringType(kw == CHAR, optLength)
        } |
        DATE ^^^ DateType | TIME ^^^ TimeType | TIMESTAMP ^^^ TimestampType |
        ENUM ~> ("(" ~> rep1sep(stringLit, ",") <~ ")") ^^ { EnumType(_) } |
        ident ^^ {
          t => throw SqlException("Unsupported type " + t)
        }

    lazy val int = numericLit ^^ { s => s.toInt }

    lazy val columnConstraint: Parser[ColumnConstraint] =
      PRIMARY ~> KEY ~> direction ~ conflictClause ~ AUTOINCREMENT.? ^^ { case direction ~ onConflict ~ autoIncrOpt =>
        PrimaryKeyC(direction, onConflict, autoIncrOpt.isDefined)
      } |
        NOT ~> NULL ~> conflictClause ^^ { case onConflict => NotNull(onConflict) } |
        UNIQUE ~> conflictClause ^^ { case onConflict => UniqueC(onConflict) } |
        DEFAULT ~> defaultValue ^^ { case expr => Default(expr) } |
        checkClause |
        collationClause ^^ { case collSeq => Collate(collSeq) } |
        foreignKeyClause ^^ { case (parent, key) =>
          key match {
            case List(column) =>
              ForeignKeyC(parent, column)
            case _ =>
              throw new SqlException(s"Foreign key for a single column references multiple columns ${key.mkString(",")}")
          }
        }

    lazy val checkClause = CHECK ~> "(" ~> expression <~ ")" ^^ { Check(_) }

    lazy val conflictClause: Parser[OnConflict] =
      (ON ~> CONFLICT ~> (ROLLBACK ^^^ OnConflict.Rollback | ABORT ^^^ OnConflict.Abort | FAIL ^^^ OnConflict.Fail | IGNORE ^^^ OnConflict.Ignore | REPLACE ^^^ OnConflict.Replace)).? ^^ { _.getOrElse(OnConflict.Abort) }

    lazy val defaultValue: Parser[Expression] = literal | "(" ~> expression <~ ")"

    lazy val collationClause = COLLATE ~> ident

    // TODO support ON DELETE/UPDATE, MATCH, DEFERRABLE
    // see https://www.sqlite.org/syntax/foreign-key-clause.html
    lazy val foreignKeyClause: Parser[(Table, List[String])] =
      REFERENCES ~> table ~ fieldList.? ^^ { case table ~ optKey =>
        (table, optKey.getOrElse(table.primaryKey))
      }

    lazy val fieldList: Parser[ColumnList] =
      "(" ~> repsep(ident, ",") <~ ")"

    protected lazy val selectStmt: Parser[SelectStmt] =
      (select * setOp) ^^ { o => SelectStmt(o)}

    protected lazy val setOp: Parser[(Operator, Operator) => Operator] =
      UNION ~ ALL ^^^ { Union(_, _) } |
        INTERSECT ^^^ { Intersect(_, _) } |
        EXCEPT ^^^ { Except(_, _) } |
        UNION ~ DISTINCT.? ^^^ { (q1, q2) => Distinct(Union(q1, q2)) }

    protected lazy val select: Parser[Operator] =
      SELECT ~> DISTINCT.? ~
        selectList ~
        (FROM ~> relations) ~
        (WHERE ~> expression).? ~
        (GROUP ~ BY ~> rep1sep(expression, ",")).? ~
        (HAVING ~> expression).? ~
        (ORDER ~ BY ~> ordering).? ~
        (LIMIT ~> expression).? ^^ {
        case d ~ p ~ r ~ f ~ g ~ h ~ o ~ l =>
          val base = r
          val withFilter = f.map(Filter(base, _)).getOrElse(base)
          val withProjection = p match {
            case None => withFilter
            case Some(columns) =>
              Project(withFilter, columns)
          }
          val withGroupBy = g.map(GroupBy(withProjection, _)).getOrElse(withProjection)
          val withDistinct = d.map(_ => Distinct(withGroupBy)).getOrElse(withGroupBy)
          val withHaving = h.map(Filter(withDistinct, _)).getOrElse(withDistinct)
          val withOrder = o.map(OrderBy(withHaving, _)).getOrElse(withHaving)
          val withLimit = l.map(Limit(withOrder, _)).getOrElse(withOrder)
          withLimit
      }

    protected lazy val selectList: Parser[Option[List[ProjectionColumn]]] =
      ("*" ^^^ None) | (repsep(projectionColumn, ",") ^^ { Some(_) })

    protected lazy val projectionColumn: Parser[ProjectionColumn] =
      expression ~ (AS.? ~> ident.?) ^^ {
        case e ~ a => ProjectionColumn(e, a)
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
      expression ~ direction ~ (NULLS ~> (FIRST ^^^ NullsFirst | LAST ^^^ NullsLast)).? ^^ {
        case e ~ d ~ n => SortSpec(e, d, n.getOrElse(NullsOrderingUnspecified))
      }

    protected lazy val direction: Parser[SortDirection] =
      (ASC ^^^ Ascending | DESC ^^^ Descending).? ^^ { _.getOrElse(Ascending) }

    protected lazy val expression: Parser[Expression] =
      orExpression

    protected lazy val orExpression: Parser[Expression] =
      andExpression * (OR ^^^ { (e1: Expression, e2: Expression) => BinOpExpr(Or, e1, e2)})

    protected lazy val andExpression: Parser[Expression] =
      comparisonExpression * (AND ^^^ { (e1: Expression, e2: Expression) => BinOpExpr(And, e1, e2)})

    private def notOpt(not: Option[_], expr: Expression) =
      not match {
        case None => expr
        case Some(_) => NotExpr(expr)
      }

    protected lazy val comparisonOp: Parser[ComparisonOp ~ Boolean] =
      ("=" | "==") ^^^ new ~(Eq, false) | ("<>" | "!=") ^^^ new ~(Eq, true) |
        IS ~ NOT ^^^ new ~(Is, true) | IS ^^^ new ~(Is, false) |
        ("<" ^^^ Less | "<=" ^^^ LessEq | ">" ^^^ Greater | ">=" ^^^ GreaterEq) ^^ { op => new ~(op, false) }

    protected lazy val comparisonExpression: Parser[Expression] =
      termExpression ~ comparisonOp ~ termExpression ^^ {
        case e1 ~ (op ~ negate) ~ e2 =>
          if (negate)
            NotExpr(BinOpExpr(op, e1, e2))
          else
            BinOpExpr(op, e1, e2)
      } |
        termExpression ~ NOT.? ~ (BETWEEN ~> termExpression) ~ (AND ~> termExpression) ^^ {
          case e ~ not ~ el ~ eu =>
            val betweenExpr = BinOpExpr(And,
              BinOpExpr(GreaterEq, e, el),
              BinOpExpr(LessEq, e, eu))
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

    protected lazy val plusPriorityOp: Parser[BinOp] =
      "+" ^^^ Plus | "-" ^^^ Minus

    protected lazy val termExpression: Parser[Expression] =
      productExpression *
        (plusPriorityOp ^^ { op =>
          (e1: Expression, e2: Expression) => BinOpExpr(op, e1, e2)
        })

    // || actually has even higher priority, shouldn't mix up with the rest in actual SQL
    protected lazy val timesPriorityOp: Parser[BinOp] =
      "||" ^^^ Concat | "*" ^^^ Times | "/" ^^^ Divide | "%" ^^^ Modulo

    protected lazy val productExpression: Parser[Expression] =
      baseExpression *
        (timesPriorityOp ^^ { op =>
          (e1: Expression, e2: Expression) => BinOpExpr(op, e1, e2)
        })

    protected lazy val aggregateOp: Parser[AggregateOp] =
      COUNT ^^^ Count | SUM ^^^ Sum | AVG ^^^ Avg | MAX ^^^ Max | MIN ^^^ Min

    protected lazy val aggregateExpression =
      COUNT ~ "(" ~> "*" <~ ")" ^^^ CountAllExpr |
        // FILTER clause not supported for now
        aggregateOp ~ ("(" ~> (DISTINCT | ALL).?) ~ expression <~ ")" ^^ {
          case op ~ quantifier ~ value =>
            val distinct = quantifier == Some(DISTINCT)
            AggregateExpr(op, distinct, value)
        }

    protected lazy val caseExpression =
      CASE ~> expression.? ~ (WHEN ~> expression ~ (THEN ~> expression)).* ~
        (ELSE ~> expression).? <~ END ^^ {
        case casePart ~ altPart ~ elsePart =>
          val altExprs = altPart.flatMap { case whenExpr ~ thenExpr =>
            Seq(casePart.fold(whenExpr)(BinOpExpr(Eq, _, whenExpr)), thenExpr)
          }
          CaseWhenExpr(altExprs ++ elsePart.toList)
      }

    protected lazy val function: Parser[Expression] =
      aggregateExpression |
        caseExpression |
        (SUBSTR | SUBSTRING) ~ "(" ~> expression ~ ("," ~> expression) ~ ("," ~> expression) <~ ")" ^^ {
          case s ~ p ~ l => SubstrExpr(s, p, l)
        } |
        ident ~ ("(" ~> repsep(expression, ",")) <~ ")" ^^ { case func ~ exprs => FuncExpr(func, exprs)}

    protected lazy val cast: Parser[Expression] =
      CAST ~ "(" ~> expression ~ (AS ~> columnType) <~ ")" ^^ { case exp ~ t => CastExpr(exp, t)}

    protected lazy val literal: Parser[Expression] =
      (numericLiteral
        | booleanLiteral
        | stringLit ^^ { case s => Literal(s, BasicStringType) }
        | NULL ^^^ NullLiteral
        | (CURRENT_DATE | CURRENT_TIME | CURRENT_TIMESTAMP) ^^ { kw => FuncExpr(kw.str.toLowerCase, Nil) }
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

    protected lazy val signedExpression: Parser[Expression] =
      sign ~ baseExpression ^^ { case s ~ e => if (s == "-") NegExpr(e) else e}

    protected lazy val baseExpression: PackratParser[Expression] =
      (literal
        | cast
        | "(" ~> expression <~ ")"
        | function
        | (ident <~ ".").? ~ ident ^^ { case tableOpt ~ name => ColumnRef(tableOpt, name)}
        | signedExpression
        | "(" ~> selectStmt <~ ")" ^^ { stmt => SelectExpr(stmt)}
        )
  }

}
