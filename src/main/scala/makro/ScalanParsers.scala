/**
 * User: Alexander Slesarenko
 * Date: 11/17/13
 */
package makro

import scala.util.parsing.combinator.JavaTokenParsers
import java.text.ParseException

trait ScalanAst {

  // Tpe universe --------------------------------------------------------------------------
  sealed abstract class TpeExpr
  type TpeExprs = List[TpeExpr]
  case class TraitCall(name: String, tpeExprs: List[TpeExpr] = Nil) extends TpeExpr {
    override def toString = name + (if (tpeExprs.isEmpty) "" else tpeExprs.mkString("[",",","]"))
  }
  case object TpeInt extends TpeExpr     { override def toString = "Int" }
  case object TpeBoolean extends TpeExpr { override def toString = "Boolean" }
  case object TpeFloat extends TpeExpr   { override def toString = "Float" }
  case object TpeString extends TpeExpr  { override def toString = "String" }
  case class TpeTuple(items: List[TpeExpr]) extends TpeExpr {
    override def toString = items.mkString("(", ",", ")")
  }
  case class TpeFunc(items: List[TpeExpr]) extends TpeExpr {
    override def toString = items.mkString("=>")
  }
  case class TpeSum(items: List[TpeExpr]) extends TpeExpr {
    override def toString = items.mkString("(", "|", ")")
  }

  // Expr universe --------------------------------------------------------------------------
  abstract class Expr
  case class MethodCall(obj: Expr, name: String, args: List[Expr] = Nil) extends Expr

  // BodyItem universe ----------------------------------------------------------------------
  abstract class BodyItem
  case class ImportStat(names: List[String]) extends BodyItem
  case class MethodDef(name: String, tpeArgs: TpeArgs = Nil, args: MethodArgs = Nil,
                       tpeRes: TpeExpr = TraitCall("Any"), isImplicit: Boolean = false) extends BodyItem
  case class TpeDef(name: String, tpeArgs: TpeArgs = Nil, rhs: TpeExpr) extends BodyItem

  case class TpeArg(name: String, bound: Option[TpeExpr] = None, contextBound: List[String] = Nil)
  type TpeArgs = List[TpeArg]

  case class MethodArg(name: String, tpe: TpeExpr, default: Option[Expr] = None)
  type MethodArgs = List[MethodArg]

  case class ClassArg(impFlag: Boolean, overFlag: Boolean, valFlag: Boolean, name: String, tpe: TpeExpr, default: Option[Expr] = None)
  type ClassArgs = List[ClassArg]

  case class SelfTypeDef(name: String, components: List[TpeExpr])

  case class TraitDef(
    name: String,
    tpeArgs: List[TpeArg] = Nil,
    ancestors: List[TraitCall] = Nil,
    body: List[BodyItem] = Nil,
    selfType: Option[SelfTypeDef] = None) extends BodyItem

  case class ClassDef(
    name: String,
    tpeArgs: List[TpeArg] = Nil,
    args: ClassArgs = Nil,
    implicitArgs: ClassArgs = Nil,
    ancestors: List[TraitCall] = Nil,
    body: List[BodyItem] = Nil,
    selfType: Option[SelfTypeDef] = None,
    isAbstract: Boolean = false) extends BodyItem

  case class EntityModuleDef(
    packageName: String,
    imports: List[ImportStat],
    name: String,
    typeSyn: TpeDef,
    entityOps: TraitDef,
    concreteClasses: List[ClassDef],
    selfType: Option[SelfTypeDef] = None) {
  }

  def getConcreteClasses(defs: List[BodyItem]) = defs.collect { case c: ClassDef => c }

  object EntityModuleDef {
    def fromModuleTrait(packageName: String, imports: List[ImportStat], moduleTrait: TraitDef): EntityModuleDef = {
      val moduleName = moduleTrait.name
      val defs = moduleTrait.body

      val (typeSyn, opsTrait) = defs match {
        case (ts: TpeDef) :: (ot: TraitDef) :: _ => (ts, ot)
        case _ =>
          throw new ParseException(s"Invalid syntax of Entity module trait $moduleName", 0)
      }
      val classes = getConcreteClasses(defs)

      EntityModuleDef(packageName, imports, moduleName, typeSyn, opsTrait, classes, moduleTrait.selfType)
    }

  }



}


trait ScalanParsers extends JavaTokenParsers  { self: ScalanAst =>

  implicit class OptionListOps[A](opt: Option[List[A]]) {
    def flatList: List[A] = opt.toList.flatten
  }

  def wrapIfMany[A <: TpeExpr,B <: TpeExpr](w: List[A]=>B, xs: List[A]):TpeExpr = {
    val sz = xs.size
    assert(sz >= 1)
    if (sz > 1) w(xs) else xs.head
  }

  val keywords = Set("def", "trait", "type", "class", "abstract")

  lazy val scalanIdent = ident ^? ({ case s if !keywords.contains(s) => s }, s => s"Keyword $s cannot be used as identifier")

  lazy val qualId = rep1sep(scalanIdent, ".")

  lazy val tpeArg: Parser[TpeArg] = (scalanIdent ~ opt("<:" ~ tpeExpr) ~ rep(":" ~> scalanIdent)) ^^ {
    case name ~ None ~ ctxs => TpeArg(name, None, ctxs)
    case name ~ Some(_ ~ bound) ~ ctxs => TpeArg(name, Some(bound), ctxs)
  }

  lazy val tpeArgs = "[" ~ tpeArg ~ rep( ("," ~ tpeArg) ^^ { case _ ~ x => x }) ~ "]" ^^ {
    case _ ~ x ~ xs ~ _ => x :: xs
  }

  lazy val extendsList = "extends" ~> traitCall ~ rep( ("with" ~ traitCall) ^^ { case _ ~ x => x }) ^^ {
    case x ~ xs => x :: xs
  }

  lazy val tpeBase: Parser[TpeExpr] = "Int" ^^^ { TpeInt } |
                     "Boolean" ^^^ { TpeBoolean } |
                     "Float" ^^^ { TpeFloat }     |
                     "String" ^^^ { TpeString }   |
                     traitCall

  lazy val tpeFactor = tpeBase | "(" ~> tpeExpr <~ ")"

  lazy val tpeFunc = rep1sep(tpeFactor, "=>") ^^ { wrapIfMany(TpeFunc, _) }

  lazy val tpeTuple = rep1sep(tpeFunc, ",") ^^ { wrapIfMany(TpeTuple, _) }

  lazy val tpeExpr: Parser[TpeExpr] = rep1sep(tpeTuple, "|") ^^ { wrapIfMany(TpeSum, _) }

  lazy val traitCallArgs = "[" ~> rep1sep(tpeExpr, ",") <~ "]" ^^ {
    case List(t: TpeTuple) => t.items
    case x => x
  }

  lazy val traitCall = scalanIdent ~ opt(traitCallArgs) ^^ {
    case n ~ None => TraitCall(n)
    case n ~ Some(ts) => TraitCall(n, ts)
  }

  lazy val methodArg = scalanIdent ~ ":" ~ tpeFactor ^^ { case n ~ _ ~ t => MethodArg(n, t, None)}
  lazy val methodArgs = "(" ~> rep1sep(methodArg, ",") <~ ")"


  lazy val classArg = opt("implicit") ~ opt("override") ~ opt("val") ~ scalanIdent ~ ":" ~ tpeFactor ^^ {
    case imp ~ over ~ value ~ n ~ _ ~ t =>
      ClassArg(imp.isDefined, over.isDefined, value.isDefined, n, t, None)
  }
  lazy val classArgs = "(" ~> rep1sep(classArg, ",")  <~ ")"

  lazy val methodBody = "???"

  lazy val methodDef = opt("implicit") ~ "def" ~ scalanIdent ~ opt(tpeArgs) ~ opt(methodArgs) ~ ":" ~ tpeExpr ~ opt("=" ~ methodBody) ^^ {
    case implicitModifier ~ _ ~ n ~ targs ~ args ~ _ ~ tres ~ _ =>
      MethodDef(n, targs.toList.flatten, args.toList.flatten, tres, implicitModifier.isDefined)
  }

  lazy val importStat = "import" ~ qualId ~ opt(";") ^^ { case _ ~ ns ~ _ => ImportStat(ns) }

  lazy val tpeDef = "type" ~ scalanIdent ~ opt(tpeArgs) ~ "=" ~ tpeExpr ~ opt(";") ^^ {
    case _ ~ n ~ targs ~ _ ~ rhs ~ _ => TpeDef(n, targs.toList.flatten, rhs)
  }

  lazy val bodyItem =
    methodDef |
    importStat |
    tpeDef     |
    traitDef   |
    classDef

  lazy val bodyItems = rep(bodyItem)

  lazy val selfType = (scalanIdent <~ ":") ~ (tpeExpr <~ "=>") ^^ { case n ~ t => SelfTypeDef(n, List(t)) }

  lazy val traitBody = "{" ~ opt(selfType) ~ opt(bodyItems) ~ "}" ^^ {
    case _ ~ self ~ body ~ _ => (self, body.toList.flatten)
  }

  lazy val traitDef: Parser[TraitDef] = "trait" ~ scalanIdent ~ opt(tpeArgs) ~ opt(extendsList) ~ opt(traitBody) ^^ {
    case _ ~ n ~ targs ~ ancs ~ body =>
      TraitDef(n, targs.toList.flatten, ancs.toList.flatten, body.map(_._2).flatList, body.map(_._1).flatten)
  }

  lazy val classDef: Parser[ClassDef] =
    opt("abstract") ~ "class" ~ scalanIdent ~ opt(tpeArgs) ~ opt(classArgs) ~ opt(classArgs) ~ opt(extendsList) ~ opt(traitBody) ^^ {
    case abs ~ _ ~ n ~ targs ~ args ~ impArgs ~ ancs ~ body =>
      ClassDef(n, targs.flatList, args.flatList, impArgs.flatList, ancs.flatList, body.map(_._2).flatList, body.map(_._1).flatten, abs.isDefined)
  }

  lazy val entityModuleDef =
    "package" ~ qualId ~ opt(";") ~
    rep1sep(importStat, opt(";")) ~
    traitDef ^^ {
      case _ ~ ns ~ _ ~ imports ~ moduleTrait => {
        val packageName = ns.mkString(".")
        EntityModuleDef.fromModuleTrait(packageName, imports, moduleTrait)
      }
  }

  //TODO add regex for comments
  protected val endlineComment = """//\.*\n""".r

  protected override def handleWhiteSpace(source: java.lang.CharSequence, offset: Int): Int = {
    val ofs = super.handleWhiteSpace(source, offset)

//    (endlineComment findPrefixMatchOf (source.subSequence(ofs, source.length))) match {
//      case Some(matched) => ofs + matched.end
//      case None => ofs
//    }
    ofs
  }

  def parseTpeExpr(s: String) = parseAll(tpeExpr, s).get
  def parseTpeArgs(s: String) = parseAll(tpeArgs, s).get

  def parseTrait(s: String) = parseAll(traitDef, s).get
  def parseEntityModule(s: String) = {
    val res = parseAll(entityModuleDef, s)
    res match {
      case Success(r, next) => r
      case Failure(msg, next) => {
        val rest = next.rest.source.toString
        val source = next.source.toString
        throw new ParseException(s"$msg pos:${next.pos} rest:$rest", next.offset)
      }
    }
  }

}

object ScalanImpl
  extends ScalanParsers
     with ScalanAst
{
}

