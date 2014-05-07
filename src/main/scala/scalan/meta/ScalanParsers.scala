/**
 * User: Alexander Slesarenko
 * Date: 11/17/13
 */
package scalan.meta

import scala.util.parsing.combinator.JavaTokenParsers
import java.text.ParseException

trait ScalanAst {

  // Tpe universe --------------------------------------------------------------------------
  sealed abstract class TpeExpr
  type TpeExprs = List[TpeExpr]
  case class TraitCall(name: String, tpeExprs: List[TpeExpr] = Nil) extends TpeExpr {
    override def toString = name + (if (tpeExprs.isEmpty) "" else tpeExprs.mkString("[", ",", "]"))
  }
  case object TpeInt extends TpeExpr { override def toString = "Int" }
  case object TpeBoolean extends TpeExpr { override def toString = "Boolean" }
  case object TpeFloat extends TpeExpr { override def toString = "Float" }
  case object TpeString extends TpeExpr { override def toString = "String" }
  case class TpeTuple(items: List[TpeExpr]) extends TpeExpr {
    override def toString = items.mkString("(", ",", ")")
  }
  case class TpeFunc(items: List[TpeExpr]) extends TpeExpr {
    override def toString = items.mkString("=>")
  }
  case class TpeSum(items: List[TpeExpr]) extends TpeExpr {
    override def toString = items.mkString("(", "|", ")")
  }

  implicit class TpeExprExtensions(self: TpeExpr) {
    def applySubst(subst: Map[String, TpeExpr]): TpeExpr = self match {
      case TraitCall(n, args) => // higher-kind usage of names is not supported  Array[A] - ok, A[Int] - nok
        subst.get(n) match {
          case Some(t) => t
          case None => TraitCall(n, args map { _.applySubst(subst) })
        }
      case TpeTuple(items) => TpeTuple(items map { _.applySubst(subst) })
      case TpeSum(items) => TpeSum(items map { _.applySubst(subst) })
      case _ => self
    }
  }


  // Expr universe --------------------------------------------------------------------------
  abstract class Expr
  case class MethodCall(obj: Expr, name: String, args: List[Expr] = Nil) extends Expr

  // BodyItem universe ----------------------------------------------------------------------
  abstract class BodyItem
  case class ImportStat(names: List[String]) extends BodyItem
  case class MethodDef(name: String, tpeArgs: TpeArgs = Nil, args: List[MethodArgs] = Nil,
    tpeRes: TpeExpr = TraitCall("Any"), isImplicit: Boolean = false) extends BodyItem
  case class TpeDef(name: String, tpeArgs: TpeArgs = Nil, rhs: TpeExpr) extends BodyItem

  case class TpeArg(name: String, bound: Option[TpeExpr] = None, contextBound: List[String] = Nil)
  type TpeArgs = List[TpeArg]

  case class MethodArg(name: String, tpe: TpeExpr, default: Option[Expr] = None)
  case class MethodArgs(impFlag: Boolean, args: List[MethodArg])

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
    
  case class ObjectDef(
    name: String,
    ancestors: List[TraitCall] = Nil,
    body: List[BodyItem] = Nil) extends BodyItem

  case class EntityModuleDef(
    packageName: String,
    imports: List[ImportStat],
    name: String,
    typeSyn: TpeDef,
    entityOps: TraitDef,
    concreteClasses: List[ClassDef],
    selfType: Option[SelfTypeDef] = None) {

    object EntityRepType {
      def unapply(ty: TpeExpr): Option[TpeExpr] = ty match {
        case TraitCall(n, args) if n == typeSyn.name =>
          typeSyn.rhs match {
            case TraitCall("Rep", List(entityType)) =>
              val subst = (typeSyn.tpeArgs.map(_.name) zip args).toMap
              Some(entityType.applySubst(subst))
            case _ => sys.error(s"Entity type synonym should be Rep type but was $typeSyn")
          }
        case _ => None
      }
    }
  }

  def getConcreteClasses(defs: List[BodyItem]) = defs.collect { case c: ClassDef => c }

  object EntityModuleDef {
    def fromModuleTrait(packageName: String, imports: List[ImportStat], moduleTrait: TraitDef, config: CodegenConfig): EntityModuleDef = {
      val moduleName = moduleTrait.name
      val defs = moduleTrait.body

      val (typeSyn, opsTrait) = defs match {
        case (ts: TpeDef) :: (ot: TraitDef) :: _ => (ts, ot)
        case _ =>
          throw new ParseException(s"Invalid syntax of Entity module trait $moduleName:\n${defs.mkString("\n")}", 0)
      }
      val classes = getConcreteClasses(defs)
      
      val extraImports = config.extraImports.map(i => ImportStat(List(i)))

      EntityModuleDef(packageName, imports ++ extraImports, moduleName, typeSyn, opsTrait, classes, moduleTrait.selfType)
    }

  }

}

trait ScalanParsers extends JavaTokenParsers { self: ScalanAst =>

  implicit class OptionListOps[A](opt: Option[List[A]]) {
    def flatList: List[A] = opt.toList.flatten
  }

  def wrapIfMany[A <: TpeExpr, B <: TpeExpr](w: List[A] => B, xs: List[A]): TpeExpr = {
    val sz = xs.size
    assert(sz >= 1)
    if (sz > 1) w(xs) else xs.head
  }

  val keywords = Set("def", "trait", "type", "class", "abstract", "with")
  
  val op = """[!#%&*+-/:<=>?@\^|~]*""".r

  lazy val scalanIdent = (ident | op) ^? ({ case s if !keywords.contains(s) => s }, s => s"Keyword $s cannot be used as identifier")

  lazy val bracedIdentList = "{" ~> rep1sep(scalanIdent, ",") <~ "}" ^^ { case xs => xs.mkString("{", ",", "}") }

  lazy val qualId = rep1sep(scalanIdent | bracedIdentList, ".")

  lazy val tpeArg: Parser[TpeArg] = (opt("@uncheckedVariance") ~> "[+-]?".r ~> (scalanIdent <~ opt("@uncheckedVariance")) ~ opt("<:" ~> tpeExpr) ~ rep(":" ~> scalanIdent)) ^^ {
    case name ~ bound ~ ctxs => TpeArg(name, bound, ctxs)
  }

  lazy val tpeArgs = "[" ~> rep1sep(tpeArg, ",") <~ "]"

  lazy val extendsList = "extends" ~> rep1sep(traitCall, "with")

  lazy val tpeBase: Parser[TpeExpr] = "Int" ^^^ { TpeInt } |
    "Boolean" ^^^ { TpeBoolean } |
    "Float" ^^^ { TpeFloat } |
    "String" ^^^ { TpeString } |
    traitCall

  lazy val tpeFactor = tpeBase | "(" ~> tpeExpr <~ ")"

  lazy val tpeFunc = rep1sep(tpeFactor, "=>") ^^ { wrapIfMany(TpeFunc, _) }

  lazy val tpeTuple = "(" ~> rep1sep(tpeFunc, ",") <~ ")" ^^ { wrapIfMany(TpeTuple, _) }

  lazy val tpeExpr: Parser[TpeExpr] = opt("@uncheckedVariance") ~> rep1sep(tpeTuple | tpeFunc, "|") <~ opt("@uncheckedVariance") ^^ { wrapIfMany(TpeSum, _) }

  lazy val traitCallArgs = "[" ~> rep1sep(tpeExpr, ",") <~ "]"

  lazy val traitCall = scalanIdent ~ opt(traitCallArgs) ^^ {
    case n ~ None => TraitCall(n)
    case n ~ Some(ts) => TraitCall(n, ts)
  }

  lazy val methodArg = (scalanIdent <~ ":") ~ tpeExpr ^^ { case n ~ t => MethodArg(n, t, None) }
  lazy val methodArgs = rep("(" ~> opt("implicit") ~ rep1sep(methodArg, ",") <~ ")")

  lazy val classArg = opt("implicit") ~ opt("override") ~ opt("val") ~ scalanIdent ~ (":" ~> tpeExpr) ^^ {
    case imp ~ over ~ value ~ n ~ t =>
      ClassArg(imp.isDefined, over.isDefined, value.isDefined, n, t, None)
  }
  lazy val classArgs = "(" ~> rep1sep(classArg, ",") <~ ")"

  lazy val methodBody = "???"

  lazy val methodDef = (opt("implicit") <~ "def") ~ scalanIdent ~ opt(tpeArgs) ~ (methodArgs <~ ":") ~ tpeExpr ~ opt("=" ~> methodBody) ^^ {
    case implicitModifier ~ n ~ targs ~ args ~ tres ~ _ =>
      MethodDef(n, targs.toList.flatten, args.map { case i ~ args => MethodArgs(i.isDefined, args) }, tres, implicitModifier.isDefined)
  }

  lazy val importStat = "import" ~> qualId <~ opt(";") ^^ { case ns => ImportStat(ns) }

  lazy val tpeDef = "type" ~> scalanIdent ~ opt(tpeArgs) ~ ("=" ~> tpeExpr <~ opt(";")) ^^ {
    case n ~ targs ~ rhs => TpeDef(n, targs.toList.flatten, rhs)
  }

  lazy val bodyItem: Parser[BodyItem] =
    methodDef |
      importStat |
      tpeDef |
      traitDef |
      classDef |
      objectDef

  lazy val bodyItems = rep(bodyItem)

  lazy val selfType = (scalanIdent <~ ":") ~ (tpeExpr <~ "=>") ^^ { case n ~ t => SelfTypeDef(n, List(t)) }

  lazy val traitBody = "{" ~> opt(selfType) ~ opt(bodyItems) <~ "}" ^^ {
    case self ~ body => (self, body.toList.flatten)
  }

  lazy val traitDef: Parser[TraitDef] = ("trait" ~> scalanIdent) ~ opt(tpeArgs) ~ opt(extendsList) ~ traitBody ^^ {
    case n ~ targs ~ ancs ~ body =>
      TraitDef(n, targs.toList.flatten, ancs.toList.flatten, body._2, body._1)
  }

  lazy val classDef: Parser[ClassDef] =
    opt("abstract") ~ ("class" ~> scalanIdent) ~ opt(tpeArgs) ~ opt(classArgs) ~ opt(classArgs) ~ opt(extendsList) ~ opt(traitBody) ^^ {
      case abs ~ n ~ targs ~ args ~ impArgs ~ ancs ~ body =>
        ClassDef(n, targs.flatList, args.flatList, impArgs.flatList, ancs.flatList, body.map(_._2).flatList, body.map(_._1).flatten, abs.isDefined)
    }
  
  lazy val objectDef: Parser[ObjectDef] =
    ("object" ~> scalanIdent) ~ opt(extendsList) ~ opt("{" ~> opt(bodyItems) <~ "}") ^^ {
      case n ~ ancs ~ body =>
        ObjectDef(n, ancs.flatList, body.flatten.flatList)
    }
  
  def config: CodegenConfig

  lazy val entityModuleDef =
    ("package" ~> qualId <~ opt(";")) ~
      repsep(importStat, opt(";")) ~
      traitDef ^^ {
        case ns ~ imports ~ moduleTrait => {
          val packageName = ns.mkString(".")
          EntityModuleDef.fromModuleTrait(packageName, imports, moduleTrait, config)
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
    def handleError(msg: String, next: Input) = {
      val rest = next.rest.source.toString
      val source = next.source.toString
      throw new ParseException(s"$msg pos:${next.pos} rest:$rest", next.offset)
    }

    val res = parseAll(entityModuleDef, s)
    res match {
      case Success(r, next) => r
      case Failure(msg, next) => handleError(msg, next)
      case Error(msg, next) => handleError(msg, next)
    }
  }

}

object ScalanImpl
  extends ScalanParsers
  with ScalanAst {
  val config = BoilerplateTool.liteConfig
}
