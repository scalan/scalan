/**
 * User: Alexander Slesarenko
 * Date: 11/17/13
 */
package scalan.meta

import java.text.ParseException
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.interactive.Response
import scala.language.implicitConversions

trait ScalanAst {

  // STpe universe --------------------------------------------------------------------------
  sealed abstract class STpeExpr
  type STpeExprs = List[STpeExpr]
  case class STraitCall(name: String, tpeSExprs: List[STpeExpr]) extends STpeExpr {
    override def toString = name + (if (tpeSExprs.isEmpty) "" else tpeSExprs.mkString("[", ",", "]"))
  }
  case object STpeInt extends STpeExpr { override def toString = "Int" }
  case object STpeBoolean extends STpeExpr { override def toString = "Boolean" }
  case object STpeFloat extends STpeExpr { override def toString = "Float" }
  case object STpeString extends STpeExpr { override def toString = "String" }
  case class STpeTuple(items: List[STpeExpr]) extends STpeExpr {
    override def toString = items.mkString("(", ",", ")")
  }
  case class STpeFunc(domain: STpeExpr, range: STpeExpr) extends STpeExpr {
    override def toString = s"$domain => $range"
  }
  case class STpeSum(items: List[STpeExpr]) extends STpeExpr {
    override def toString = items.mkString("(", "|", ")")
  }

  implicit class STpeExprExtensions(self: STpeExpr) {
    def applySubst(subst: Map[String, STpeExpr]): STpeExpr = self match {
      case STraitCall(n, args) => // higher-kind usage of names is not supported  Array[A] - ok, A[Int] - nok
        subst.get(n) match {
          case Some(t) => t
          case None => STraitCall(n, args map { _.applySubst(subst) })
        }
      case STpeTuple(items) => STpeTuple(items map { _.applySubst(subst) })
      case STpeSum(items) => STpeSum(items map { _.applySubst(subst) })
      case _ => self
    }
  }

  // SExpr universe --------------------------------------------------------------------------
  abstract class SExpr
  case class SMethodCall(obj: SExpr, name: String, args: List[SExpr]) extends SExpr

  // SBodyItem universe ----------------------------------------------------------------------
  abstract class SBodyItem
  case class SImportStat(name: String) extends SBodyItem
  case class SMethodDef(name: String, tpeArgs: STpeArgs, args: List[SMethodArgs],
    tpeRes: Option[STpeExpr], isImplicit: Boolean) extends SBodyItem
  case class SValDef(name: String, tpe: Option[STpeExpr], isLazy: Boolean, isImplicit: Boolean) extends SBodyItem
  case class STpeDef(name: String, tpeArgs: STpeArgs, rhs: STpeExpr) extends SBodyItem

  case class STpeArg(name: String, bound: Option[STpeExpr], contextBound: List[String])
  type STpeArgs = List[STpeArg]

  case class SMethodArg(name: String, tpe: STpeExpr, default: Option[SExpr])
  case class SMethodArgs(impFlag: Boolean, args: List[SMethodArg])

  case class SClassArg(impFlag: Boolean, overFlag: Boolean, valFlag: Boolean, name: String, tpe: STpeExpr, default: Option[SExpr])
  type SClassArgs = List[SClassArg]

  case class SSelfTypeDef(name: String, components: List[STpeExpr])

  case class STraitDef(
    name: String,
    tpeArgs: List[STpeArg],
    ancestors: List[STraitCall],
    body: List[SBodyItem],
    selfType: Option[SSelfTypeDef]) extends SBodyItem

  case class SClassDef(
    name: String,
    tpeArgs: List[STpeArg],
    args: SClassArgs,
    implicitArgs: SClassArgs,
    ancestors: List[STraitCall],
    body: List[SBodyItem],
    selfType: Option[SSelfTypeDef],
    isAbstract: Boolean) extends SBodyItem

  case class SObjectDef(
    name: String,
    ancestors: List[STraitCall],
    body: List[SBodyItem]) extends SBodyItem

  case class SEntityModuleDef(
    packageName: String,
    imports: List[SImportStat],
    name: String,
    typeSyn: STpeDef,
    entityOps: STraitDef,
    concreteSClasses: List[SClassDef],
    selfType: Option[SSelfTypeDef]) {

    object EntityRepType {
      def unapply(ty: STpeExpr): Option[STpeExpr] = ty match {
        case STraitCall(n, args) if n == typeSyn.name =>
          typeSyn.rhs match {
            case STraitCall("Rep", List(entityType)) =>
              val subst = (typeSyn.tpeArgs.map(_.name) zip args).toMap
              Some(entityType.applySubst(subst))
            case _ => sys.error(s"Entity type synonym should be Rep type but was $typeSyn")
          }
        case _ => None
      }
    }
  }

  def getConcreteClasses(defs: List[SBodyItem]) = defs.collect { case c: SClassDef => c }

  object SEntityModuleDef {
    def fromModuleTrait(packageName: String, imports: List[SImportStat], moduleTrait: STraitDef, config: CodegenConfig): SEntityModuleDef = {
      val moduleName = moduleTrait.name
      val defs = moduleTrait.body

      val (typeSyn, opsTrait) = defs match {
        case (ts: STpeDef) :: (ot: STraitDef) :: _ => (ts, ot)
        // constructor
        case (_: SMethodDef) :: (ts: STpeDef) :: (ot: STraitDef) :: _ => (ts, ot)
        case _ =>
          throw new ParseException(s"Invalid syntax of Entity module trait $moduleName:\n${defs.mkString("\n")}", 0)
      }
      val classes = getConcreteClasses(defs)

      val extraImports = config.extraImports.map(i => SImportStat(i))

      SEntityModuleDef(packageName, imports ++ extraImports, moduleName, typeSyn, opsTrait, classes, moduleTrait.selfType)
    }
  }
}

trait ScalanParsers { self: ScalanAst =>
  val settings = new Settings
  val reporter = new StoreReporter
  val compiler: Global = new Global(settings, reporter)

  import compiler._
  implicit def nameToString(name: compiler.Name): String = name.toString

  implicit class OptionListOps[A](opt: Option[List[A]]) {
    def flatList: List[A] = opt.toList.flatten
  }

  def !!!(msg: String, tree: Tree) = {
    // TODO better position reporting
    throw new ParseException(msg, tree.pos.startOrPoint)
  }

  def !!!(msg: String) = {
    // TODO better position reporting
    throw new ParseException(msg, -1)
  }

  def ???(tree: Tree) = {
    val pos = tree.pos
    val msg = s"Unhandled case at $pos:\n${showRaw(tree)}\n\nIn code: $tree"
    val offset = if (pos.isDefined) pos.startOrPoint else -1
    throw new ParseException(msg, offset)
  }

  //  def wrapIfMany[A <: STpeExpr, B <: STpeExpr](w: List[A] => B, xs: List[A]): STpeExpr = {
  //    val sz = xs.size
  //    assert(sz >= 1)
  //    if (sz > 1) w(xs) else xs.head
  //  }
  //
  //  val keywords = Set("def", "trait", "type", "class", "abstract", "with")
  //  
  //  val op = """[!#%&*+-/:<=>?@\^|~]*""".r
  //
  //  lazy val scalanIdent = (ident | op) ^? ({ case s if !keywords.contains(s) => s }, s => s"Keyword $s cannot be used as identifier")
  //
  //  lazy val bracedIdentList = "{" ~> rep1sep(scalanIdent, ",") <~ "}" ^^ { case xs => xs.mkString("{", ",", "}") }
  //
  //  lazy val qualId = rep1sep(scalanIdent | bracedIdentList, ".")
  //
  //  lazy val extendsList = "extends" ~> rep1sep(traitCall, "with")
  //
  //  lazy val tpeBase: Parser[STpeExpr] = "Int" ^^^ { STpeInt } |
  //    "Boolean" ^^^ { STpeBoolean } |
  //    "Float" ^^^ { STpeFloat } |
  //    "String" ^^^ { STpeString } |
  //    traitCall
  //
  //  lazy val tpeFactor = tpeBase | "(" ~> tpeSExpr <~ ")"
  //
  //  lazy val tpeFunc = rep1sep(tpeFactor, "=>") ^^ { wrapIfMany(STpeFunc, _) }
  //
  //  lazy val tpeTuple = "(" ~> rep1sep(tpeFunc, ",") <~ ")" ^^ { wrapIfMany(STpeTuple, _) }
  //
  //  lazy val tpeSExpr: Parser[STpeExpr] = opt("@uncheckedVariance") ~> rep1sep(tpeTuple | tpeFunc, "|") <~ opt("@uncheckedVariance") ^^ { wrapIfMany(STpeSum, _) }
  //
  //  lazy val traitCallArgs = "[" ~> rep1sep(tpeSExpr, ",") <~ "]"
  //
  //  lazy val traitCall = scalanIdent ~ opt(traitCallArgs) ^^ {
  //    case n ~ None => STraitCall(n)
  //    case n ~ Some(ts) => STraitCall(n, ts)
  //  }
  //
  //  lazy val methodArg = (scalanIdent <~ ":") ~ tpeSExpr ^^ { case n ~ t => SMethodArg(n, t, None) }
  //  lazy val methodArgs = rep("(" ~> opt("implicit") ~ rep1sep(methodArg, ",") <~ ")")
  //
  //  lazy val classArg = opt("implicit") ~ opt("override") ~ opt("val") ~ scalanIdent ~ (":" ~> tpeSExpr) ^^ {
  //    case imp ~ over ~ value ~ n ~ t =>
  //      SClassArg(imp.isDefined, over.isDefined, value.isDefined, n, t, None)
  //  }
  //  lazy val classArgs = "(" ~> rep1sep(classArg, ",") <~ ")"
  //
  //  lazy val noBraces = "[^{}]+".r
  //  lazy val balancedBraces: Parser[Unit] = "{" ~> rep(noBraces | balancedBraces) <~ "}" ^^^ { () }
  //  lazy val methodBody = "???" | balancedBraces
  //
  //  lazy val methodDef = (opt("implicit") <~ "def") ~ scalanIdent ~ opt(tpeArgs) ~ methodArgs ~ opt(":" ~> tpeSExpr) ~ opt("=" ~> methodBody) ^^ {
  //    case implicitModifier ~ n ~ targs ~ args ~ tres ~ _ =>
  //      SMethodDef(n, targs.toList.flatten, args.map { case i ~ args => SMethodArgs(i.isDefined, args) }, tres, implicitModifier.isDefined)
  //  }
  //  
  //  lazy val valDef = opt("implicit") ~ (opt("lazy") <~ "val") ~ scalanIdent ~ opt(":" ~> tpeSExpr) ~ opt("=" ~> methodBody) ^^ {
  //    case implicitMod ~ lazyMod ~ name ~ tpe ~ _ =>
  //      SValDef(name, tpe, lazyMod.isDefined, implicitMod.isDefined)
  //  }
  //
  //  lazy val importStat = "import" ~> qualId <~ opt(";") ^^ { case ns => SImportStat(ns) }
  //
  //  lazy val tpeDef = "type" ~> scalanIdent ~ opt(tpeArgs) ~ ("=" ~> tpeSExpr <~ opt(";")) ^^ {
  //    case n ~ targs ~ rhs => STpeDef(n, targs.toList.flatten, rhs)
  //  }
  //
  //  lazy val bodyItem: Parser[SBodyItem] =
  //    methodDef |
  //    valDef |
  //    importStat |
  //    tpeDef |
  //    traitDef |
  //    classDef |
  //    objectDef
  //
  //  lazy val bodyItems = rep(bodyItem)
  //
  //  lazy val selfType = (scalanIdent <~ ":") ~ (tpeSExpr <~ "=>") ^^ { case n ~ t => SSelfTypeDef(n, List(t)) }
  //
  //  lazy val traitBody = "{" ~> opt(selfType) ~ opt(bodyItems) <~ "}" ^^ {
  //    case self ~ body => (self, body.toList.flatten)
  //  }
  //
  //  lazy val classDef: Parser[SClassDef] =
  //    opt("abstract") ~ ("class" ~> scalanIdent) ~ opt(tpeArgs) ~ opt(classArgs) ~ opt(classArgs) ~ opt(extendsList) ~ opt(traitBody) ^^ {
  //      case abs ~ n ~ targs ~ args ~ impArgs ~ ancs ~ body =>
  //        SClassDef(n, targs.flatList, args.flatList, impArgs.flatList, ancs.flatList, body.map(_._2).flatList, body.map(_._1).flatten, abs.isDefined)
  //    }
  //  
  //  lazy val objectDef: Parser[SObjectDef] =
  //    ("object" ~> scalanIdent) ~ opt(extendsList) ~ opt("{" ~> opt(bodyItems) <~ "}") ^^ {
  //      case n ~ ancs ~ body =>
  //        SObjectDef(n, ancs.flatList, body.flatten.flatList)
  def config: CodegenConfig

  def parseEntityModule(filePath: String) = {
    val source = compiler.getSourceFile(filePath)
    val tree = compiler.parseTree(source)
    tree match {
      case pd: PackageDef =>
        entityModule(pd)
      case tree =>
        throw new Exception(s"Unexpected tree in file $filePath:\n\n$tree")
    }
  }

  def entityModule(pdTree: PackageDef) = {
    val packageName = pdTree.name.toString
    val statements = pdTree.stats
    val imports = statements.collect {
      case i: Import => SImportStat(i.toString)
    }
    val moduleTraitTree = statements.collect {
      case cd: ClassDef if cd.mods.isTrait => cd
    } match {
      case Seq(only) => only
      case seq => !!!(s"There must be exactly one module trait in file, found ${seq.length}")
    }
    val moduleTrait = traitDef(moduleTraitTree)
    SEntityModuleDef.fromModuleTrait(packageName, imports, moduleTrait, config)
  }

  def traitDef(td: ClassDef): STraitDef = {
    val tpeArgs = td.tparams.map(tpeArg)
    val ancestors = td.impl.parents.map(traitCall)
    val body = td.impl.body.flatMap(optBodyItem)
    val selfType = this.selfType(td.impl.self)
    STraitDef(td.name, tpeArgs, ancestors, body, selfType)
  }

  def classDef(cd: ClassDef): SClassDef = {
    val tpeArgs = cd.tparams.map(tpeArg)
    val ancestors = cd.impl.parents.map(traitCall)
    val constructor = (cd.impl.children.collect {
      case dd: DefDef if dd.name == nme.CONSTRUCTOR => dd
    }) match {
      case Seq(only) => only
      case seq => !!!(s"Class ${cd.name} should have 1 constructor but has ${seq.length} constructors", cd)
    }
    // TODO simplify
    val (args, implicitArgs) = constructor.vparamss match {
      case Seq() =>
        (classArgs(List.empty), classArgs(List.empty))
      case Seq(nonImplConArgs) =>
        (classArgs(nonImplConArgs), classArgs(List.empty))
      case Seq(nonImplConArgs, implConArgs) =>
        (classArgs(nonImplConArgs), classArgs(implConArgs))
      case seq => !!!(s"Constructor of class ${cd.name} has more than 2 parameter lists, not supported")
    }
    val body = cd.impl.body.flatMap(optBodyItem)
    val selfType = this.selfType(cd.impl.self)
    val isAbstract = cd.mods.hasAbstractFlag
    SClassDef(cd.name, tpeArgs, args, implicitArgs, ancestors, body, selfType, isAbstract)
  }

  def classArgs(vds: List[ValDef]): SClassArgs = vds.map(classArg)

  def classArg(vd: ValDef): SClassArg = {
    val tpe = tpeExpr(vd.tpt)
    val default = optExpr(vd.rhs)
    val isOverride = vd.mods.isAnyOverride
    val isVal = vd.mods.isParamAccessor
    SClassArg(vd.mods.isImplicit, isOverride, isVal, vd.name, tpe, default)
  }

  def traitCall(tree: Tree): STraitCall = tree match {
    case ident: Ident =>
      STraitCall(ident.name, List())
    case select: Select =>
      STraitCall(select.name, List())
    case AppliedTypeTree(tpt, args) =>
      STraitCall(tpt.toString, args.map(tpeExpr))
    case tree => ???(tree)
  }

  def optBodyItem(tree: Tree): Option[SBodyItem] = tree match {
    case md: DefDef =>
      Some(methodDef(md))
    case td: TypeDef =>
      val tpeArgs = td.tparams.map(tpeArg)
      val rhs = tpeExpr(td.rhs)
      Some(STpeDef(td.name, tpeArgs, rhs))
    case td: ClassDef if td.mods.isTrait =>
      Some(traitDef(td))
    case cd: ClassDef if !cd.mods.isTrait => // isClass doesn't exist
      Some(classDef(cd))
    case vd: ValDef =>
      val tpeRes = optTpeExpr(vd.tpt)
      val isImplicit = vd.mods.isImplicit
      val isLazy = vd.mods.isLazy
      Some(SValDef(vd.name, tpeRes, isImplicit, isLazy))
    case EmptyTree =>
      None
    case tree => ???(tree)
  }

  def methodDef(md: DefDef) = {
    val tpeArgs = md.tparams.map(tpeArg)
    val args = md.vparamss.map(methodArgs)
    val tpeRes = optTpeExpr(md.tpt)
    val isImplicit = md.mods.isImplicit
    SMethodDef(md.name, tpeArgs, args, tpeRes, isImplicit)
  }

  def methodArgs(vds: List[ValDef]): SMethodArgs = vds match {
    case Nil => SMethodArgs(false, List.empty)
    case vd :: _ =>
      val isImplicit = vd.mods.isImplicit
      SMethodArgs(isImplicit, vds.map(methodArg))
  }

  def optTpeExpr(tree: Tree): Option[STpeExpr] = {
    if (tree.isEmpty)
      None
    else
      Some(tpeExpr(tree))
  }

  def tpeExpr(tree: Tree): STpeExpr = tree match {
    case ident: Ident =>
      ident.name.toString match {
        case "Int" => STpeInt
        case "Boolean" => STpeBoolean
        case "Float" => STpeFloat
        case "String" => STpeString
        case name => STraitCall(name, List())
      }
    case select: Select =>
      STraitCall(select.name, List())
    case AppliedTypeTree(tpt, args) =>
      val argTpeExprs = args.map(tpeExpr)
      val genericTypeString = tpt.toString
      if (genericTypeString.contains("scala.Tuple"))
        STpeTuple(argTpeExprs)
      else if (genericTypeString.contains("scala.Function")) {
        val domainTpeExpr = argTpeExprs.length match {
          case 2 => argTpeExprs(0)
          case n => STpeTuple(argTpeExprs.init)
        }
        STpeFunc(domainTpeExpr, argTpeExprs.last)
      } else
        STraitCall(tpt.toString, argTpeExprs)
    case Annotated(_, arg) =>
      tpeExpr(arg)
    case tree => ???(tree)
  }

  def optExpr(tree: Tree): Option[SExpr] = {
    if (tree.isEmpty)
      None
    else
      Some(expr(tree))
  }

  def expr(tree: Tree): SExpr = tree match {
    case tree => ???(tree)
  }

  def methodArg(vd: ValDef): SMethodArg = {
    val tpe = tpeExpr(vd.tpt)
    val default = optExpr(vd.rhs)
    SMethodArg(vd.name, tpe, default)
  }

  def selfType(vd: ValDef): Option[SSelfTypeDef] = {
    if (vd.tpt.isEmpty)
      None
    else
      // TODO doesn't handle "with" correctly
      Some(SSelfTypeDef(vd.name, List(tpeExpr(vd.tpt))))
  }

  def tpeArg(tdTree: TypeDef): STpeArg = {
    val bound = tdTree.rhs match {
      case TypeBoundsTree(low, high) =>
        // TODO lower bound currently ignored
        optTpeExpr(high)
      case _ => ???(tdTree)
    }
    val contextBound = tdTree.rhs match {
      case TypeBoundsTree(_, _) =>
        List.empty
      case _ => ???(tdTree)
    }
    STpeArg(tdTree.name, bound, contextBound)
  }
}

object ScalanImpl
  extends ScalanParsers
  with ScalanAst {
  val config = BoilerplateTool.liteConfig
}
