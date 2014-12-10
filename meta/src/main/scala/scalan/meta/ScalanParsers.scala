/**
 * User: Alexander Slesarenko
 * Date: 11/17/13
 */
package scalan.meta

import scala.tools.nsc.interactive.Global
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.StoreReporter
import scala.language.implicitConversions
import scala.reflect.internal.util.RangePosition
import scala.reflect.internal.util.OffsetPosition

object ScalanAst {

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

    def unRep(module: SEntityModuleDef, config: CodegenConfig) = self match {
      case STraitCall("Rep", Seq(t)) => Some(t)
      case STraitCall(name, args) =>
        val typeSynonyms = config.entityTypeSynonyms ++
          module.entityRepSynonym.toSeq.map(typeSyn => typeSyn.name -> module.entityOps.name).toMap
        typeSynonyms.get(name).map(unReppedName => STraitCall(unReppedName, args))
      case _ => None
    }

    def isRep(module: SEntityModuleDef, config: CodegenConfig) = unRep(module, config) match {
      case Some(_) => true
      case None => false
    }
  }

  // SExpr universe --------------------------------------------------------------------------
  case class SExpr(expr: String)

  // SBodyItem universe ----------------------------------------------------------------------
  abstract class SBodyItem
  case class SImportStat(name: String) extends SBodyItem
  case class SMethodDef(name: String, tpeArgs: STpeArgs, args: List[SMethodArgs],
    tpeRes: Option[STpeExpr], isImplicit: Boolean) extends SBodyItem {
    def explicitArgs = args.filter(!_.impFlag).flatMap(_.args)
  }
  case class SValDef(name: String, tpe: Option[STpeExpr], isLazy: Boolean, isImplicit: Boolean) extends SBodyItem
  case class STpeDef(name: String, tpeArgs: STpeArgs, rhs: STpeExpr) extends SBodyItem

  case class STpeArg(name: String, bound: Option[STpeExpr], contextBound: List[String])
  type STpeArgs = List[STpeArg]

  case class SMethodArg(name: String, tpe: STpeExpr, default: Option[SExpr])
  case class SMethodArgs(impFlag: Boolean, args: List[SMethodArg])

  case class SClassArg(impFlag: Boolean, overFlag: Boolean, valFlag: Boolean, name: String, tpe: STpeExpr, default: Option[SExpr])
  type SClassArgs = List[SClassArg]

  case class SSelfTypeDef(name: String, components: List[STpeExpr]) {
    def tpe = components.mkString(" with ")
  }

  abstract class STraitOrClassDef extends SBodyItem {
    def name: String
    def tpeArgs: List[STpeArg]
    def ancestors: List[STraitCall]
    def body: List[SBodyItem]
    def selfType: Option[SSelfTypeDef]
    def companion: Option[STraitOrClassDef]
  }

  case class STraitDef(
    name: String,
    tpeArgs: List[STpeArg],
    ancestors: List[STraitCall],
    body: List[SBodyItem],
    selfType: Option[SSelfTypeDef],
    companion: Option[STraitOrClassDef]) extends STraitOrClassDef

  case class SClassDef(
    name: String,
    tpeArgs: List[STpeArg],
    args: SClassArgs,
    implicitArgs: SClassArgs,
    ancestors: List[STraitCall],
    body: List[SBodyItem],
    selfType: Option[SSelfTypeDef],
    companion: Option[STraitOrClassDef],
    isAbstract: Boolean) extends STraitOrClassDef

  case class SObjectDef(
    name: String,
    ancestors: List[STraitCall],
    body: List[SBodyItem]) extends SBodyItem

  case class SEntityModuleDef(
    packageName: String,
    imports: List[SImportStat],
    name: String,
    entityRepSynonym: Option[STpeDef],
    entityOps: STraitDef,
    concreteSClasses: List[SClassDef],
    selfType: Option[SSelfTypeDef])

  def getConcreteClasses(defs: List[SBodyItem]) = defs.collect { case c: SClassDef => c }

  object SEntityModuleDef {
    def fromModuleTrait(packageName: String, imports: List[SImportStat], moduleTrait: STraitDef, config: CodegenConfig): SEntityModuleDef = {
      val moduleName = moduleTrait.name
      val defs = moduleTrait.body

      val entityRepSynonym = defs.collectFirst { case t: STpeDef => t }

      val opsTrait = defs.collectFirst { case t: STraitDef => t }.getOrElse {
        throw new IllegalStateException(s"Invalid syntax of entity module trait $moduleName. First member trait must define the entity, but no member traits found.")
      }
      val classes = getConcreteClasses(defs)

      SEntityModuleDef(packageName, imports, moduleName, entityRepSynonym, opsTrait, classes, moduleTrait.selfType)
    }
  }
}

trait ScalanParsers {
  import ScalanAst._
  val settings = new Settings
  settings.usejavacp.value = true
  val reporter = new StoreReporter
  val compiler: Global = new Global(settings, reporter)

  import compiler._
  implicit def nameToString(name: compiler.Name): String = name.toString

  implicit class OptionListOps[A](opt: Option[List[A]]) {
    def flatList: List[A] = opt.toList.flatten
  }

  private def positionString(tree: Tree) = {
    tree.pos match {
      case pos: RangePosition =>
        val path = pos.source.file.canonicalPath
        s"file $path at ${pos.line}:${pos.column} (start ${pos.point - pos.start} before, end ${pos.end - pos.point} after)"
      case pos: OffsetPosition =>
        val path = pos.source.file.canonicalPath
        s"file $path at ${pos.line}:${pos.column}"
      case pos => pos.toString
    }
  }

  def !!!(msg: String, tree: Tree) = {
    val fullMsg = s"$msg at ${positionString(tree)}"
    throw new IllegalStateException(fullMsg)
  }

  def !!!(msg: String) = {
    throw new IllegalStateException(msg)
  }

  def ???(tree: Tree) = {
    val pos = tree.pos
    val msg = s"Unhandled case in ${positionString(tree)}:\nAST: ${showRaw(tree)}\n\nCode for AST: $tree"
    throw new IllegalStateException(msg)
  }

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
    val packageName = pdTree.pid.toString
    val statements = pdTree.stats
    val imports = statements.collect {
      case i: Import => importStat(i)
    }
    val moduleTraitTree = statements.collect {
      case cd: ClassDef if cd.mods.isTrait && !cd.name.contains("Dsl") => cd
    } match {
      case Seq(only) => only
      case seq => !!!(s"There must be exactly one module trait in file, found ${seq.length}")
    }
    val moduleTrait = traitDef(moduleTraitTree, moduleTraitTree)
    SEntityModuleDef.fromModuleTrait(packageName, imports, moduleTrait, config)
  }

  def importStat(i: Import): SImportStat = {
    SImportStat(i.toString.stripPrefix("import "))
  }

  def isEvidenceParam(vd: ValDef) = vd.name.toString.startsWith("evidence$")

  def tpeArgs(typeParams: List[TypeDef], possibleImplicits: List[ValDef]): List[STpeArg] = {
    val evidenceTypes = possibleImplicits.filter(isEvidenceParam(_)).map(_.tpt)

    def tpeArg(tdTree: TypeDef): STpeArg = {
      val bound = tdTree.rhs match {
        case TypeBoundsTree(low, high) =>
          if (high.toString == "_root_.scala.Any")
            None
          else
            optTpeExpr(high)
        case _ => ???(tdTree)
      }
      val contextBounds = evidenceTypes.collect {
        case AppliedTypeTree(tpt, List(arg)) if arg.toString == tdTree.name.toString =>
          Some(tpt.toString)
        case _ => None
      }.flatten
      STpeArg(tdTree.name, bound, contextBounds)
    }

    typeParams.map(tpeArg)
  }

  // exclude default parent
  def ancestors(trees: List[Tree]) = trees.map(traitCall).filter(_.name != "AnyRef")

  def traitDef(td: ClassDef, parentScope: ImplDef): STraitDef = {
    val tpeArgs = this.tpeArgs(td.tparams, Nil)
    val ancestors = this.ancestors(td.impl.parents)
    val body = td.impl.body.flatMap(optBodyItem(_, td))
    val selfType = this.selfType(td.impl.self)
    val name = td.name.toString
    val companion = parentScope.impl.body.collect {
      case c: ClassDef if c.name.toString == name + "Companion" =>
        if (c.mods.isTrait) traitDef(c, parentScope) else classDef(c, parentScope)
    }.headOption
    STraitDef(name, tpeArgs, ancestors, body, selfType, companion)
  }

  def classDef(cd: ClassDef, parentScope: ImplDef): SClassDef = {
    val ancestors = this.ancestors(cd.impl.parents)
    val constructor = (cd.impl.body.collect {
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
    val tpeArgs = this.tpeArgs(cd.tparams, constructor.vparamss.lastOption.getOrElse(Nil))
    val body = cd.impl.body.flatMap(optBodyItem(_, cd))
    val selfType = this.selfType(cd.impl.self)
    val isAbstract = cd.mods.hasAbstractFlag
    val name = cd.name.toString
    val companion = parentScope.impl.body.collect {
      case c: ClassDef if c.name.toString == name + "Companion" =>
        if (c.mods.isTrait) traitDef(c, parentScope) else classDef(c, parentScope)
    }.headOption
    SClassDef(cd.name, tpeArgs, args, implicitArgs, ancestors, body, selfType, companion, isAbstract)
  }

  def objectDef(od: ModuleDef): SObjectDef = {
    val ancestors = this.ancestors(od.impl.parents)
    val body = od.impl.body.flatMap(optBodyItem(_, od))
    SObjectDef(od.name, ancestors, body)
  }

  def classArgs(vds: List[ValDef]): SClassArgs = vds.filter(!isEvidenceParam(_)).map(classArg)

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

  def optBodyItem(tree: Tree, parentScope: ImplDef): Option[SBodyItem] = tree match {
    case i: Import =>
      Some(importStat(i))
    case md: DefDef =>
      if (!nme.isConstructorName(md.name))
        md.tpt match {
          case AppliedTypeTree(tpt, _) if tpt.toString == "Elem" || tpt.toString == "Element" =>
            None
          case _ =>
            Some(methodDef(md))
        }
      else
        None
    case td: TypeDef =>
      val tpeArgs = this.tpeArgs(td.tparams, Nil)
      val rhs = tpeExpr(td.rhs)
      Some(STpeDef(td.name, tpeArgs, rhs))
    case td: ClassDef if td.mods.isTrait =>
      Some(traitDef(td, parentScope))
    case cd: ClassDef if !cd.mods.isTrait => // isClass doesn't exist
      Some(classDef(cd, parentScope))
    case od: ModuleDef =>
      Some(objectDef(od))
    case vd: ValDef =>
      if (!vd.mods.isParamAccessor) {
        val tpeRes = optTpeExpr(vd.tpt)
        val isImplicit = vd.mods.isImplicit
        val isLazy = vd.mods.isLazy
        Some(SValDef(vd.name, tpeRes, isImplicit, isLazy))
      } else
        None
    case EmptyTree =>
      None
    case tree => ???(tree)
  }

  def methodDef(md: DefDef) = {
    val tpeArgs = this.tpeArgs(md.tparams, md.vparamss.lastOption.getOrElse(Nil))
    val args0 = md.vparamss.map(methodArgs)
    val args = if (!args0.isEmpty && args0.last.args.isEmpty) args0.init else args0
    val tpeRes = optTpeExpr(md.tpt)
    val isImplicit = md.mods.isImplicit
    SMethodDef(md.name, tpeArgs, args, tpeRes, isImplicit)
  }

  def methodArgs(vds: List[ValDef]): SMethodArgs = vds match {
    case Nil => SMethodArgs(false, List.empty)
    case vd :: _ =>
      val isImplicit = vd.mods.isImplicit
      SMethodArgs(isImplicit, vds.filter(!isEvidenceParam(_)).map(methodArg))
  }

  def optTpeExpr(tree: Tree): Option[STpeExpr] = {
    tree match {
      case _ if tree.isEmpty => None
      case _: ExistentialTypeTree => None
      case tree => Some(tpeExpr(tree))
    }
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
    case tree => SExpr(tree.toString)
  }

  def methodArg(vd: ValDef): SMethodArg = {
    val tpe = tpeExpr(vd.tpt)
    val default = optExpr(vd.rhs)
    SMethodArg(vd.name, tpe, default)
  }

  def selfType(vd: ValDef): Option[SSelfTypeDef] = {
    val components = vd.tpt match {
      case t if t.isEmpty =>
        Nil
      case CompoundTypeTree(Template(ancestors, _, _)) =>
        ancestors.map(tpeExpr)
      case t =>
        List(tpeExpr(t))
    }

    if (components.isEmpty)
      None
    else
      Some(SSelfTypeDef(vd.name.toString, components))
  }
}
