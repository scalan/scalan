/**
 * User: Alexander Slesarenko
 * Date: 11/17/13
 */
package scalan.meta

import java.io.File

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
  case class STpePrimitive(typeName: String, defaultValueString: String) extends STpeExpr {
    override def toString = typeName
  }
  val STpePrimitives = Map(
    "Int" -> STpePrimitive("Int", "0"),
    "Long" -> STpePrimitive("Long", "0l"),
    "Byte" -> STpePrimitive("Byte", "0.toByte"),
    "Boolean" -> STpePrimitive("Boolean", "false"),
    "Float" -> STpePrimitive("Float", "0.0f"),
    "Double" -> STpePrimitive("Double", "0.0"),
    "String" -> STpePrimitive("String", "\"\"")
  )
  case class STpeTuple(items: List[STpeExpr]) extends STpeExpr {
    override def toString = items.mkString("(", ",", ")")
  }
  case class STpeFunc(domain: STpeExpr, range: STpeExpr) extends STpeExpr {
    override def toString = {
      val domainStr = domain match {
        case tuple: STpeTuple => s"($tuple)"
        case _ => domain.toString
      }
      s"$domainStr => $range"
    }
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

    def isTupledFunc = self match {
      case STraitCall("Rep", List(STpeFunc(STpeTuple(a1 :: a2 :: tail), _))) => true
      case STpeFunc(STpeTuple(a1 :: a2 :: tail), _) => true
      case _ => false
    }
  }

  // SExpr universe --------------------------------------------------------------------------
  case class SExpr(expr: String)

  // SBodyItem universe ----------------------------------------------------------------------
  abstract class SBodyItem
  case class SImportStat(name: String) extends SBodyItem

  trait MethodAnnotation
  case object ExternalMethod extends MethodAnnotation
  case object ExternalConstructor extends MethodAnnotation

  trait ArgAnnotation
  case object ArgList extends ArgAnnotation

  case class SMethodDef(name: String, tpeArgs: STpeArgs, argSections: List[SMethodArgs],
    tpeRes: Option[STpeExpr], isImplicit: Boolean, overloadId: Option[String], external: Option[MethodAnnotation], elem: Option[Unit] = None) extends SBodyItem {
    def explicitArgs = argSections.filter(!_.impFlag).flatMap(_.args)
    def allArgs = argSections.flatMap(_.args)
  }
  case class SValDef(name: String, tpe: Option[STpeExpr], isLazy: Boolean, isImplicit: Boolean) extends SBodyItem
  case class STpeDef(name: String, tpeArgs: STpeArgs, rhs: STpeExpr) extends SBodyItem

  case class STpeArg(name: String, bound: Option[STpeExpr], contextBound: List[String], tparams: List[STpeArg] = Nil) {
    def isHighKind = !tparams.isEmpty
    def declaration: String =
      if (isHighKind) {
        val params = tparams.map(_.declaration).mkString(",")
        s"$name[$params]"
      }
      else name
  }
  type STpeArgs = List[STpeArg]

  case class SMethodArg(name: String, tpe: STpeExpr, default: Option[SExpr], annotations: List[ArgAnnotation] = Nil)
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
    def isTrait: Boolean
    def isHighKind = tpeArgs.exists(_.isHighKind)

    def getMethodsWithAnnotation(a: MethodAnnotation) = body.collect {
      case md: SMethodDef if md.external.fold(false)(_ == a) => md
    }

    def getFieldDefs: List[SMethodDef] = body.collect {
      case md: SMethodDef if md.allArgs.isEmpty => md
    }

    def getAncestorTraits(module: SEntityModuleDef): List[STraitDef] = {
      ancestors.filter(tc => module.isEntity(tc.name)).map(tc => module.getEntity(tc.name))
    }

    def getAvailableFields(module: SEntityModuleDef): Set[String] = {
      getFieldDefs.map(_.name).toSet ++ getAncestorTraits(module).flatMap(_.getAvailableFields(module))
    }
  }

  case class STraitDef(
    name: String,
    tpeArgs: List[STpeArg],
    ancestors: List[STraitCall],
    body: List[SBodyItem],
    selfType: Option[SSelfTypeDef],
    companion: Option[STraitOrClassDef]) extends STraitOrClassDef {
    def isTrait = true
  }

  final val BaseTypeTraitName = "BaseTypeEx"

  implicit class STraitDefOps(td: STraitDef) {
    def optBaseType: Option[STraitCall] = td.ancestors.find(a => a.name == BaseTypeTraitName) match {
      case Some(STraitCall(_, (h: STraitCall) :: _)) => Some(h)
      case _ => None
    }
  }

  case class SClassDef(
    name: String,
    tpeArgs: List[STpeArg],
    args: SClassArgs,
    implicitArgs: SClassArgs,
    ancestors: List[STraitCall],
    body: List[SBodyItem],
    selfType: Option[SSelfTypeDef],
    companion: Option[STraitOrClassDef],
    isAbstract: Boolean) extends STraitOrClassDef {
    def isTrait = false
  }

  case class SObjectDef(
    name: String,
    ancestors: List[STraitCall],
    body: List[SBodyItem]) extends SBodyItem

  case class SSeqImplementation(explicitMethods: List[SMethodDef]) {
    def containsMethodDef(m: SMethodDef) = {
      val equalSignature = for {
        eq <- explicitMethods.filter(em => em.name == m.name)
              if eq.allArgs == m.allArgs && eq.tpeArgs == m.tpeArgs
      } yield eq
      equalSignature.length > 0
    }
  }

  case class SEntityModuleDef(
    packageName: String,
    imports: List[SImportStat],
    name: String,
    entityRepSynonym: Option[STpeDef],
    entityOps: STraitDef,
    entities: List[STraitDef],
    concreteSClasses: List[SClassDef],
    methods: List[SMethodDef],
    selfType: Option[SSelfTypeDef],
    seqDslImpl: Option[SSeqImplementation] = None)
  {
    def getEntity(name: String): STraitDef = {
      val entity = entities.find(e => e.name == name)
      entity match {
        case Some(e) => e
        case _ => sys.error(s"Cannot find entity with name $name: available entities ${entities.map(_.name)}")
      }
    }
    def isEntity(name: String) = entities.exists(e => e.name == name)
  }

  def getConcreteClasses(defs: List[SBodyItem]) = defs.collect { case c: SClassDef => c }

  object SEntityModuleDef {
    def getImplicitArgs(entity: STraitDef): SClassArgs = {
      val implicitElems = entity.body.collect {
        case md @ SMethodDef(name, _, _, Some(elem @ STraitCall("Elem", List(tyArg))), true, _, _, Some(_)) => (name, elem)
      }
      val args = entity.tpeArgs.map(a => {
        val optDef = implicitElems.collectFirst {
          case (methName, elem @ STraitCall("Elem", List(STraitCall(name, _)))) if name == a.name =>
            (methName, elem)
        }

        optDef.map { case (name, tyElem) => {
          SClassArg(true, false, true, name, tyElem, None)
        }}
      })
      val missingElems = args.filterNot(_.isDefined)
      if (missingElems.length > 0)
        sys.error(s"implicit def eA: Elem[A] should be declared for all type parameters: missing ${missingElems}")
      args.flatMap(a => a)
    }

    def tpeUseExpr(arg: STpeArg): STpeExpr = STraitCall(arg.name, arg.tparams.map(tpeUseExpr(_)))

    def apply(packageName: String, imports: List[SImportStat], moduleTrait: STraitDef, config: CodegenConfig): SEntityModuleDef = {
      val moduleName = moduleTrait.name
      val defs = moduleTrait.body

      val entityRepSynonym = defs.collectFirst { case t: STpeDef => t }

      val traits = defs.collect { case t: STraitDef if !t.name.endsWith("Companion") => t }
      val entity = traits.headOption.getOrElse {
        throw new IllegalStateException(s"Invalid syntax of entity module trait $moduleName. First member trait must define the entity, but no member traits found.")
      }

      val classes = entity.optBaseType match {
        case Some(bt) =>
          val entityName = entity.name
          val entityImplName = entityName + "Impl"
          val typeUseExprs = entity.tpeArgs.map(tpeUseExpr(_))
          val defaultBTImpl = SClassDef(
            name = entityImplName,
            tpeArgs = entity.tpeArgs,
            args = List(SClassArg(false, false, true, "wrappedValueOfBaseType", STraitCall("Rep", List(bt)), None)),
            implicitArgs = getImplicitArgs(entity),
            ancestors = List(STraitCall(entity.name, typeUseExprs)),
            body = List(

            ),
            selfType = None,
            companion = None,
//            companion = defs.collectFirst {
//              case c: STraitOrClassDef if c.name.toString == entityImplName + "Companion" => c
//            },
            true
          )
          defaultBTImpl :: getConcreteClasses(defs)
        case None => getConcreteClasses(defs)
      }

      val methods = defs.collect { case md: SMethodDef => md }

      SEntityModuleDef(packageName, imports, moduleName, entityRepSynonym, entity, traits, classes, methods, moduleTrait.selfType)
    }
  }
}

trait ScalanParsers {
  import ScalanAst._
  val settings = new Settings
  settings.embeddedDefaults(getClass.getClassLoader)
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

  def parseEntityModule(file: File) = {
    val source = compiler.getSourceFile(file.getPath)
    val tree = compiler.parseTree(source)
    tree match {
      case pd: PackageDef =>
        entityModule(pd)
      case tree =>
        throw new Exception(s"Unexpected tree in file $file:\n\n$tree")
    }
  }

  def seqImplementation(methods: List[DefDef], parent: Tree): List[SMethodDef] = {
    methods.map(methodDef(_))
  }

  def entityModule(fileTree: PackageDef) = {
    val packageName = fileTree.pid.toString
    val statements = fileTree.stats
    val imports = statements.collect {
      case i: Import => importStat(i)
    }
    val moduleTraitTree = statements.collect {
      case cd: ClassDef if cd.mods.isTrait && !cd.name.contains("Dsl") => cd
    } match {
      case Seq(only) => only
      case seq => !!!(s"There must be exactly one module trait in file, found ${seq.length}")
    }

    val moduleTraitDef = traitDef(moduleTraitTree, moduleTraitTree)
    val module = SEntityModuleDef(packageName, imports, moduleTraitDef, config)
    val moduleName = moduleTraitDef.name

    val dslSeq = fileTree.stats.collectFirst {
      case cd @ ClassDef(_,name,_,_) if name.toString == (moduleName + "DslSeq") => cd
    }
    val seqExplicitOps = for {
      seqImpl <- dslSeq
      seqOpsTrait <- seqImpl.impl.body.collectFirst {
        case cd @ ClassDef(_,name,_,_) if name.toString == ("Seq" + module.entityOps.name) => cd
      }
    } yield {
      val cd = seqOpsTrait.impl.body.collect { case item: DefDef => item }
      seqImplementation(cd, seqOpsTrait)
    }

    module.copy(seqDslImpl = seqExplicitOps.map(SSeqImplementation(_)))
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
      val tparams = tdTree.tparams.map(tpeArg)
      STpeArg(tdTree.name, bound, contextBounds, tparams)
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
            Some(methodDef(md, true))
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

  case class SAnnotation(name: String, args: List[Tree])

  class MethodAnnotation(anClass: String) {
    def unapply(md: DefDef): Option[List[Tree]] =
      md.mods.annotations.collectFirst {
        case Apply(Select(New(Ident(ident)), nme.CONSTRUCTOR), args)
          if ident.toString == anClass => args
      }
  }
  val HasExternalAnnotation = new MethodAnnotation("External")
  val HasConstructorAnnotation = new MethodAnnotation("Constructor")
  val OverloadIdAnnotation = new MethodAnnotation("OverloadId")

  def ArgAnnotation(anClass: String) = new {
    def unapply(md: ValDef): Option[List[Tree]] =
      md.mods.annotations.collectFirst {
        case Apply(Select(New(Ident(ident)), nme.CONSTRUCTOR), args)
          if ident.toString == anClass => args
      }
  }
  val HasArgListAnnotation = ArgAnnotation("ArgList")

  def methodDef(md: DefDef, isElem: Boolean = false) = {
    val tpeArgs = this.tpeArgs(md.tparams, md.vparamss.lastOption.getOrElse(Nil))
    val args0 = md.vparamss.map(methodArgs)
    val args = if (!args0.isEmpty && args0.last.args.isEmpty) args0.init else args0
    val tpeRes = optTpeExpr(md.tpt)
    val isImplicit = md.mods.isImplicit
    val optOverloadId = md match {
      case OverloadIdAnnotation(List(Literal(Constant(overloadId)))) =>
        Some(overloadId.toString)
      case _ => None
    }
    val optExternal = md match {
      case HasExternalAnnotation(_) => Some(ExternalMethod)
      case HasConstructorAnnotation(_) => Some(ExternalConstructor)
      case _ => None
    }
    SMethodDef(md.name, tpeArgs, args, tpeRes, isImplicit, optOverloadId, optExternal, if (isElem) Some(()) else None)
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
      val name = ident.name.toString
      STpePrimitives.getOrElse(name, STraitCall(name, List()))
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
    val annotations = vd match {
      case HasArgListAnnotation(_) => List(ArgList)
      case _ => Nil
    }
    SMethodArg(vd.name, tpe, default, annotations)
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
