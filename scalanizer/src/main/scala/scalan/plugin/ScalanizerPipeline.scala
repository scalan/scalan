package scalan.plugin

import scala.collection.mutable
import scalan.meta.scalanizer.Scalanizer
import scala.tools.nsc.Global
import scala.annotation.tailrec
import scala.reflect.internal.Phase
import scalan.meta.ScalanAstTransformers.isIgnoredExternalType
import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstExtensions._
import scalan.util.CollectionUtil._
import scalan.meta.{SourceModuleConf, ScalanCodegen}

abstract class ScalanizerPipeline[+G <: Global](val scalanizer: Scalanizer[G]) { pipeline =>
  import scalanizer._
  import scalanizer.global._

  def name: String

  def runAfter: List[String]

  def steps: List[PipelineStep]

  var isEnabled: Boolean = false // should be explicitly enabled based on passed options in ScalanPlugin.init()
  trait UnitContext {
    val unit: global.CompilationUnit
  }

  object UnitContext {
    def apply(u: global.CompilationUnit) = new UnitContext {val unit = u}
  }

  sealed trait PipelineStep {
    def name: String
  }

  case class ForEachUnitStep(name: String)(val action: UnitContext => Unit) extends PipelineStep

  case class RunStep(name: String)(val action: Unit => Unit) extends PipelineStep

  def forEachUnitComponent(runsAfter: List[String], step: ForEachUnitStep) =
    new ScalanizerComponent(step.name, runsAfter, global, pipeline) {
      def newPhase(prev: Phase) = new StdPhase(prev) {
        def apply(unit: global.CompilationUnit): Unit = {
          val context = UnitContext(unit.asInstanceOf[scalanizer.global.CompilationUnit])
          step.action(context)
        }
      }
    }

  def forRunComponent(runsAfter: List[String], step: RunStep) =
    new ScalanizerComponent(step.name, runsAfter, global, pipeline) {
      def newPhase(prev: Phase) = new StdPhase(prev) {
        override def run(): Unit = {
          step.action(())
        }

        def apply(unit: global.CompilationUnit): Unit = ()
      }
    }

  /** Checks that the method has @HotSpot */
  def isHotSpotTree(tree: Tree): Boolean = tree match {
    case method: DefDef =>
      val isHotSpot = method.symbol.annotations exists {annotation =>
        annotation.symbol.nameString == "HotSpot"
      }
      isHotSpot
    case _ => false
  }

  /** Applying of the policy: wrap all types outside of virtualization scope. */
  def isWrapperSym(sym: Symbol): Boolean = {
    !sym.hasPackageFlag && sym.isClass && isWrapper(sym.nameString)
  }

  def isWrapperType(tpe: Type): Boolean = {
    val res = isWrapperSym(tpe.typeSymbol)
    //    inform(s"isWrapperType(${show(tpe)}) == $res")
    res
  }

  def registerArrayOp(mkOp: STpeArg => SMethodDef) = {
    val tT = STpeArg("T")
    updateWrapperSpecial("scala", "Array", List(tT), Nil, false, mkOp(tT), Nil)
  }

  def catchSpecialWrapper(tree: Tree): Boolean = tree match {
    case IsArrayWrapperMethod(_, method) =>
      //      inform(s"catchSpecialWrapper(${show(q"$x.Predef.$ops($v).$method")})")
      method.decoded match {
        case "map" => registerArrayOp(mkArrayMapMethod)
        case "zip" => registerArrayOp(mkArrayZipMethod)
      }
      true
    //    case sel@q"$x.Predef.$y($z)" =>
    //      //      inform(s"catchSpecialWrapper(${show(q"$x.Predef.$y")})")
    //      false
    //    case sel@q"$x.Predef.$y[$t]($z)" =>
    //      //      inform(s"catchSpecialWrapper(${show(q"$x.Predef.$y")})")
    //      false
    case sel @ q"$x.Predef.$y" =>
      //      inform(s"catchSpecialWrapper(${show(q"$x.Predef.$y")})")
      false
    case q"$x.Array.canBuildFrom" => true // make sure it is ignored
    case _ => false
  }

  /** For each method call, create type wrapper if the external type should be wrapped. */
  def catchWrapperUsage(tree: Tree): Unit =
    if (!catchSpecialWrapper(tree)) {
      tree match {
        case sel @ Select(obj @ Apply(TypeApply(_, _), _), member) if isWrapperType(obj.tpe) =>
          //          inform(s"${show(sel)}: ${show(obj.tpe)}")
          updateWrapper(obj.tpe, member, sel.tpe, sel.symbol)
        case sel @ Select(obj @ Select(_, _), member) if isWrapperType(obj.tpe) =>
          //          inform(s"${show(sel)}: ${show(obj.tpe)}")
          updateWrapper(obj.tpe, member, sel.tpe, sel.symbol)
        case sel @ Select(obj, member) if isWrapperType(obj.tpe) =>
          //          inform(s"${show(sel)}: ${show(obj.tpe)}")
          updateWrapper(obj.tpe, member, sel.tpe, sel.symbol)
        case _ =>
        //          inform(s"UNCATCHED(${show(tree)})")
      }
    }

  /** Form the list of method arguments in terms of Meta AST by using symbols from Scala AST. */
  def formMethodArgs(args: List[Symbol]): List[SMethodArg] = {
    args.map {arg =>
      val tpe = parseType(arg.tpe)
      val isTypeDesc = tpe match {
        case STraitCall("Elem", _) => true
        case STraitCall("Cont", _) => true
        case _ => false
      }
      SMethodArg(
        impFlag = arg.isImplicit, overFlag = false,
        name = arg.nameString,
        tpe = tpe,
        default = None, annotations = Nil, isTypeDesc = isTypeDesc
      )
    }
  }

  def formMethodTypeArgs(targs: List[Symbol]): List[STpeArg] = {
    targs.map {targ =>
      STpeArg(
        name = targ.nameString,
        bound = None, contextBound = Nil, tparams = Nil
      )
    }
  }

  def formMethodRes(res: Type): STpeExpr = parseType(res)

  def formExternalMethodDef(name: String,
      tpeArgs: List[STpeArg],
      argSections: List[SMethodArgs],
      tpeRes: STpeExpr): SMethodDef = {
    val reifiedArgs = mutable.Set[STpeArg]()
    val argsWithoutClassTags = argSections.filterMap {section =>
      val filteredArgs = section.args.filterMap {arg =>
        arg.tpe match {
          case STraitCall("ClassTag", List(tT)) =>
            tpeArgs.find(t => t.name == tT.name) match {
              case Some(tpeArg) =>
                reifiedArgs += tpeArg
                None // filter the arg out of section
              case None =>
                // this ClassTag doesn't relate to type arguments so leave it in the section
                Some(arg)
            }
          case _ =>
            Some(arg)
        }
      }
      if (filteredArgs.isEmpty) None // filter out the section
      else Some(section.copy(args = filteredArgs))
    }
    val annotatedArgs = tpeArgs.map(a =>
      if (reifiedArgs.contains(a))
        a.copy(annotations = STypeArgAnnotation("Reified", Nil) :: a.annotations)
      else
        a)
    SMethodDef(
      name = name,
      tpeArgs = annotatedArgs,
      argSections = argsWithoutClassTags.joinArgSections(),
      tpeRes = Some(tpeRes),
      isImplicit = false, isOverride = false,
      overloadId = None,
      annotations = List(SMethodAnnotation(annotationClass = "External", args = Nil)),
      body = None,
      isTypeDesc = false
    )
  }

  /** Gets the list of ancestors of the external type in term of Meta AST. */
  def getExtTypeAncestors(externalType: Type): List[STypeApply] = {
    def convToMetaType(types: List[Type]): List[STraitCall] = {
      types map parseType collect {case t: STraitCall => t}
    }

    val ancestors = convToMetaType(getParents(externalType))
    ancestors.filterNot(a => isIgnoredExternalType(a.name)).map(_.toTypeApply)
  }

  /** Gets names of an external type, its class and its module. */
  def wrapperNames(externalName: String): (String, String, String) = {
    val className = wrap(externalName)
    (externalName, className, comp(className))
  }

  def mkCompanionAncestors(wClassName: String, kind: Int) =
    List(STraitCall("ExCompanion" + kind.toString, List(STraitCall(wClassName, Nil))).toTypeApply)

  /** Creates scalan-meta Module for an external type symbol. For example:
    * trait WCols extends Base with TypeWrappers { self: Wrappers =>
    * trait WCol[A] extends Def[WCol[A]] { self =>
    * def arr: Array[A]
    * };
    * trait WColCompanion extends ExCompanion1[WCol]
    * }
    * where
    * externalType is "class Col"
    * one of the members is "def arr: Array[A]"
    * */
  def createWrapper(externalType: Type): WrapperDescr = {
    val externalTypeSym = externalType.typeSymbol
    val isCompanion = externalTypeSym.isModuleClass
    val clazz = if (isCompanion) externalTypeSym.companionClass else externalTypeSym
    val tpeArgs = clazz.typeParams.map {param =>
      STpeArg(name = param.nameString, bound = None, contextBound = Nil, tparams = Nil)
    }
    val originalEntityAncestors = getExtTypeAncestors(externalType)
    val ownerChain = externalTypeSym.ownerChain.map(_.nameString)
    createWrapperSpecial(
      externalTypeSym.enclosingPackage.name,
      externalType.typeSymbol.nameString, tpeArgs,
      originalEntityAncestors,
      ownerChain)
  }

  /** Create wrapper module for a new type externalTypeName */
  def createWrapperSpecial(packageName: String, externalTypeName: String, tpeArgs: STpeArgs,
      originalEntityAncestors: List[STypeApply],
      ownerChain: List[String]): WrapperDescr = {
    val (externalName, wClassName, companionName) = wrapperNames(externalTypeName)
    val wrapperConf = snConfig.wrapperConfigs.getOrElse(externalTypeName, WrapperConfig.default(externalTypeName))
    val typeParams = tpeArgs.map {arg =>
      STraitCall(name = arg.name, args = Nil)
    }
    val baseType = STraitCall(externalName, typeParams)
    val entityAncestors = originalEntityAncestors
    val externalAnnot = externalTmplAnnotation(externalTypeName)
    val entityAnnotations = externalAnnot :: wrapperConf.annotations.map {a => STmplAnnotation(a, Nil)}
    val entity = STraitDef(
      name = wClassName,
      tpeArgs = tpeArgs,
      ancestors = entityAncestors,
      body = Nil,
      selfType = Some(SSelfTypeDef("self", Nil)),
      companion = Some(STraitDef(
        name = companionName,
        tpeArgs = Nil,
        ancestors = Nil, //mkCompanionAncestors(wClassName, kind = typeParams.length),
        body = Nil,
        selfType = None, companion = None
      )),
      annotations = entityAnnotations
    )
    val imports = List(
      SImportStat("scalan._"),
      SImportStat("impl._")
    )
    val module = SUnitDef(
      packageName = wrapPackage(packageName),
      imports = imports,
      name = wmod(externalTypeName),
      typeDefs = Nil,
      traits = List(entity),
      classes = Nil, methods = Nil,
      selfType = Some(SSelfTypeDef(
        name = "self",
        components = List(STraitCall("Wrappers", Nil))
      )),
      ancestors = Nil,
      origModuleTrait = None,
      isVirtualized = false
    )(context)
    WrapperDescr(module, ownerChain, wrapperConf)
  }

  /** Create/update Meta AST of the module for the external type. It assembles
    * Meta AST of a method (value) by its Scala's Type. */
  def updateWrapper(objType: Type,
      methodName: Name, methodReturnType: Type, methodSym: Symbol): Unit = {
    val externalTypeName = objType.typeSymbol.nameString
    val owner = methodSym.owner
    val pre = objType.typeSymbol.typeSignature
    val memberType = methodSym.tpe.asSeenFrom(pre, owner)
    val member = memberType match {
      case method @ (_: NullaryMethodType | _: MethodType) =>

        /** Not polymorphic methods like:
          * trait Col[A] {
          * def arr: Array[A]
          * def apply(i: Int): A
          * }
          */
        val (args, res) = uncurryMethodType(method)
        formExternalMethodDef(methodName.toString, Nil, args, res)
      case PolyType(typeArgs, method @ (_: NullaryMethodType | _: MethodType)) =>

        /** Methods that have type parameters like:
          * object Col {
          * def apply[T: ClassTag](arr: Array[T]): Col[T] = fromArray(arr)
          * def fromArray[T: ClassTag](arr: Array[T]): Col[T] = new ColOverArray(arr)
          * }
          */
        val tpeArgs = formMethodTypeArgs(typeArgs)
        val (args, res) = uncurryMethodType(method)
        formExternalMethodDef(methodName.toString, tpeArgs, args, res)
      case TypeRef(_, sym, _) =>

        /** Example: arr.length where
          * arr has type MyArr[Int] and
          * class MyArr[T] {
          * val length = 0
          * }
          */
        formExternalMethodDef(methodName.toString, Nil, Nil, formMethodRes(sym.tpe))
      case _ => throw new NotImplementedError(s"memberType = ${showRaw(memberType)}")
    }
    val wrapper = snState.getWrapper(externalTypeName).getOrElse {
      createWrapper(objType)
    }
    val updatedWrapper = addMember(objType.typeSymbol.isModuleClass, member, wrapper)
    snState.updateWrapper(externalTypeName, updatedWrapper)
    createMemberDependencies(memberType)
  }

  def updateWrapperSpecial(packageName: String,
      externalTypeName: String,
      tpeArgs: STpeArgs,
      originalEntityAncestors: List[STypeApply],
      isCompanion: Boolean,
      member: SMethodDef,
      ownerChain: List[String]): Unit = {
    val wrapper = snState.getWrapper(externalTypeName).getOrElse {
      createWrapperSpecial(packageName, externalTypeName, tpeArgs, originalEntityAncestors, ownerChain)
    }
    val updatedWrapper = addMember(isCompanion, member, wrapper)
    snState.updateWrapper(externalTypeName, updatedWrapper)
  }

  /** Adds a method or a value to the wrapper.
    * The member is added into the first entity in the list if it is not already there.
    * Other entities if any are ignored and left unchanged
    *
    * @param isCompanion specifies where to put the method (value) - into class or its companion. */
  def addMember(isCompanion: Boolean, member: SMethodDef, wrapper: WrapperDescr): WrapperDescr = {
    def isAlreadyAdded(e: STraitDef) = {
      if (isCompanion) {
        e.companion.fold(false)(_.body.contains(member))
      }
      else e.body.contains(member)
    }

    val module = wrapper.module
    val newModule = module.updateFirstEntity {e =>
      if (isAlreadyAdded(e)) e
      else {
        if (isCompanion) {
          val updatedCompanion = e.companion match {
            case Some(companion: STraitDef) =>
              val updatedBody = member :: companion.body
              Some(companion.copy(body = updatedBody))
            case _ =>
              throw new IllegalArgumentException(
                s"Cannot add member $member to companion because it is not found: module ${module.name}, entity: ${e.name}")
          }
          e.copy(companion = updatedCompanion)
        } else {
          val updatedBody = member :: e.body
          e.copy(body = updatedBody)
        }
      }
    }
    wrapper.copy(
      module = newModule
    )
  }

  /** Converts curried method type its uncurried Meta AST representation. */
  def uncurryMethodType(method: Type): (List[SMethodArgs], STpeExpr) = {
    def addArgSection(sections: List[SMethodArgs], args: List[Symbol]): List[SMethodArgs] = {
      val methodArgs = formMethodArgs(args)
      if (methodArgs.isEmpty) sections
      else sections :+ SMethodArgs(methodArgs)
    }

    @tailrec
    def loop(currMethod: Type, currArgs: List[SMethodArgs]): (List[SMethodArgs], STpeExpr) = {
      currMethod match {
        case NullaryMethodType(method) => loop(method, currArgs)
        case MethodType(args, method: MethodType) => loop(method, addArgSection(currArgs, args))
        case MethodType(args, resType) => (addArgSection(currArgs, args), parseType(resType))
        case tref @ (_: AbstractTypeRef | _: NoArgsTypeRef | _: ArgsTypeRef) =>
          (currArgs, parseType(tref))
      }
    }

    loop(method, Nil)
  }

  /** For the given type, find all dependencies and wrap them. */
  def createDependencies(objType: Type): Unit = {
    val parentDecls = objType.typeSymbol.typeSignature match {
      case PolyType(_, ClassInfoType(parents, _, _)) => parents
      case ClassInfoType(parents, _, _) => parents
      case _ => Nil
    }
    val externalTypes = snState.externalTypes
    parentDecls foreach {parent =>
      val name = parent.typeSymbol.nameString
      if (!isIgnoredExternalType(name) && !externalTypes.contains(name)) {
        val wrapperDescr = createWrapper(parent)
        snState.updateWrapper(name, wrapperDescr)
      }
    }
  }

  /** Traversing of the type and adding of wrappers for external types. */
  def createMemberDependencies(memberType: Type): Unit = {
    class DependencyTraverser extends TypeTraverser {
      def traverse(tp: Type): Unit = tp match {
        case TypeRef(pre, sym, args) if isWrapperSym(sym) =>
          val typeName = sym.nameString
          if (!snState.hasWrapper(typeName)) {
            val w = createWrapper(sym.tpe)
            snState.updateWrapper(typeName, w)
          }
        case _ => mapOver(tp)
      }
    }

    new DependencyTraverser().traverse(memberType)
  }

  case class WrapperSlices(abs: STraitDef)

  /** Calls Scalan Meta to generate boilerplate code for the wrapper. */
  def genWrapperBoilerplateText(mc: SourceModuleConf, unit: SUnitDef): String = {
    val gen = new scalan.meta.ModuleFileGenerator(
      ScalanCodegen, unit, mc.mkUnit(unit.unitName, unit.fileName))
    val implCode = gen.emitImplFile
    implCode
  }

  /** Generates Scala AST for the given wrapper (without implementation). */
  def genWrapperPackage(module: SUnitDef): Tree = {
    implicit val genCtx = GenCtx(context = module.context, toRep = true)
    val scalaAst = genModuleTrait(module)
    val imports = module.imports.map(genImport(_))
    val selfType = Some(SSelfTypeDef("self", List(STraitCall("Wrappers", Nil))))
    //    val extensions = genExtensions(module.name, selfType, Nil).map(
    //      extTrait => genTrait(extTrait)(GenCtx(module, false))
    //    )
    val pkgStats = imports :+ scalaAst
    val wrappersPackage = PackageDef(Ident(TermName(module.packageName)), pkgStats)
    wrappersPackage
  }

  def initWrapperSlices: WrapperSlices = {
    val wmodule = STraitDef("WrappersModule",
      tpeArgs = Nil,
      ancestors = List(),
      body = Nil, selfType = None, companion = None)
    WrapperSlices(wmodule)
  }

  def updateWrapperSlices(slices: WrapperSlices, module: SUnitDef): WrapperSlices = {
    val absAncestors = slices.abs.ancestors :+ STraitCall(module.name + "Module", Nil).toTypeApply
    WrapperSlices(
      abs = slices.abs.copy(ancestors = absAncestors)
    )
  }

  /** Replaces external types by their wrappers. For example:
    * trait Col[A] { def arr: Array[A]; }
    * The external type Array is replaced by its wrapper WArray
    * trait Col[A] { def arr: WArray[A]; }
    * */
  def replaceExternalTypeByWrapper(module: SUnitDef)(implicit ctx: AstContext): SUnitDef = {
    class TypeInWrappersTransformer(name: String) extends External2WrapperTypeTransformer(name) {
      override def classArgTransform(classArg: SClassArg) = classArg

      override def entityAncestorTransform(ancestor: STypeApply): STypeApply = {
        ancestor.copy(tpe = typeTransformer.traitCallTransform(ancestor.tpe))
      }
    }
    val wrappedModule = snState.externalTypes.foldLeft(module) {(acc, externalTypeName) =>
      new TypeInWrappersTransformer(externalTypeName).moduleTransform(acc)
    }
    wrappedModule
  }
}
