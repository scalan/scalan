package scalan.meta.scalanizer

import scala.tools.nsc.Global
import scalan.meta.ScalanAst._
import scalan.meta.{ScalanParsers, CodegenConfig}
import scalan.util.FileUtil

trait ScalanizerBase[G <: Global] extends ScalanParsers[G] {
  import global._

  def snState : ScalanizerState[G]
  def snConfig: ScalanizerConfig
  def config: CodegenConfig = snConfig.codegenConfig

  /** Gets module name by its entity. TODO: Should be a general solution. */
  def mod(name: String) = name + "s"
  /** Converts the name of external type to the name of its wrapper. */
  def wrap(name: String) = "W" + name
  /** Converts the name of external type to the name of the module which
    * contains a wrapper for the type. */
  def wmod(name: String) = "W" + mod(name)
  /** Gets name of companion by entity name */
  def comp(name: String) = name + "Companion"
  /** Gets name of the target package to put wrapper based on original package name */
  def wrapPackage(packageName: String) = packageName

  /** Classification of external types by their names. */
  def isPrimitive(name: String): Boolean = {
    STpePrimitives.keySet.contains(name)
  }
  def isStandardType(name: String): Boolean = {
    Set("Tuple", "Function").exists(name.startsWith(_)) ||
    Set("ClassTag").contains(name)
  }
  def isEntity(name: String): Boolean = {
    snConfig.concreteClassesOfEntity.keySet.contains(name)
  }
  def isEntityCompanion(name: String): Boolean = {
    snConfig.concreteClassesOfEntity.keys.map(comp(_)).toSet.contains(name)
  }
  def isClass(name: String): Boolean = {
    snConfig.concreteClassesOfEntity.values.flatten.toSet.contains(name)
  }
  def isClassCompanion(name: String): Boolean = {
    snConfig.concreteClassesOfEntity.values.flatten.map(comp(_)).toSet.contains(name)
  }
  def isModule(name: String): Boolean = {
    snConfig.concreteClassesOfEntity.keys.map(mod(_)).toSet.contains(name)
  }
  def isNonWrapper(name: String): Boolean = {
    snConfig.nonWrappers.keys.toSet.contains(name)
  }
  def isWrapper(name: String): Boolean = {
    val ok = !Set(
      isPrimitive _, isStandardType _,
      isEntity _, isEntityCompanion _,
      isClass _, isClassCompanion _,
      isModule _, isNonWrapper _
    ).exists(_(name))
    ok
  }

  def getParents(externalType: Type) = {
    externalType.typeSymbol.typeSignature match {
      case PolyType(_, ClassInfoType(parents, _, _)) => parents
      case ClassInfoType(parents, _, _) => parents
      case _ => Nil
    }
  }

  /** The class implements a default Meta AST transformation strategy: breadth-first search */
  class MetaAstTransformer {
    def constTransform(c: SConst): SConst = c
    def identTransform(ident: SIdent): SExpr = ident
    def selectTransform(select: SSelect): SExpr = {
      val newExpr = exprTransform(select.expr)
      select.copy(expr = newExpr)
    }
    def applyTransform(apply: SApply): SApply = {
      val newFun = exprTransform(apply.fun)
      val newArgss = apply.argss map (_.map(exprTransform))

      apply.copy(fun = newFun, argss = newArgss)
    }
    def exprApplyTransform(exprApply: SExprApply): SExprApply = {
      val newFun = exprTransform(exprApply.fun)
      exprApply.copy(fun = newFun)
    }
    def thisTransform(sthis: SThis): SThis = sthis
    def constrTransform(constr: SContr): SContr = {
      val newArgs = constr.args mapConserve exprTransform
      constr.copy(args = newArgs)
    }
    def ascrTransform(ascr: SAscr): SAscr = {
      val newExpr = exprTransform(ascr.expr)
      ascr.copy(expr = newExpr)
    }
    def funcTransform(func: SFunc): SFunc = {
      val newParams = func.params mapConserve valdefTransform
      val newRes = exprTransform(func.res)
      func.copy(params = newParams, res = newRes)
    }
    def blockTransform(block: SBlock): SBlock = {
      val newBlockInit = block.init mapConserve exprTransform
      val newBlocklast = exprTransform(block.last)

      block.copy(init = newBlockInit, last = newBlocklast)
    }

    def exprTransform(expr: SExpr): SExpr = expr match {
      case empty: SEmpty => empty
      case c: SConst => constTransform(c)
      case ident: SIdent => identTransform(ident)
      case apply: SApply => applyTransform(apply)
      case exprApply: SExprApply => exprApplyTransform(exprApply)
      case select: SSelect => selectTransform(select)
      case constr: SContr => constrTransform(constr)
      case sthis: SThis => thisTransform(sthis)
      case ascr: SAscr => ascrTransform(ascr)
      case func: SFunc => funcTransform(func)
      case block: SBlock => blockTransform(block)
      case bodyItem: SBodyItem => bodyItemTransform(bodyItem)
      case _ => throw new NotImplementedError(s"$expr")
    }

    def methodArgTransform(arg: SMethodArg): SMethodArg = arg
    def methodArgsTransform(args: SMethodArgs): SMethodArgs = {
      val newArgs = args.args mapConserve methodArgTransform

      args.copy(args = newArgs)
    }
    def methodArgSectionsTransform(argSections: List[SMethodArgs]): List[SMethodArgs] = {
      argSections mapConserve methodArgsTransform
    }
    def methodResTransform(res: Option[STpeExpr]): Option[STpeExpr] = res
    def methodBodyTransform(body: Option[SExpr]): Option[SExpr] = body match {
      case Some(bodyExpr) => Some(exprTransform(bodyExpr))
      case None => None
    }
    def methodTransform(method: SMethodDef): SMethodDef = {
      val newArgSections = methodArgSectionsTransform(method.argSections)
      val newTpeRes = methodResTransform(method.tpeRes)
      val newBody = methodBodyTransform(method.body)

      method.copy(
        argSections = newArgSections,
        tpeRes = newTpeRes,
        body = newBody
      )
    }
    def valdefTransform(valdef: SValDef): SValDef = {
      val newExpr = exprTransform(valdef.expr)
      valdef.copy(expr = newExpr)
    }

    def bodyItemTransform(bodyItem: SBodyItem): SBodyItem = bodyItem match {
      case method: SMethodDef => methodTransform(method)
      case valdef: SValDef => valdefTransform(valdef)
      case _ => throw new NotImplementedError(s"${bodyItem}")
    }
    def bodyTransform(body: List[SBodyItem]): List[SBodyItem] = body mapConserve bodyItemTransform

    def traitCompTransform(traitComp: STraitDef): STraitDef = {
      val newBody = bodyTransform(traitComp.body)
      traitComp.copy(body = newBody)
    }
    def classCompTransform(classComp: SClassDef): SClassDef = {
      val newBody = bodyTransform(classComp.body)
      classComp.copy(body = newBody)
    }
    def objCompTransform(obj: SObjectDef): SObjectDef = {
      val newBody = bodyTransform(obj.body)
      obj.copy(body = newBody)
    }

    def entityCompTransform(companion: Option[STraitOrClassDef]): Option[STraitOrClassDef] = {
      companion match {
        case Some(tr: STraitDef) => Some(traitCompTransform(tr))
        case Some(clazz: SClassDef) => Some(classCompTransform(clazz))
        case Some(obj: SObjectDef) => Some(objCompTransform(obj))
        case None => None
        case _ => throw new NotImplementedError(s"companion = $companion")
      }
    }

    def entityAncestorTransform(ancestor: STypeApply): STypeApply = ancestor
    def entityAncestorsTransform(ancestors: List[STypeApply]): List[STypeApply] = {
      ancestors mapConserve entityAncestorTransform
    }

    def entityTpeArgTransform(tpeArg: STpeArg): STpeArg = tpeArg
    def entityTpeArgsTransform(tpeArgs: List[STpeArg]): List[STpeArg] = {
      tpeArgs mapConserve entityTpeArgTransform
    }

    def entityTransform(entity: STraitDef): STraitDef = {
      val newTpeArgs = entityTpeArgsTransform(entity.tpeArgs)
      val newBody = bodyTransform(entity.body)
      val newCompanion = entityCompTransform(entity.companion)
      val newAncestors = entityAncestorsTransform(entity.ancestors)

      entity.copy(
        tpeArgs = newTpeArgs,
        body = newBody,
        companion = newCompanion,
        ancestors = newAncestors
      )
    }

    def classCompanionTransform(companion: Option[STraitOrClassDef]): Option[STraitOrClassDef] = {
      companion.map {
        case obj: SObjectDef => obj.copy(body = bodyTransform(obj.body))
        case tr: STraitDef => tr.copy(body = bodyTransform(tr.body))
        case unknown => throw new NotImplementedError(unknown.toString)
      }
    }
    def classArgTransform(classArg: SClassArg): SClassArg = classArg
    def classArgsTransform(classArgs: SClassArgs): SClassArgs = {
      val newArgs = classArgs.args mapConserve classArgTransform

      classArgs.copy(args = newArgs)
    }
    def classTransform(clazz: SClassDef): SClassDef = {
      val newBody = bodyTransform(clazz.body)
      val newCompanion = classCompanionTransform(clazz.companion)
      val newClassArgs = classArgsTransform(clazz.args)
      val newImplicitClassArgs = classArgsTransform(clazz.implicitArgs)

      clazz.copy(
        args = newClassArgs,
        implicitArgs = newImplicitClassArgs,
        body = newBody,
        companion = newCompanion
      )
    }

    def moduleTransform(module: SModuleDef): SModuleDef = {
      val newEntityOps = entityTransform(module.entityOps)
      val newEntities = module.entities mapConserve entityTransform
      val newClasses = module.concreteSClasses mapConserve classTransform

      module.copy(
        entityOps = newEntityOps,
        entities = newEntities,
        concreteSClasses = newClasses
      )
    }
  }

  /** Transform some Meta AST to another AST by applying the repl function to each type name. */
  class MetaAstReplacer(name: String, repl: String => String) extends MetaAstTransformer {
    val typeTransformer = new TypeReplacer(name, repl)

    override def methodArgTransform(arg: SMethodArg): SMethodArg = {
      arg.copy(tpe = typeTransformer.typeTransform(arg.tpe))
    }
    override def methodResTransform(res: Option[STpeExpr]): Option[STpeExpr] = res match {
      case Some(traitCall: STraitCall) => Some(typeTransformer.traitCallTransform(traitCall))
      case _ => super.methodResTransform(res)
    }
    override def classArgTransform(classArg: SClassArg): SClassArg = {
      classArg.copy(tpe = typeTransformer.typeTransform(classArg.tpe))
    }
    override def selectTransform(select: SSelect): SExpr = select match {
      case select: SSelect if select.tname == name =>
        SSelect(SEmpty(), repl(select.tname))
      case _ => super.selectTransform(select)
    }
    override def ascrTransform(ascr: SAscr): SAscr = {
      val newPt = typeTransformer.typeTransform(ascr.pt)
      super.ascrTransform(ascr.copy(pt = newPt))
    }
    override def applyTransform(apply: SApply): SApply = {
      val newTs = apply.ts mapConserve typeTransformer.typeTransform
      super.applyTransform(apply.copy(ts = newTs))
    }
    override def exprApplyTransform(exprApply: SExprApply): SExprApply = {
      val newTs = exprApply.ts mapConserve typeTransformer.typeTransform
      super.exprApplyTransform(exprApply.copy(ts = newTs))
    }
    override def valdefTransform(valdef: SValDef): SValDef = {
      val newTpe = valdef.tpe.map(typeTransformer.typeTransform _)
      super.valdefTransform(valdef.copy(tpe = newTpe))
    }
  }
  class ExtType2WrapperTransformer(name: String) extends MetaAstReplacer(name, wrap)

  /** Transforming of Meta AST related to types (children of STpeExpr)*/
  class MetaTypeTransformer {
    def typeTransform(tpe: STpeExpr): STpeExpr = tpe match {
      case empty: STpeEmpty => emptyTransform(empty)
      case traitCall: STraitCall => traitCallTransform(traitCall)
      case prim: STpePrimitive => primitiveTransform(prim)
      case existType: STpeExistential => existTypeTransform(existType)
      case _ => tpe
    }

    def emptyTransform(emptyType: STpeEmpty) = emptyType
    def primitiveTransform(prim: STpePrimitive) = prim

    def traitCallArgsTransform(args: List[STpeExpr]): List[STpeExpr] = args mapConserve(typeTransform)
    def traitCallNameTransform(name: String): String = name
    def traitCallTransform(traitCall: STraitCall): STraitCall = {
      val newName = traitCallNameTransform(traitCall.name)
      val newArgs = traitCallArgsTransform(traitCall.tpeSExprs)

      traitCall.copy(name = newName, tpeSExprs = newArgs)
    }
    def tpeDefArgTransform(tpeArg: STpeArg): STpeArg = tpeArg
    def tpeDefArgsTransform(tpeArgs: STpeArgs): STpeArgs = {
      tpeArgs mapConserve tpeDefArgTransform
    }
    def tpeDefTransform(tpeDef: STpeDef): STpeDef = {
      val newTpeArgs = tpeDefArgsTransform(tpeDef.tpeArgs)

      tpeDef.copy(tpeArgs = newTpeArgs)
    }
    def existItemTransform(item: SBodyItem): SBodyItem = item match {
      case tpeDef: STpeDef => tpeDefTransform(tpeDef)
      case _ => item
    }
    def existItemsTransform(items: List[SBodyItem]): List[SBodyItem] = {
      items mapConserve existItemTransform
    }
    def existTypeTransform(existType: STpeExistential): STpeExistential = {
      val newTpt = typeTransform(existType.tpt)
      val newItems = existItemsTransform(existType.items)

      existType.copy(tpt = newTpt, items = newItems)
    }
  }

  /** Renaming of all types with the given name by applying the repl function. */
  class TypeReplacer(name: String, repl: String => String) extends MetaTypeTransformer {
    override def traitCallNameTransform(tname: String): String = {
      if (tname == name) repl(tname)
      else tname
    }
  }

  class ExtType2WrapperTypeTransformer(name: String) extends TypeReplacer(name, wrap)
  /** Renaming of types with oldName to new name (newName). */
  class TypeRenamer(oldName: String, newName: String) extends MetaTypeTransformer {
    override def traitCallNameTransform(tname: String): String = {
      if (tname == oldName) newName
      else tname
    }
    override def tpeDefArgTransform(tpeArg: STpeArg): STpeArg = {
      if (tpeArg.name == oldName) tpeArg.copy(name = newName)
      else tpeArg
    }
  }
  /** Traverse whole META AST and rename types. Returns new tree. */
  class TypeNameTransformer(oldName: String, newName: String) extends MetaAstTransformer {
    val typeRenamer = new TypeRenamer(oldName, newName)
    override def entityTpeArgTransform(tpeArg: STpeArg): STpeArg = {
      if (tpeArg.name == oldName) tpeArg.copy(name = newName)
      else tpeArg
    }
    override def methodResTransform(res: Option[STpeExpr]): Option[STpeExpr] = res match {
      case Some(resType) => Some(typeRenamer.typeTransform(resType))
      case _ => res
    }
    override def methodArgTransform(arg: SMethodArg): SMethodArg = {
      val newTpe = typeRenamer.typeTransform(arg.tpe)
      arg.copy(tpe = newTpe)
    }
    override def entityAncestorTransform(ancestor: STypeApply): STypeApply = {
      val newTpe = typeRenamer.traitCallTransform(ancestor.tpe)
      val newTs = ancestor.ts.mapConserve(exprTransform)
      ancestor.copy(tpe = newTpe, ts = newTs)
    }
  }

  /** Removing of internal parts of annotations that should be ignored at code generation. */
  def filterInternalAnnot(annotations: List[SAnnotation]): List[SAnnotation] = {
    annotations map {
      case annotation @ SMethodAnnotation("Constructor", args) =>
        val newArgs = args filter {
          case SAssign(SIdent("original",_), m: SMethodDef,_) => false
          case _ => true
        }
        annotation.copy(args = newArgs)
      case other => other
    }
  }

  def getExtentionNames(moduleName: String): Set[String] = {
    Set("Dsl", "DslStd", "DslExp").map(moduleName + _)
  }
  /** The external types that should be rejected during virtualization. */
  def isIgnoredExternalType(typeName: String) = Set("Object", "Any", "AnyRef").contains(typeName)

  def getWrappersHome = snConfig.home + "/src/main/scala"

  def saveWrapperCode(packageName: String, fileName: String, wrapperCode: String) = {
    val packagePath = packageName.split('.').mkString("/")
    val wrapperFile = FileUtil.file(getWrappersHome, packagePath, fileName + ".scala")
    wrapperFile.mkdirs()
    FileUtil.write(wrapperFile, wrapperCode)
  }
}
