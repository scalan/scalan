package scalan.meta.scalanizer

import java.io.File

import scala.tools.nsc.Global
import scalan.meta.ScalanAst._
import scalan.util.FileUtil

trait Enricher[G <: Global] extends ScalanizerBase[G] {
  /** Module parent is replaced by the parent with its extension. */
  def composeParentWithExt(module: SModuleDef) = {
    val parentsWithExts = module.ancestors.map{ancestor =>
      ancestor.copy(name = ancestor.name + "Dsl")
    }

    module.copy(ancestors = parentsWithExts)
  }

  /** Gets all packages needed for the module and imports them. */
  def getImportByName(name: String): SImportStat = {
    val pkgOfModule = snState.packageOfModule.get(name) match {
      case Some(pkgName) => pkgName + "."
      case _ => ""
    }
    SImportStat(pkgOfModule + "implOf"+name+".StagedEvaluation._")
  }

  /** Imports scalan._ and other packages needed by Scalan and further transformations. */
  def addImports(module: SModuleDef) = {
    val usedModules = snState.dependenceOfModule.getOrElse(module.name, List())
    val usedImports = usedModules.map(getImportByName)
    val usersImport = module.imports.collect{
      case imp @ SImportStat("scalan.compilation.KernelTypes._") => imp
    }

    module.copy(imports = SImportStat("scalan._") :: (usedImports ++ usersImport))
  }

  /** Introduces a synonym for the entity. If name of the entity is Matr, the method adds:
    *   type RepMatr[T] = Rep[Matr[T]]
    * */
  def repSynonym(module: SModuleDef) = {
    val entity = module.entityOps
    module.copy(entityRepSynonym = Some(STpeDef(
      name = "Rep" + entity.name,
      tpeArgs = entity.tpeArgs,
      rhs = STraitCall("Rep", List(STraitCall(entity.name, entity.tpeArgs.map(_.toTraitCall))))
    )))
  }

  /** Extends the module by Base from Scalan */
  def addModuleAncestors(module: SModuleDef) = {
    val newAncestors = STraitCall(name = "Base", tpeSExprs = List()) :: module.ancestors

    module.copy(ancestors = newAncestors)
  }

  /** Extends the entiry T by Def[T] */
  def addEntityAncestors(module: SModuleDef) = {
    val newAncestors = STraitCall(
      name = "Def",
      tpeSExprs = List(STraitCall(module.entityOps.name,
                                  module.entityOps.tpeArgs.map(arg => STraitCall(arg.name, List()))))
    ) :: module.entityOps.ancestors
    val newEntity = module.entityOps.copy(ancestors = newAncestors)

    module.copy(entityOps = newEntity, entities = List(newEntity))
  }

  /** Appends the given suffix to the type of self. For example:
    *   trait Matrs {self: LinearAlgebra => ... }
    * and for the suffix "Dsl", the method extends LinearAlgebra to LinearAlgebraDsl:
    *   trait Matrs {self: LinearAlgebraDsl => ... }
    * If the module doesn't have self than self is added with the type of module plus the suffix:
    *   trait Matrs {...} -> trait Matrs {self: MatrsDsl => ...}
    * */
  def selfComponentsWithSuffix(moduleName: String,
                               selfType: Option[SSelfTypeDef],
                               suffix: String): List[STpeExpr] = {
    selfType match {
      case Some(selfTypeDef) => selfTypeDef.components.map{(c: STpeExpr) => c match {
        case tr: STraitCall => tr.copy(name = c.name + suffix)
        case _ => c
      }}
      case _ => List(STraitCall(moduleName + suffix, List()))
    }
  }
  def selfModuleComponents(module: SModuleDef, suffix: String): List[STpeExpr] = {
    selfComponentsWithSuffix(module.name, module.selfType, suffix)
  }

  /** Puts the module to the cake. For example, trait Segments is transformed to
    * trait Segments {self: SegmentsDsl => ... } */
  def updateSelf(module: SModuleDef) = {
    module.copy(selfType = Some(SSelfTypeDef(
      name = "self",
      components = selfModuleComponents(module, "Module")
    )))
  }

  /** Creates empty companion trait (no body) for an entity or concrete classes. */
  def createCompanion(baseName: String) = STraitDef(
    name = baseName + "Companion",
    tpeArgs = List(),
    ancestors = List(),
    body = List(),
    selfType = None,
    companion = None
  )

  /** Converts and fixes the type of companion. SObjectDef -> STraitDef. */
  def convertCompanion(comp: STraitOrClassDef): STraitOrClassDef = comp match {
    case obj: SObjectDef =>
      STraitDef(name = obj.name + "Companion",
        tpeArgs = obj.tpeArgs, ancestors = obj.ancestors, body = obj.body, selfType = obj.selfType,
        companion = obj.companion, annotations = obj.annotations)
    case _ => comp
  }

  /** Checks that the entity has a companion. If the entity doesn't have it
    * then the method adds the companion. */
  def checkEntityCompanion(module: SModuleDef) = {
    val entity = module.entityOps
    val newCompanion = entity.companion match {
      case Some(comp) => Some(convertCompanion(comp))
      case None => Some(createCompanion(entity.name))
    }
    val newEntity = entity.copy(companion = newCompanion)

    module.copy(entityOps = newEntity, entities = List(newEntity))
  }

  /** Checks that concrete classes have their companions and adds them. */
  def checkClassCompanion(module: SModuleDef) = {
    val newClasses = module.concreteSClasses.map{ clazz =>
      val newCompanion = clazz.companion match {
        case Some(comp) => Some(convertCompanion(comp))
        case None => Some(createCompanion(clazz.name))
      }

      clazz.copy(companion = newCompanion)
    }

    module.copy(concreteSClasses = newClasses)
  }

  def saveDebugCode(fileName: String, code: String) = {
    val folder = new File(snConfig.home)
    val file = FileUtil.file(folder, "debug", fileName)
    file.mkdirs()

    FileUtil.write(file, code)
  }

  def saveImplCode(file: File, implCode: String) = {
    val fileName = file.getName.split('.')(0)
    val folder = file.getParentFile
    val implFile = FileUtil.file(folder, "impl", s"${fileName}Impl.scala")

    implFile.mkdirs()

    FileUtil.write(implFile, implCode)
  }

  def eraseModule(module: SModuleDef) = module

  def firstKindArgs(tpeArgs: List[STpeArg]): List[STpeArg] = {
    tpeArgs.filter(_.tparams.isEmpty)
  }

  def highKindArgs(tpeArgs: List[STpeArg]): List[STpeArg] = {
    tpeArgs.filter(!_.tparams.isEmpty)
  }

  def genElemsByTypeArgs(tpeArgs: List[STpeArg]): List[SMethodDef] = {
    def genImplicit(tpeArg: STpeArg, methodPrefix: String, descTypeName: String) =
      SMethodDef(name = methodPrefix + tpeArg.name,
        tpeArgs = Nil, argSections = Nil,
        tpeRes = Some(STraitCall(descTypeName, List(STraitCall(tpeArg.name, Nil)))),
        isImplicit = true, isOverride = false,
        overloadId = None, annotations = Nil,
        body = None, isTypeDesc = true)

    def genElem(tpeArg: STpeArg) = genImplicit(tpeArg, "e", "Elem")
    def genCont(tpeArg: STpeArg) = genImplicit(tpeArg, "c", "Cont")

    tpeArgs.map{ targ =>
      if (targ.tparams.isEmpty) genElem(targ)
      else if (targ.tparams.size == 1) genCont(targ)
      else !!!(s"Cannot create descriptor method for a high-kind tpeArg with with more than one type arguments: $targ")
    }
  }

  def genEntityImpicits(module: SModuleDef) = {

    val bodyWithImpElems = genElemsByTypeArgs(module.entityOps.tpeArgs) ++ module.entityOps.body
    val newEntity = module.entityOps.copy(body = bodyWithImpElems)

    module.copy(entityOps = newEntity, entities = List(newEntity))
  }

  def genImplicitClassArgs(module: SModuleDef, clazz: SClassDef): List[SClassArg] = {
    def genImplicit(argPrefix: String, argSuffix: String,
                    typePrefix: String, typeSuffix: STpeExpr) = {
      SClassArg(impFlag = true,
        overFlag = false, valFlag = false,
        name = argPrefix + argSuffix,
        tpe = STraitCall(typePrefix, List(typeSuffix)),
        default = None, annotations = Nil, isTypeDesc = true)
    }
    def genElem(valName: String, typeName: STpeExpr) =
      genImplicit("e", valName, "Elem", typeName)
    def genCont(valName: String, typeName: STpeExpr) =
      genImplicit("c", valName, "Cont", typeName)
    def genImplicitArg(isFirstKind: Boolean, valName: String, typeName: STpeExpr): SClassArg = {
      if (isFirstKind) genElem(valName, typeName)
      else genCont(valName, typeName)
    }
    def getEntityByAncestor(ancestor: STraitCall): Option[STraitDef] = {
      module.entities.find(entity => entity.name == ancestor.name)
    }
    lazy val ancestorPairs: List[(STpeExpr, STpeArg)] = {
      val ancestors: List[STraitCall] = clazz.ancestors

      ancestors.flatMap { (ancestor: STraitCall) =>
        val optEntity: Option[STraitDef] = getEntityByAncestor(ancestor)

        optEntity match {
          case Some(entity) => ancestor.tpeSExprs zip entity.tpeArgs
          case None => List[(STpeExpr, STpeArg)]()
        }
      }
    }
    def tpeArg2Expr(tpeArg: STpeArg): STpeExpr = STraitCall(tpeArg.name, Nil)
    val classImplicits = clazz.tpeArgs.map { tpeArg =>
      ancestorPairs.find(pair => tpeArg2Expr(tpeArg) == pair._1) match {
        case Some((aParam, eParam)) => aParam match {
          case _: STraitCall => genImplicitArg(eParam.tparams.isEmpty, "e"+eParam.name, aParam)
          case _ => throw new NotImplementedError(s"genImplicitClassArgs: $eParam")
        }
        case None => genImplicitArg(tpeArg.tparams.isEmpty, "c"+tpeArg.name, tpeArg2Expr(tpeArg))
    }}
    val entityImplicits = ancestorPairs.map{pair =>
      val (ancestorParam, entityParam) = pair
      genImplicitArg(entityParam.tparams.isEmpty, "e"+entityParam.name, ancestorParam)
    }

    (entityImplicits ++ classImplicits).distinct
  }

  def genClassesImplicits(module: SModuleDef) = {
    def unpackElem(classArg: SClassArg): Option[STpeExpr] = classArg.tpe match {
      case STraitCall("Elem", List(prim @ STpePrimitive(_,_))) => Some(prim)
      case _ => None
    }
    /** The function checks that the Elem is already defined somewhere in scope. */
    def isElemAlreadyDefined(classArg: SClassArg): Boolean = unpackElem(classArg) match {
      case Some(_) => true
      case None => false
    }
    def convertElemValToMethod(classArg: SClassArg): SMethodDef = {
      SMethodDef(name = classArg.name, tpeArgs = Nil, argSections = Nil,
        tpeRes = Some(classArg.tpe),
        isImplicit = false, isOverride = false, overloadId = None, annotations = Nil,
        body = Some(STypeApply(SIdent("element"), unpackElem(classArg).toList)),
        isTypeDesc = true)
    }
    val newClasses = module.concreteSClasses.map{clazz =>
      val (definedElems, elemArgs) = genImplicitClassArgs(module, clazz) partition isElemAlreadyDefined
      val newImplicitArgs = SClassArgs(clazz.implicitArgs.args ++ elemArgs)
      val newBody = definedElems.map(convertElemValToMethod) ++ clazz.body

      clazz.copy(implicitArgs = newImplicitArgs, body = newBody)
    }

    module.copy(concreteSClasses = newClasses)
  }

  def genImplicitMethodArgs(method: SMethodDef): SMethodDef = {
    def genImplicit(tpeArg: STpeArg, valPrefix: String, resPrefix: String) = {
      SMethodArg(impFlag = true, overFlag = false,
        name = valPrefix + tpeArg.name,
        tpe = STraitCall(resPrefix, List(STraitCall(tpeArg.name, Nil))),
        default = None, annotations = Nil, isTypeDesc = true)
    }
    def genElem(tpeArg: STpeArg) = genImplicit(tpeArg, "em", "Elem")
    def genCont(tpeArg: STpeArg) = genImplicit(tpeArg, "cm", "Cont")
    def genImplicitVals(tpeArgs: List[STpeArg]): List[SMethodArg] = {
      tpeArgs.map{tpeArg =>
        if (tpeArg.tparams.isEmpty) genElem(tpeArg)
        else genCont(tpeArg)
      }
    }
    val args = genImplicitVals(method.tpeArgs) match {
      case Nil => method.argSections
      case as => method.argSections ++ List(SMethodArgs(as))
    }

    method.copy(argSections = joinImplicitArgs(args))
  }

  def genMethodsImplicits(module: SModuleDef) = {
    def genBodyItem(item: SBodyItem): SBodyItem = item match {
      case m: SMethodDef => genImplicitMethodArgs(m)
      case _ => item
    }
    def genCompanion(companion: Option[STraitOrClassDef]) = companion match {
      case Some(t : STraitDef) => Some(t.copy(body = t.body.map(genBodyItem)))
      case Some(c : SClassDef) => Some(c.copy(body = c.body.map(genBodyItem)))
      case Some(unsupported) => throw new NotImplementedError(s"genCompanion: $unsupported")
      case None => None
    }
    def genEntity(entity: STraitDef): STraitDef = {
      val newBodyItems = entity.body.map(genBodyItem)
      entity.copy(body = newBodyItems, companion = genCompanion(entity.companion))
    }
    def genEntities(entities: List[STraitDef]): List[STraitDef] = {
      entities.map(genEntity)
    }
    def genClass(clazz: SClassDef): SClassDef = {
      val newBodyItems = clazz.body.map(genBodyItem)
      clazz.copy(body = newBodyItems, companion = genCompanion(clazz.companion))
    }
    def genClasses(classes: List[SClassDef]): List[SClassDef] = {
      classes.map(genClass)
    }

    module.copy(entityOps = genEntity(module.entityOps),
      entities = genEntities(module.entities),
      concreteSClasses = genClasses(module.concreteSClasses)
    )
  }

  def genExtensions(moduleName: String,
                    selfModuleType: Option[SSelfTypeDef],
                    moduleAncestors: List[STraitCall]
                    ): List[STraitDef] = {
    val boilerplateSuffix = Map("Dsl" -> "Abs", "DslStd" -> "Std", "DslExp" -> "Exp")
    val extensions = snState.subcakesOfModule(moduleName)
                     .filterNot(ext => ext.endsWith("Std") && !snConfig.codegenConfig.isStdEnabled)

    (extensions map {extName =>
      val extSuffix = extName.stripPrefix(moduleName)
      val selfType: SSelfTypeDef = SSelfTypeDef(
        name = "self",
        components = selfComponentsWithSuffix(moduleName, selfModuleType, extSuffix)
      )
      val boilerplate = STraitCall(moduleName + boilerplateSuffix(extSuffix), Nil)
      val ancestors: List[STraitCall] = moduleAncestors map {
        ancestor => ancestor.copy(name = ancestor.name + extSuffix)
      }

      STraitDef(
        name = extName,
        tpeArgs = Nil,
        ancestors = boilerplate :: ancestors,
        body = Nil,
        selfType = Some(selfType),
        companion = None,
        annotations = Nil
      )
    }).toList
  }

  def genModuleExtensions(module: SModuleDef): List[STraitDef] = {
    genExtensions(module.name, module.selfType, module.ancestors)
  }

  /** ClassTags are removed because they can be extracted from Elems. */
  def cleanUpClassTags(module: SModuleDef) = {
    class ClassTagTransformer extends MetaAstTransformer {
      override def methodArgsTransform(args: SMethodArgs): SMethodArgs = {
        val newArgs = args.args.filter {marg => marg.tpe match {
          case tc: STraitCall if tc.name == "ClassTag" => false
          case _ => true
        }} mapConserve methodArgTransform

        args.copy(args = newArgs)
      }
      override def methodArgSectionsTransform(argSections: List[SMethodArgs]): List[SMethodArgs] = {
        argSections mapConserve methodArgsTransform filter {
          case SMethodArgs(List()) | SMethodArgs(Nil) => false
          case _ => true
        }
      }
      override def bodyTransform(body: List[SBodyItem]): List[SBodyItem] = {
        body filter{
          case SMethodDef(_,_,_,Some(STraitCall("ClassTag", _)),true,_,_,_,_,_) => false
          case _ => true
        } mapConserve bodyItemTransform
      }
      override def classArgsTransform(classArgs: SClassArgs): SClassArgs = {
        val newArgs = classArgs.args.filter { _.tpe match {
          case STraitCall("ClassTag", _) => false
          case _ => true
        }} mapConserve classArgTransform

        classArgs.copy(args = newArgs)
      }
    }

    new ClassTagTransformer().moduleTransform(module)
  }

  /** According to scala docs, a method or constructor can have only one implicit parameter list,
    * and it must be the last parameter list given. */
  def joinImplicitArgs(argSections: List[SMethodArgs]): List[SMethodArgs] = {
    val cleanArgs = argSections.map(_.args)
    val (imp, nonImp) = cleanArgs.partition {
      case (m: SMethodArg) :: _ => m.impFlag
      case _ => false
    }
    val newArgs = imp.flatten match {
      case Nil => nonImp
      case as => nonImp ++ List(as)
    }

    newArgs.map(args => SMethodArgs(args))
  }

  def externalTypeToWrapper(module: SModuleDef) = {
    val wrappedModule = snState.externalTypes.foldLeft(module){(acc, externalTypeName) =>
      new ExtType2WrapperTransformer(externalTypeName).moduleTransform(acc)
    }

    wrappedModule
  }

  def fixEntityCompanionName(module: SModuleDef) = {
    class ECompanionTransformer extends MetaAstTransformer {
      override def applyTransform(apply: SApply): SApply = {
        apply match {
          case SApply(sel @ SSelect(SThis(clazz,_),_,_),_,_,_) if isEntity(clazz) =>
            apply.copy(fun = sel.copy(expr = SThis(clazz + "Companion")))
          case _ => super.applyTransform(apply)
        }
      }
    }
    new ECompanionTransformer().moduleTransform(module)
  }

  def replaceClassTagByElem(module: SModuleDef) = {
    new MetaAstReplacer("ClassTag", (_:String) => "Elem") {
      override def selectTransform(select: SSelect): SExpr = {
        val type2Elem = Map(
          "AnyRef" -> "AnyRefElement",
          "Boolean" -> "BoolElement",
          "Byte" -> "ByteElement",
          "Short" -> "ShortElement",
          "Int" -> "IntElement",
          "Long" -> "LongElement",
          "Float" -> "FloatElement",
          "Double" -> "DoubleElement",
          "Unit" -> "UnitElement",
          "String" -> "StringElement",
          "Char" -> "CharElement"
        )
        select match {
          case SSelect(SIdent("ClassTag",_), t,_) if type2Elem.keySet.contains(t) =>
            SSelect(SIdent("self"), type2Elem(t))
          case _ => super.selectTransform(select)
        }
      }
    }.moduleTransform(module)
  }

  def eliminateClassTagApply(module: SModuleDef) = {
    new MetaAstTransformer {
      override def applyTransform(apply: SApply): SApply = apply match {
        case SApply(SSelect(SIdent("ClassTag",_), "apply",_), List(tpe), _,_) =>
          apply.copy(
            fun = SIdent("element"),
            argss = Nil
          )
        case _ => super.applyTransform(apply)
      }
    }.moduleTransform(module)
  }

  def fixEvidences(module: SModuleDef) = {
    new MetaAstTransformer {
      def implicitElem(tpeSExprs: List[STpeExpr]) = {
        STypeApply(
          SSelect(
            SIdent("Predef"),
            "implicitly"
          ),
          tpeSExprs map (tpe => STraitCall("Elem", List(tpe)))
        )
      }

      override def identTransform(ident: SIdent): SExpr = ident match {
        case SIdent(name, Some(STraitCall("ClassTag", targs))) if name.startsWith("evidence$") =>
          super.typeApplyTransform(implicitElem(targs))
        case _ => super.identTransform(ident)
      }
      override def selectTransform(select: SSelect): SExpr = select match {
        case SSelect(_, name, Some(STraitCall("ClassTag", targs))) if name.startsWith("evidence$") =>
          super.typeApplyTransform(implicitElem(targs))
        case _ => super.selectTransform(select)
      }
    }.moduleTransform(module)
  }

  def fixExistentialType(module: SModuleDef) = {
    new MetaAstTransformer {
      def containsExistential(tpe: STpeExpr): Boolean = {
        var hasExistential = false
        new MetaTypeTransformer {
          override def existTypeTransform(existType: STpeExistential): STpeExistential = {
            hasExistential = true
            super.existTypeTransform(existType)
          }
        }.typeTransform(tpe)

        hasExistential
      }

      override def applyTransform(apply: SApply): SApply = {
        val hasExistential = apply.argss exists (_.exists(arg =>
          containsExistential(arg.exprType.getOrElse(STpeEmpty()))
        ))
        def castToUniversal(targs: List[STpeExpr]) = {
          val newArgss = apply.argss map(_.map(arg =>
            SApply(SSelect(arg, "asRep"),targs, Nil)
          ))
          apply.copy(argss = newArgss)
        }

        if (hasExistential) {
          apply.fun.exprType match {
            case Some(methodType: STpeMethod) => castToUniversal(methodType.params)
            case _ => super.applyTransform(apply)
          }
        } else super.applyTransform(apply)
      }
    }.moduleTransform(module)
  }
}
