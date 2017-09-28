package scalan.meta.scalanizer

import java.io.File

import scala.tools.nsc.Global
import scalan.meta.ScalanAst
import scalan.meta.ScalanAst._
import scalan.util.FileUtil
import scalan.util.CollectionUtil._

trait Enricher[G <: Global] extends ScalanizerBase[G] {

  /** Gets all packages needed for the module and imports them. */
  def getImportByName(name: String): SImportStat = {
    val pkgOfModule = snState.packageOfModule.get(name) match {
      case Some(pkgName) => pkgName + "."
      case _ => ""
    }
    SImportStat(pkgOfModule + "implOf"+name+".StagedEvaluation._")
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


  def genClassArg(argPrefix: String, argName: String, descName: String, descArg: STpeExpr) = {
    SClassArg(impFlag = true,
      overFlag = false, valFlag = false,
      name = argPrefix + argName,
      tpe = STraitCall(descName, List(descArg)),
      default = None, annotations = Nil, isTypeDesc = true)
  }
  def genElemClassArg(argName: String, tpe: STpeExpr) = genClassArg("e", argName, "Elem", tpe)
  def genContClassArg(argName: String, tpe: STpeExpr) = genClassArg("c", argName, "Cont", tpe)
  def genImplicitClassArg(isHighKind: Boolean, argName: String, tpe: STpeExpr): SClassArg = {
    if (!isHighKind) genElemClassArg(argName, tpe)
    else genContClassArg(argName, tpe)
  }

  def genImplicitClassArgs(module: SModuleDef, clazz: SClassDef): List[SClassArg] = {
    val ancestorSubst: List[(STpeArg, STpeExpr)] = {
      clazz.ancestors.flatMap { a =>
        val ancestorEnt_? = module.findEntity(a.tpe.name, globalSearch = true)
        ancestorEnt_? match {
          case Some(e) =>
            val pairs = e.tpeArgs zip a.tpe.tpeSExprs
            pairs//.filterNot { case (a, t) => e.isExtractableArg(module, a) }
          case None => List[(STpeArg, STpeExpr)]()
        }
      }
    }
    val classImplicits = clazz.tpeArgs/*.filterNot(a => clazz.isExtractableArg(module, a))*/.map { clsArg =>
      val argTpe = clsArg.toTraitCall
      val substOpt = ancestorSubst.find { case (tyArg, tpe) => tpe == argTpe }
      val res = substOpt match {
        case Some((tyArg, tpe)) => tpe match {
          case _: STraitCall => genImplicitClassArg(tyArg.tparams.nonEmpty, tyArg.name, tpe) // NOTE: tpe == argTpe
          case _ => throw new NotImplementedError(s"genImplicitClassArgs: $clsArg")
        }
        case None => genImplicitClassArg(clsArg.tparams.nonEmpty, clsArg.name, clsArg.toTraitCall)
      }
      res
    }
//    val entityImplicits = ancestorSubst.map { case (tyArg, ancestorParam) =>
//      genImplicitClassArg(tyArg.tparams.nonEmpty, tyArg.name, ancestorParam)
//    }

    (classImplicits).distinct
  }


  def genImplicitMethodArgs(module: SModuleDef, method: SMethodDef): SMethodDef = {
    def genImplicit(tpeArg: STpeArg, valPrefix: String, descTypeName: String) = {
      SMethodArg(impFlag = true, overFlag = false,
        name = valPrefix + tpeArg.name,
        tpe = STraitCall(descTypeName, List(STraitCall(tpeArg.name, Nil))),
        default = None, annotations = Nil, isTypeDesc = true)
    }
    def genElem(tpeArg: STpeArg) = genImplicit(tpeArg, "em", "Elem")
    def genCont(tpeArg: STpeArg) = genImplicit(tpeArg, "cm", "Cont")
    def genImplicitVals(tpeArgs: List[STpeArg]): List[SMethodArg] = {
      tpeArgs.map { arg =>
        if (arg.tparams.isEmpty) genElem(arg)
        else genCont(arg)
      }
    }

    val args = genImplicitVals(method.tpeArgs/*.filterNot(a => method.isExtractableArg(module, a))*/) match {
      case Nil => method.argSections
      case as => method.argSections ++ List(SMethodArgs(as))
    }

    method.copy(argSections = joinImplicitArgs(args))
  }

  def genExtensions(moduleName: String,
                    selfModuleType: Option[SSelfTypeDef],
                    moduleAncestors: List[STypeApply]
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
      val boilerplate = STraitCall(moduleName + boilerplateSuffix(extSuffix), Nil).toTypeApply
      val ancestors = moduleAncestors map {
        a => a.copy(tpe = a.tpe.copy(name = a.tpe.name + extSuffix))
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


  object ModuleVirtualizationPipeline extends (SModuleDef => SModuleDef) {
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

    def externalTypeToWrapper(module: SModuleDef) = {
      val wrappedModule = snState.externalTypes.foldLeft(module){(acc, externalTypeName) =>
        new External2WrapperTypeTransformer(externalTypeName).moduleTransform(acc)
      }
      wrappedModule
    }

    /** Module parent is replaced by the parent with its extension. */
    def composeParentWithExt(module: SModuleDef) = {
      val parentsWithExts = module.ancestors.map { a =>
        a.copy(tpe = a.tpe.copy(name = a.tpe.name + "Dsl"))
      }
      module.copy(ancestors = parentsWithExts)
    }

    /** Extends the module by Base from Scalan */
    def addModuleAncestors(module: SModuleDef) = {
      val newAncestors = STraitCall(name = "Base", tpeSExprs = List()).toTypeApply :: module.ancestors
      module.copy(ancestors = newAncestors)
    }

    /** Extends the entiry T by Def[T] */
    def addEntityAncestors(module: SModuleDef) = {
      val newAncestors = STraitCall(
        name = "Def",
        tpeSExprs = List(STraitCall(module.entityOps.name,
          module.entityOps.tpeArgs.map(arg => STraitCall(arg.name, List()))))
      ).toTypeApply :: module.entityOps.ancestors
      val newEntity = module.entityOps.copy(ancestors = newAncestors)
      module.copy(entityOps = newEntity, entities = List(newEntity))
    }

    /** Puts the module to the cake. For example, trait Segments is transformed to
      * trait Segments {self: SegmentsDsl => ... } */
    def updateSelf(module: SModuleDef) = {
      module.copy(selfType = Some(SSelfTypeDef(
        name = "self",
        components = selfModuleComponents(module, "Module")
      )))
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

    /** Imports scalan._ and other packages needed by Scalan and further transformations. */
    def addImports(module: SModuleDef) = {
      //    val usedModules = snState.dependenceOfModule.getOrElse(module.name, List())
      //    val usedImports = usedModules.map(getImportByName)
      val usersImport = module.imports.collect{
        case imp @ SImportStat("scalan.compilation.KernelTypes._") => imp
      }
      module.copy(imports = SImportStat("scalan._") :: (usersImport))
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

    def genEntityImpicits(module: SModuleDef) = {

      val bodyWithImpElems = genElemsByTypeArgs(module.entityOps.tpeArgs) ++ module.entityOps.body
      val newEntity = module.entityOps.copy(body = bodyWithImpElems)

      module.copy(entityOps = newEntity, entities = List(newEntity))
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
          body = Some(SExprApply(SIdent("element"), unpackElem(classArg).toList)),
          isTypeDesc = true)
      }
      val newClasses = module.concreteSClasses.map { clazz =>
        val (definedElems, elemArgs) = genImplicitClassArgs(module, clazz) partition isElemAlreadyDefined
        val newImplicitArgs = SClassArgs(clazz.implicitArgs.args ++ elemArgs)
        val newBody = definedElems.map(convertElemValToMethod) ++ clazz.body

        clazz.copy(implicitArgs = newImplicitArgs, body = newBody)
      }

      module.copy(concreteSClasses = newClasses)
    }

    def genMethodsImplicits(module: SModuleDef) = {
      def genBodyItem(item: SBodyItem): SBodyItem = item match {
        case m: SMethodDef => genImplicitMethodArgs(module, m)
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

    def fixEvidences(module: SModuleDef) = {
      new MetaAstTransformer {
        def implicitElem(tpeSExprs: List[STpeExpr]) = {
          SExprApply(
            SSelect(
              SIdent("Predef"),
              "implicitly"
            ),
            tpeSExprs map (tpe => STraitCall("Elem", List(tpe)))
          )
        }

        override def identTransform(ident: SIdent): SExpr = ident match {
          case SIdent(name, Some(STraitCall("ClassTag", targs))) if name.startsWith("evidence$") =>
            super.exprApplyTransform(implicitElem(targs))
          case _ => super.identTransform(ident)
        }
        override def selectTransform(select: SSelect): SExpr = select match {
          case SSelect(_, name, Some(STraitCall("ClassTag", targs))) if name.startsWith("evidence$") =>
            super.exprApplyTransform(implicitElem(targs))
          case _ => super.selectTransform(select)
        }
      }.moduleTransform(module)
    }

//    def addModuleTrait(module: SModuleDef) = {
//      if (module.origModuleTrait.isEmpty) {
//        val mainName = module.name
//        val mt = STraitDef(
//          name = SModuleDef.moduleTraitName(mainName),
//          tpeArgs = Nil,
//          ancestors = List(STraitCall(s"impl.${mainName}Defs"), STraitCall("scala.wrappers.WrappersModule")).map(STypeApply(_)),
//          body = Nil, selfType = None, companion = None)
//        module.copy(origModuleTrait = Some(mt))
//      }
//      else module
//    }

    private val chain = scala.Function.chain(Seq(
      fixExistentialType _,
      externalTypeToWrapper _,
      composeParentWithExt _,
      addModuleAncestors _, addEntityAncestors _,
      updateSelf _,
      repSynonym _,
      addImports _,
      checkEntityCompanion _, checkClassCompanion _,
      cleanUpClassTags _, replaceClassTagByElem _, eliminateClassTagApply _,
      genEntityImpicits _, genClassesImplicits _, genMethodsImplicits _,
      fixEntityCompanionName _,
      fixEvidences _
    ))
    override def apply(module: Module): Module = chain(module)
  }
}
