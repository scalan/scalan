package scalan.meta

import scalan.meta.ScalanAstTransformers._
import scalan.meta.ScalanAstUtils._
import scalan.meta.ScalanAst._
import scalan.util.CollectionUtil._

class SModuleBuilder(implicit val context: AstContext) {

  // Pipeline Step
  def fixExistentialType(module: SModuleDef) = {
    new AstTransformer {
      def containsExistential(tpe: STpeExpr): Boolean = {
        var hasExistential = false
        val trans = new TypeTransformer {
          override def existTypeTransform(existType: STpeExistential): STpeExistential = {
            hasExistential = true
            super.existTypeTransform(existType)
          }
        }
        trans(tpe)

        hasExistential
      }

      override def applyTransform(apply: SApply): SApply = {
        val hasExistential = apply.argss exists (_.exists(arg =>
          containsExistential(arg.exprType.getOrElse(STpeEmpty()))
        ))
        def castToUniversal(targs: List[STpeExpr]) = {
          val newArgss = apply.argss map(sec => SArgSection(sec.map(arg =>
            SApply(SSelect(arg, "asRep"),targs, Nil)
          )))
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

  /** Pipeline Step
    * Make the module inherit from Base trait from Scalan */
  def addBaseToAncestors(module: SModuleDef) = {
    val newAncestors = STraitCall(name = "Base", tpeSExprs = List()).toTypeApply :: module.ancestors
    module.copy(ancestors = newAncestors)
  }

  /** Pipeline Step
    * Extends the entity T by Def[T] */
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
    * trait Segments {self: SegmentsDsl => ... }
    * Pipeline Step*/
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
    class ClassTagTransformer extends AstTransformer {
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
    new AstReplacer("ClassTag", (_:String) => "Elem") {
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
    new AstTransformer {
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
    val newBody = genDescMethodsByTypeArgs(module.entityOps.tpeArgs) ++ module.entityOps.body
    val newEntity = module.entityOps.copy(body = newBody)
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
      val (definedElems, elemArgs) = genImplicitArgsForClass(module, clazz) partition isElemAlreadyDefined
      val newArgs = (clazz.implicitArgs.args ++ elemArgs).distinctBy(_.tpe match {
        case TypeDescTpe(_,ty) => ty
        case t => t
      })
      val newImplicitArgs = SClassArgs(newArgs)
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
    def genCompanion(companion: Option[STmplDef]) = companion match {
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
    class ECompanionTransformer extends AstTransformer {
      override def applyTransform(apply: SApply): SApply = {
        apply match {
          case SApply(sel @ SSelect(SThis(clazz,_),_,_),_,_,_) if context.isEntity(clazz) =>
            apply.copy(fun = sel.copy(expr = SThis(clazz + "Companion")))
          case _ => super.applyTransform(apply)
        }
      }
    }
    new ECompanionTransformer().moduleTransform(module)
  }

  def fixEvidences(module: SModuleDef) = {
    new AstTransformer {
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

  /** Converts constructors (methods with name "<init>") to the apply method of companions. */
  def filterConstructor(module: SModuleDef): SModuleDef = {
    new AstTransformer {
      override def bodyTransform(body: List[SBodyItem]): List[SBodyItem] = body.filter {
        case m: SMethodDef if m.name == "<init>" => false
        case _ => true
      }
    }.moduleTransform(module)
  }

  def constr2apply(module: SModuleDef): SModuleDef = {
    val (constrs, entityBody) = module.entityOps.body partition {
      case m: SMethodDef if m.name == "<init>" => true
      case _ => false
    }
    val applies = constrs collect {
      case c: SMethodDef => c.copy(
        name = "apply",
        tpeArgs = (module.entityOps.tpeArgs ++ c.tpeArgs).distinct,
        // This is an internal annotation. And it should be ignored during in the backend.
        annotations = List(SMethodAnnotation("Constructor", List(SAssign(SIdent("original"), c))))
      )
    }
    val newEntityCompanion = module.entityOps.companion match {
      case Some(companion: STraitDef) => Some(companion.copy(body = applies ++ companion.body))
      case other => other
    }
    val newEntityOps = module.entityOps.copy(body = entityBody, companion = newEntityCompanion)

    module.copy(entityOps = newEntityOps, entities = List(newEntityOps))(context)
  }


  /** Discards all ancestors of the entity except TypeWrapperDef. It could be used as temporary solution
    * if inheritance of type wrappers is not supported. */
  def filterAncestors(module: SModuleDef): SModuleDef = {
    class filterAncestorTransformer extends AstTransformer {
      override def entityAncestorsTransform(ancestors: List[STypeApply]): List[STypeApply] = {
        ancestors.filter(_.tpe.isDef)
      }
    }

    new filterAncestorTransformer().moduleTransform(module)
  }

  /** Adds a prefix for type parameters To, Elem and Cont, to eliminate name conflicts. */
  def preventNameConflict(module: SModuleDef): SModuleDef = {
    val pipeline = scala.Function.chain(Seq(
      new TypeNameTransformer("Elem", module.name + "Elem").moduleTransform _,
      new TypeNameTransformer("Cont", module.name + "Cont").moduleTransform _,
      new TypeNameTransformer("To", module.name + "To").moduleTransform _
    ))
    val nonConflictModule = pipeline(module)
    nonConflictModule
  }
  
  def unrepAllTypes(module: SModuleDef): SModuleDef = {
    val t = new TypeTransformerInAst(new RepTypeRemover())
    t.moduleTransform(module)
  }
}
