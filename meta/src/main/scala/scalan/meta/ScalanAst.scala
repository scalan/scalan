package scalan.meta

trait ScalanAst {
  // STpe universe --------------------------------------------------------------------------

  /** Type expressions */
  sealed abstract class STpeExpr
  type STpeExprs = List[STpeExpr]

  /** Invocation of a trait with arguments */
  case class STraitCall(name: String, tpeSExprs: List[STpeExpr]) extends STpeExpr {
    override def toString = name + (if (tpeSExprs.isEmpty) "" else tpeSExprs.mkString("[", ",", "]"))
  }

  case class STpePrimitive(name: String, defaultValueString: String) extends STpeExpr {
    override def toString = name
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

  // SAnnotation universe --------------------------------------------------------------------------
  trait SAnnotation {
        def annotationClass: String
        def args: List[SExpr]
  }
  case class STraitOrClassAnnotation(annotationClass: String, args: List[SExpr]) extends SAnnotation
  case class SMethodAnnotation(annotationClass: String, args: List[SExpr]) extends SAnnotation
  case class SArgAnnotation(annotationClass: String, args: List[SExpr]) extends SAnnotation

  def annotationNameOf(a: java.lang.annotation.Annotation): String = a.getClass.getSimpleName

  //TODO extract this names from the corresponding classed of annotation somehow
  final val ConstuctorAnnotation = "Constructor"
  final val ExternalAnnotation = "External"
  final val ArgListAnnotation = "ArgList"

  // SExpr universe --------------------------------------------------------------------------
  trait SExpr
  case class SDefaultExpr(expr: String) extends SExpr

  // SBodyItem universe ----------------------------------------------------------------------
  abstract class SBodyItem
  case class SImportStat(name: String) extends SBodyItem

  case class SMethodDef(
    name: String, tpeArgs: STpeArgs,
    argSections: List[SMethodArgs],
    tpeRes: Option[STpeExpr],
    isImplicit: Boolean,
    overloadId: Option[String],
    annotations: List[SMethodAnnotation] = Nil, elem: Option[Unit] = None)
    extends SBodyItem {
    def externalOpt: Option[SMethodAnnotation] = annotations.filter(a => a.annotationClass == "External").headOption
    def isElem: Boolean = elem.isDefined
    def explicitArgs = argSections.flatMap(_.args.filterNot(_.impFlag))
    def allArgs = argSections.flatMap(_.args)
  }
  case class SValDef(name: String, tpe: Option[STpeExpr], isLazy: Boolean, isImplicit: Boolean) extends SBodyItem
  case class STpeDef(name: String, tpeArgs: STpeArgs, rhs: STpeExpr) extends SBodyItem

  case class STpeArg(
    name: String,
    bound: Option[STpeExpr],
    contextBound: List[String],
    tparams: List[STpeArg] = Nil)
  {
    def isHighKind = !tparams.isEmpty
    def declaration: String =
      if (isHighKind) {
        val params = tparams.map(_.declaration).mkString(",")
        s"$name[$params]"
      }
      else name
  }
  type STpeArgs = List[STpeArg]

  trait SMethodOrClassArg {
    def impFlag: Boolean
    def overFlag: Boolean
    def name: String
    def tpe: STpeExpr
    def default: Option[SExpr]
    def annotations: List[SArgAnnotation]
    def isArgList = annotations.exists(a => a.annotationClass == ArgListAnnotation)
  }

  case class SMethodArg(
    impFlag: Boolean,
    overFlag: Boolean,
    name: String,
    tpe: STpeExpr,
    default: Option[SExpr],
    annotations: List[SArgAnnotation] = Nil)
    extends SMethodOrClassArg

  case class SClassArg(
    impFlag: Boolean,
    overFlag: Boolean,
    valFlag: Boolean,
    name: String,
    tpe: STpeExpr,
    default: Option[SExpr],
    annotations: List[SArgAnnotation] = Nil)
    extends SMethodOrClassArg

  trait SMethodOrClassArgs {
    def args: List[SMethodOrClassArg]
  }

  case class SMethodArgs(args: List[SMethodArg]) extends SMethodOrClassArgs
  case class SClassArgs(args: List[SClassArg]) extends SMethodOrClassArgs

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
    def annotations: List[STraitOrClassAnnotation]
    def implicitArgs: SClassArgs
    def isHighKind = tpeArgs.exists(_.isHighKind)

    def getMethodsWithAnnotation(annClass: String) = body.collect {
      case md: SMethodDef if md.annotations.exists(a => a.annotationClass == annClass) => md
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

    def getConcreteClasses = body.collect { case c: SClassDef => c }
  }

  case class STraitDef(
    name: String,
    tpeArgs: List[STpeArg],
    ancestors: List[STraitCall],
    body: List[SBodyItem],
    selfType: Option[SSelfTypeDef],
    companion: Option[STraitOrClassDef],
    annotations: List[STraitOrClassAnnotation] = Nil) extends STraitOrClassDef {

    def isTrait = true

    lazy val implicitArgs: SClassArgs = {
      val implicitElems = body.collect {
        case md @ SMethodDef(name, _, _, Some(elem @ STraitCall("Elem", List(tyArg))), true, _, _, Some(_)) => (name, elem)
      }
      val args: List[Either[STpeArg, SClassArg]] = tpeArgs.map(a => {
        val optDef = implicitElems.collectFirst {
          case (methName, elem @ STraitCall("Elem", List(STraitCall(name, _)))) if name == a.name =>
            (methName, elem)
        }

        optDef.fold[Either[STpeArg, SClassArg]](Left[STpeArg, SClassArg](a)) { case (name, tyElem) => {
          Right[STpeArg, SClassArg](SClassArg(true, false, true, name, tyElem, None))
        }}
      })
      val missingElems = args.filter(_.isLeft)
      if (missingElems.length > 0)
        sys.error(s"implicit def eA: Elem[A] should be declared for all type parameters: missing ${missingElems}")
      SClassArgs(args.flatMap(a => a.fold(_ => Nil, List(_))))
    }
  }

  final val BaseTypeTraitName = "BaseTypeEx"

  implicit class STraitOrClassDefOps(td: STraitOrClassDef) {
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
    isAbstract: Boolean,
    annotations: List[STraitOrClassAnnotation] = Nil) extends STraitOrClassDef {
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


  object IsImplicitElem {
    def unapply(item: SBodyItem): Option[(String, STraitCall)] = item match {
      case md @ SMethodDef(name, _, _, Some(elem @ STraitCall("Elem", List(tyArg))), true, _, _, Some(_)) => Some((name, elem))
      case _ => None
    }
  }

  object SEntityModuleDef {

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
            args = SClassArgs(List(SClassArg(false, false, true, "wrappedValueOfBaseType", STraitCall("Rep", List(bt)), None))),
            implicitArgs = entity.implicitArgs,
            ancestors = List(STraitCall(entity.name, typeUseExprs)),
            body = List(

            ),
            selfType = None,
            companion = None,
//            companion = defs.collectFirst {
//              case c: STraitOrClassDef if c.name.toString == entityImplName + "Companion" => c
//            },
            true, Nil

          )
          defaultBTImpl :: moduleTrait.getConcreteClasses
        case None => moduleTrait.getConcreteClasses
      }

      val methods = defs.collect { case md: SMethodDef => md }

      SEntityModuleDef(packageName, imports, moduleName, entityRepSynonym, entity, traits, classes, methods, moduleTrait.selfType)
    }
  }
}
