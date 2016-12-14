package scalan.meta

import scala.reflect.internal.ModifierFlags
import PrintExtensions._
import scalan._
import scalan.util.{Contravariant, Covariant, Invariant}

object ScalanAst {
  // STpe universe --------------------------------------------------------------------------

  /** Type expressions */
  sealed abstract class STpeExpr {
    def name: String
    def tpeSExprs: List[STpeExpr] = Nil
  }
  type STpeExprs = List[STpeExpr]

  case class STpeEmpty() extends STpeExpr {
    def name = "Empty"
  }

  case class STpeConst(c: Any) extends STpeExpr {
    def name = "Constant"
  }

  case class STpeThis(name: String) extends STpeExpr

  /** <pre>.<single>.type */
  case class STpeSingle(pre: STpeExpr, name: String) extends STpeExpr

  /** Invocation of a trait with arguments */
  case class STraitCall(val name: String, override val tpeSExprs: List[STpeExpr]) extends STpeExpr {
    override def toString = name + tpeSExprs.asTypeParams()
  }

  case class STpePrimitive(val name: String, defaultValueString: String) extends STpeExpr {
    override def toString = name
  }

  case class STpeTypeBounds(lo: STpeExpr, hi: STpeExpr) extends STpeExpr {
    override def name = "Bounds"
    override def toString = ">:" + lo + "<:" + hi
  }

  val STpePrimitives = Map(
    "Any" -> STpePrimitive("Any", "AnyElement.defaultRepValue"),
    "AnyRef" -> STpePrimitive("AnyRef", "AnyRefElement.defaultRepValue"),
    "Nothing" -> STpePrimitive("Nothing", "???"),
    "Unit" -> STpePrimitive("Unit", "()"),
    "Short" -> STpePrimitive("Short", "0"),
    "Int" -> STpePrimitive("Int", "0"),
    "Long" -> STpePrimitive("Long", "0l"),
    "Byte" -> STpePrimitive("Byte", "0.toByte"),
    "Boolean" -> STpePrimitive("Boolean", "false"),
    "Float" -> STpePrimitive("Float", "0.0f"),
    "Double" -> STpePrimitive("Double", "0.0"),
    "String" -> STpePrimitive("String", "\"\""),
    "Char" -> STpePrimitive("Char", "0.toChar")
  )

  case class STpeTuple(override val tpeSExprs: List[STpeExpr]) extends STpeExpr {
    def name = "Tuple" + tpeSExprs.length
    override def toString = tpeSExprs.mkString("(", ", ", ")")
  }

  case class STpeFunc(domain: STpeExpr, range: STpeExpr) extends STpeExpr {
    def name = "Function1"
    override def tpeSExprs = List(domain, range)
    override def toString = {
      val domainStr = domain match {
        case tuple: STpeTuple => s"($tuple)"
        case _ => domain.toString
      }
      s"$domainStr => $range"
    }
  }

  implicit class STpeExprExtensions(self: STpeExpr) {
    def applySubst(subst: Map[String, STpeExpr]): STpeExpr = self match {
      case STraitCall(n, args) => // higher-kind usage of names is not supported  Array[A] - ok, A[Int] - nok
        subst.get(n) match {
          case Some(t) => t
          case None => STraitCall(n, args map { _.applySubst(subst) })
        }
      case STpeTuple(items) => STpeTuple(items map { _.applySubst(subst) })
      case _ => self
    }

    def unRep(module: SEntityModuleDef , config: CodegenConfig): Option[STpeExpr] = self match {
      case t if !config.isAlreadyRep => Some(t)
      case STraitCall("Elem", Seq(t)) => Some(self)
      case STraitCall("Rep", Seq(t)) => Some(t)
      case STraitCall("RFunc", Seq(a, b)) =>
        Some(STpeFunc(a, b))
      case STraitCall(name, args) =>
        val typeSynonyms = config.entityTypeSynonyms ++
          module.entityRepSynonym.toSeq.map(typeSyn => typeSyn.name -> module.entityOps.name).toMap

        typeSynonyms.get(name)
          // convert e.g. RVector or RepVector to Vector
          .orElse {
            def withoutPrefix(prefix: String, fallback: Option[String]) = {
              val indexAfterPrefix = prefix.length
              if (name.startsWith(prefix) && name(indexAfterPrefix).isUpper)
                Some(name.substring(indexAfterPrefix))
              else
                fallback
            }

            withoutPrefix("R", withoutPrefix("Rep", Some(name.stripSuffix("Rep")).filter(_ != name)))
          }
          .map(unReppedName => STraitCall(unReppedName, args))
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

  case class STpeSingleton(ref: SExpr) extends STpeExpr {
    def name = "Singleton"
  }
  case class STpeSelectFromTT(qualifier: STpeExpr, tname: String) extends STpeExpr {
    def name = "SelectFromTypeTree"
  }
  case class STpeAnnotated(tpt: STpeExpr, annot: String) extends STpeExpr {
    def name = "Annotated" + tpt.name
    override def toString = tpt.toString + " @" + annot
  }
  case class STpeExistential(tpt: STpeExpr, items: List[SBodyItem]) extends STpeExpr {
    def name = "Existential"
    override def toString = {
      val body = items map(_.toString)
      s"$tpt forSome {${body.mkString(";")}}"
    }
  }
  case class STpeBind(tname: String, texpr: STpeExpr) extends STpeExpr {
    def name = "TypedBind"
  }
  case class STpeCompound(parents: List[STpeExpr], items: List[SBodyItem]) extends STpeExpr {
    def name = "Compound Type Tree"
  }
  case class STpeMethod(tparams: List[String], params: List[STpeExpr], resultType: STpeExpr) extends STpeExpr {
    def name = tparams.mkString("[",",","]")+params.mkString("(",",",")")+resultType
  }
  // SAnnotation universe --------------------------------------------------------------------------
  trait SAnnotation {
    def annotationClass: String
    def args: List[SExpr]
  }
  case class STraitOrClassAnnotation(annotationClass: String, args: List[SExpr]) extends SAnnotation
  case class SMethodAnnotation(annotationClass: String, args: List[SExpr]) extends SAnnotation
  case class SArgAnnotation(annotationClass: String, args: List[SExpr]) extends SAnnotation

  final val ConstructorAnnotation = classOf[Constructor].getSimpleName
  final val ExternalAnnotation = classOf[External].getSimpleName
  final val ArgListAnnotation = classOf[ArgList].getSimpleName
  final val ContainerTypeAnnotation = classOf[ContainerType].getSimpleName
  final val FunctorTypeAnnotation = classOf[FunctorType].getSimpleName

  // SExpr universe --------------------------------------------------------------------------
  trait SExpr {
    def exprType: Option[STpeExpr] = None
  }
  case class SEmpty(override val exprType: Option[STpeExpr] = None) extends SExpr
  case class SConst(c: Any,
                    override val exprType: Option[STpeExpr] = None) extends SExpr
  case class SIdent(name: String,
                    override val exprType: Option[STpeExpr] = None) extends SExpr
  case class SAssign(left: SExpr, right: SExpr,
                     override val exprType: Option[STpeExpr] = None) extends SExpr
  case class SApply(fun: SExpr, ts: List[STpeExpr], argss: List[List[SExpr]],
                    override val exprType: Option[STpeExpr] = None) extends SExpr
  case class STypeApply(fun: SExpr, ts: List[STpeExpr],
                        override val exprType: Option[STpeExpr] = None) extends SExpr
  case class SSelect(expr: SExpr, tname: String,
                     override val exprType: Option[STpeExpr] = None) extends SExpr
  case class SBlock(init: List[SExpr], last: SExpr,
                    override val exprType: Option[STpeExpr] = None) extends SExpr
  case class SIf(cond: SExpr, th: SExpr, el: SExpr,
                 override val exprType: Option[STpeExpr] = None) extends SExpr
  case class SAscr(expr: SExpr, pt: STpeExpr,
                   override val exprType: Option[STpeExpr] = None) extends SExpr
  case class SFunc(params: List[SValDef], res: SExpr,
                   override val exprType: Option[STpeExpr] = None) extends SExpr
  case class SContr(name: String, args: List[SExpr],
                    override val exprType: Option[STpeExpr] = None) extends SExpr
  case class SThis(typeName: String,
                   override val exprType: Option[STpeExpr] = None) extends SExpr
  case class SSuper(name: String, qual: String, field: String,
                    override val exprType: Option[STpeExpr] = None) extends SExpr
  case class SLiteral(value: String, override val exprType: Option[STpeExpr] = None) extends SExpr
  case class SAnnotated(expr: SExpr, annot: String, override val exprType: Option[STpeExpr] = None) extends SExpr
  case class STuple(exprs: List[SExpr], override val exprType: Option[STpeExpr] = None) extends SExpr
  case class SCase(pat: SPattern, guard: SExpr, body: SExpr,
                   override val exprType: Option[STpeExpr] = None) extends SExpr
  case class SMatch(selector: SExpr, cases: List[SCase],
                    override val exprType: Option[STpeExpr] = None) extends SExpr

  trait SPattern
  case class SWildcardPattern() extends SPattern
  case class SLiteralPattern(const: SConst) extends SPattern
  case class SStableIdPattern(id: SIdent) extends SPattern
  case class SSelPattern(sel: SExpr, name: String) extends SPattern
  case class SAltPattern(alts: List[SPattern]) extends SPattern
  case class STypedPattern(tpe: STpeExpr) extends SPattern
  case class SBindPattern(name: String, pat: SPattern) extends SPattern
  case class SApplyPattern(fun: SExpr, pats: List[SPattern]) extends SPattern

  // SBodyItem universe ----------------------------------------------------------------------
  abstract class SBodyItem extends SExpr
  case class SImportStat(name: String) extends SBodyItem

  case class SMethodDef(
                         name: String, tpeArgs: STpeArgs,
                         argSections: List[SMethodArgs],
                         tpeRes: Option[STpeExpr],
                         isImplicit: Boolean,
                         isOverride: Boolean,
                         overloadId: Option[String],
                         annotations: List[SMethodAnnotation] = Nil,
                         body: Option[SExpr] = None,
                         isTypeDesc: Boolean = false)
    extends SBodyItem {
    def externalOpt: Option[SMethodAnnotation] = annotations.find(_.annotationClass == "External")
    def explicitArgs = argSections.flatMap(_.args.filterNot(_.impFlag))
    def allArgs = argSections.flatMap(_.args)
    def getOriginal: Option[SMethodDef] = {
      annotations.collectFirst {
        case mannot @ SMethodAnnotation("Constructor", _) => mannot.args collectFirst {
          case SAssign(SIdent("original",_), origMethod: SMethodDef, _) => origMethod
        }
      }.flatten
    }

    def cleanedArgs: List[SMethodArgs] = getOriginal match {
      case Some(method) =>
        def splitArgSections(sections: List[SMethodArgs]): (List[SMethodArgs], List[SMethodArgs]) = {
          sections partition  { _ match {
            case SMethodArgs((arg : SMethodArg) :: _) => arg.impFlag
            case _ => false
          }}
        }
        def existsClassTag(tpeArgs: List[STpeExpr]): Boolean = {
          val relatedClassTag = (getOriginal map (_.argSections map (_.args))).toList.flatten.flatten collectFirst {
            case marg @ SMethodArg(_,_,_,STraitCall("ClassTag", origTpeArgs),_,_,_) if origTpeArgs == tpeArgs => marg
          }

          !relatedClassTag.isEmpty
        }
        val (currImp, currNonImp) = splitArgSections(argSections)
        def isAdded(arg: SMethodArg): Boolean = arg match {
          case SMethodArg(_,_,_,STraitCall("Elem" | "Cont", tpeArgs),_,_,_) => !existsClassTag(tpeArgs)
          case _ => false
        }
        val newCurrImp = currImp map {s => s.copy(args = s.args.filterNot(isAdded(_)))} filter {!_.args.isEmpty}

        currNonImp ++ newCurrImp
      case None => argSections
    }
  }

  case class SValDef(
                      name: String,
                      tpe: Option[STpeExpr],
                      isLazy: Boolean,
                      isImplicit: Boolean,
                      expr: SExpr) extends SBodyItem
  case class STpeDef(name: String, tpeArgs: STpeArgs, rhs: STpeExpr) extends SBodyItem {
    override def toString = s"type $name"
  }

  case class STpeArg(
                      name: String,
                      bound: Option[STpeExpr],
                      contextBound: List[String],
                      tparams: List[STpeArg] = Nil,
                      flags: Long = ModifierFlags.PARAM)
  {
    def isHighKind = tparams.nonEmpty
    val variance =
      if (hasFlag(ModifierFlags.COVARIANT))
        Covariant
      else if (hasFlag(ModifierFlags.CONTRAVARIANT))
        Contravariant
      else
        Invariant
    def hasFlag(flag: Long) = (flag & flags) != 0L
    def declaration: String =
      if (isHighKind) {
        val params = tparams.rep(_.declaration)
        s"$name[$params]"
      }
      else name + bound.opt(b => s" <: ${b.name}")
    def toTraitCall: STraitCall = STraitCall(name, tparams.map(_.toTraitCall))
    def getArgBounds(args: List[SMethodArgs]): List[STraitCall] = {
      args.lastOption match {
        case Some(SMethodArgs(lastArgs)) =>
          lastArgs.collect{
            case SMethodArg(true,_,_,b @ STraitCall(_,List(STraitCall(tname,_))),_,_,_) if tname == name => b
          }
        case None => Nil
      }
    }
    def hasElemBound(args: List[SMethodArgs]) = getArgBounds(args) exists ( _.name == "Elem" )
    def hasContBound(args: List[SMethodArgs]) = getArgBounds(args) exists ( _.name == "Cont" )
    def hasWeakTypeTagBound(args: List[SMethodArgs]) = getArgBounds(args) exists ( _.name == "WeakTypeTag" )
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
    def isTypeDesc: Boolean
  }

  case class SMethodArg(
                         impFlag: Boolean,
                         overFlag: Boolean,
                         name: String,
                         tpe: STpeExpr,
                         default: Option[SExpr],
                         annotations: List[SArgAnnotation] = Nil,
                         isTypeDesc: Boolean = false)
    extends SMethodOrClassArg

  case class SClassArg(
                        impFlag: Boolean,
                        overFlag: Boolean,
                        valFlag: Boolean,
                        name: String,
                        tpe: STpeExpr,
                        default: Option[SExpr],
                        annotations: List[SArgAnnotation] = Nil,
                        isTypeDesc: Boolean = false)
    extends SMethodOrClassArg

  trait SMethodOrClassArgs {
    def args: List[SMethodOrClassArg]
  }

  case class SMethodArgs(args: List[SMethodArg]) extends SMethodOrClassArgs
  case class SClassArgs(args: List[SClassArg]) extends SMethodOrClassArgs

  case class SSelfTypeDef(name: String, components: List[STpeExpr]) {
    def tpe = components.mkString(" with ")
  }

  type Module = SEntityModuleDef
  abstract class STraitOrClassDef extends SBodyItem {
    def name: String
    def tpeArgs: List[STpeArg]
    def ancestors: List[STraitCall]
    def body: List[SBodyItem]
    def selfType: Option[SSelfTypeDef]
    def companion: Option[STraitOrClassDef]
    def isTrait: Boolean
    def annotations: List[STraitOrClassAnnotation]
    def args: SClassArgs
    def implicitArgs: SClassArgs
    def isHighKind = tpeArgs.exists(_.isHighKind)

    def isInheritedDeclared(propName: String, module: SEntityModuleDef) = {
      getInheritedDeclaredFields(module).contains(propName)
    }

    def isInheritedDefined(propName: String, module: SEntityModuleDef) = {
      getInheritedDefinedFields(module).contains(propName)
    }

    def getMethodsWithAnnotation(annClass: String) = body.collect {
      case md: SMethodDef if md.annotations.exists(a => a.annotationClass == annClass) => md
    }

    def getFieldDefs: List[SMethodDef] = body.collect {
      case md: SMethodDef if md.allArgs.isEmpty => md
    }

    def getAncestorTraits(module: SEntityModuleDef): List[STraitOrClassDef] = {
      ancestors.filter(tc => module.isEntity(tc.name)).map(tc => module.getEntity(tc.name))
    }

    def getAvailableFields(module: SEntityModuleDef): Set[String] = {
      getFieldDefs.map(_.name).toSet ++ getAncestorTraits(module).flatMap(_.getAvailableFields(module))
    }

    def getAvailableMethodDefs(module: SEntityModuleDef): Seq[SMethodDef] = {
      getFieldDefs ++ getAncestorTraits(module).flatMap(_.getAvailableMethodDefs(module))
    }

    def getInheritedMethodDefs(module: SEntityModuleDef): Seq[SMethodDef] = {
      getAncestorTraits(module).flatMap(_.getAvailableMethodDefs(module))
    }

    def getInheritedDeclaredFields(module: SEntityModuleDef): Set[String] = {
      getInheritedMethodDefs(module).collect { case md if md.body.isEmpty => md.name }.toSet
    }

    def getInheritedDefinedFields(module: SEntityModuleDef): Set[String] = {
      getInheritedMethodDefs(module).collect { case md if md.body.isDefined => md.name }.toSet
    }

    def getConcreteClasses = body.collect {
      case c: SClassDef if !c.hasAnnotation("InternalType")  => c
    }

    def getDeclaredElems(module: SEntityModuleDef):  List[(String, STpeExpr)] = {
      val res = (this :: getAncestorTraits(module))
        .flatMap(e => {
          val elems = e.body.collect {
            case SMethodDef(name, _, _, Some(elemOrCont), true, _, _, _, _, true) =>
              (name, elemOrCont)
          }
          elems
        })
      res
    }

    def getAnnotation(annotName: String) = annotations.find(a => a.annotationClass == annotName)

    def hasAnnotation(annotName: String) = getAnnotation(annotName).isDefined

    def clean: STraitOrClassDef
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
    val args = SClassArgs(Nil)
    lazy val implicitArgs: SClassArgs = {
      val implicitElems = body.collect {
        case SMethodDef(name, _, _, Some(elemOrCont), _, _, _, _, _, true) =>
          (name, elemOrCont)
      }
      val args: List[Either[STpeArg, SClassArg]] = tpeArgs.map { a =>
        val optDef = implicitElems.collectFirst {
          case (methName, elem @ STraitCall(_, List(STraitCall(name, _)))) if name == a.name =>
            (methName, elem)
          case (methName, elem @ STraitCall(_, List(STpeAnnotated(STraitCall(name, _), _)))) if name == a.name =>
            (methName, elem)
        }

        optDef match {
          case None =>
            Left(a)
          case Some((name, tyElem)) =>
            Right(SClassArg(true, false, true, name, tyElem, None, Nil, true))
        }
      }
      val missingElems = args.filter(_.isLeft)
      if (missingElems.nonEmpty)
        println/*sys.error*/(s"implicit def eA: Elem[A] should be declared for all type parameters of ${name}: missing ${missingElems.mkString(", ")}")
      SClassArgs(args.flatMap(a => a.fold(_ => Nil, List(_))))
    }
    def clean = {
      val _companion = companion.map(_.clean)
      copy(
        body = Nil,
        companion = _companion
      )
    }
  }

  final val BaseTypeTraitName = "TypeWrapper"

  implicit class STraitOrClassDefOps(td: STraitOrClassDef) {
    def optBaseType: Option[STpeExpr] = td.ancestors.find(a => a.name == BaseTypeTraitName) match {
      case Some(STraitCall(_, h :: _)) => Some(h)
      case _ => None
    }
    def baseTypeName: String = optBaseType match {
      case Some(STraitCall(name, _)) => name
      case _ => td.name
    }
    def baseInstanceName: String = baseTypeName.stripSuffix(".type")
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
    def clean = {
      val _companion = companion.map(_.clean)
      copy(
        body = Nil,
        companion = _companion
      )
    }
  }

  case class SObjectDef(
                         name: String,
                         ancestors: List[STraitCall],
                         body: List[SBodyItem]) extends STraitOrClassDef {

    val args = SClassArgs(Nil)
    def tpeArgs = Nil
    def selfType = None
    def companion = None
    def isTrait = false
    def annotations = Nil
    def implicitArgs = SClassArgs(Nil)
    def clean = {
      copy(
        body = Nil
      )
    }
  }

  case class SDeclaredImplementation(explicitMethods: List[SMethodDef]) {
    def containsMethodDef(m: SMethodDef) =
      explicitMethods.exists { em =>
        em.name == m.name && em.allArgs == m.allArgs &&
          em.tpeArgs == m.tpeArgs
      }
  }
  case class SDeclaredImplementations(declarations: Map[String, SDeclaredImplementation]) {
    def containsMethodDef(name: String, m: SMethodDef) =
      declarations.get(name) match {
        case Some(decl) => decl.containsMethodDef(m)
        case None => false
      }
  }

  type Entity = STraitOrClassDef
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
                               body: List[SBodyItem] = Nil,
                               stdDslImpls: Option[SDeclaredImplementations] = None,
                               expDslImpls: Option[SDeclaredImplementations] = None,
                               hasDsl: Boolean = false,
                               hasDslStd: Boolean = false,
                               hasDslExp: Boolean = false,
                               ancestors: List[STraitCall] = List())
  {
    def getFullName(shortName: String): String = s"$packageName.$name.$shortName"
    def isEqualName(shortName: String, fullName: String): Boolean =
      if (fullName == getFullName(shortName)) true
      else shortName == fullName

    def getEntity(name: String): STraitOrClassDef = {
      def isEqualName(shortName: String, fullName: String): Boolean =
        fullName == shortName || fullName == s"$packageName.$name.$shortName"
      def findByName(classes: List[STraitOrClassDef]) =
        classes.find(e => isEqualName(e.name, name))

      findByName(entities).orElse(findByName(concreteSClasses)).getOrElse {
        sys.error(s"Cannot find entity with name $name: available entities ${entities.map(_.name)}")
      }
    }

    def isEntity(name: String) = entities.exists(e => e.name == name)
    def isClass(name: String) = concreteSClasses.exists(c => c.name == name)
    def allEntities = entities ++ concreteSClasses

    private def hasDeclaredImplFor(traitName: String, decls: Option[SDeclaredImplementations]) = {
      decls match {
        case Some(impls) => impls.declarations.contains(traitName)
        case None => false
      }
    }

    def hasStdImplFor(traitName: String) = hasDeclaredImplFor(traitName, stdDslImpls)
    def hasExpImplFor(traitName: String) = hasDeclaredImplFor(traitName, expDslImpls)

    def clean = {
      val _entities = entities.map(_.clean)
      val _concreteSClasses = concreteSClasses.map(_.clean)
      val _entityOps = _entities.headOption.get
      copy(
        imports = Nil,
        entityRepSynonym = None,
        entityOps = _entityOps,
        entities = _entities,
        concreteSClasses = _concreteSClasses,
        methods = Nil,
        body = Nil,
        hasDsl = false,
        hasDslStd = false,
        hasDslExp = false,
        ancestors = Nil
      )
    }

    def printAst(ast: SEntityModuleDef): Unit = {
      val entityNames = ast.entities.map(_.name).mkString(",")
      val concreteClassNames = ast.concreteSClasses.map(_.name).mkString(",")

      print(
        s"""
          | Package name: ${ast.packageName}
          | Module name: ${ast.name}
          | Entity: $entityNames
          | Concrete Classes: $concreteClassNames
      """)
    }
  }

  object SEntityModuleDef {

    def tpeUseExpr(arg: STpeArg): STpeExpr = STraitCall(arg.name, arg.tparams.map(tpeUseExpr(_)))

    def wrapperImpl(entity: STraitDef, bt: STpeExpr): SClassDef = {
      val entityName = entity.name
      val entityImplName = entityName + "Impl"
      val typeUseExprs = entity.tpeArgs.map(tpeUseExpr(_))

      SClassDef(
        name = entityImplName,
        tpeArgs = entity.tpeArgs,
        args = SClassArgs(List(SClassArg(false, false, true, "wrappedValueOfBaseType", bt, None))),
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
    }

//    def apply(packageName: String, imports: List[SImportStat], moduleTrait: STraitDef, config: CodegenConfig): SEntityModuleDef = {
//      val moduleName = moduleTrait.name
//      val defs = moduleTrait.body
//      val classes = entity.optBaseType match {
//        case Some(bt) =>
//          val repBaseType = STraitCall("Rep", List(bt))
//          wrapperImpl(entity, repBaseType) :: moduleTrait.getConcreteClasses
//        case None => moduleTrait.getConcreteClasses
//      }
//      val methods = defs.collect { case md: SMethodDef => md }
//
//      SEntityModuleDef(packageName, imports, moduleName,
//        entityRepSynonym, entity, traits, classes, methods,
//        moduleTrait.selfType, Nil, None, moduleTrait.ancestors)
//    }
  }

}