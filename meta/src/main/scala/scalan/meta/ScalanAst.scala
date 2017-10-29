package scalan.meta

import java.io.File
import java.util.Objects
import com.typesafe.config.ConfigUtil
import scalan.meta.PrintExtensions._
import scala.collection.mutable.{Map => MMap}
import scala.reflect.internal.ModifierFlags
import scalan._
import scalan.util.{Covariant, Contravariant, FileUtil, Invariant}
import scalan.util.CollectionUtil._
import scalan.meta.ScalanAstExtensions._
import scala.tools.nsc.Global

object ScalanAst {

  // STpe universe --------------------------------------------------------------------------

  sealed abstract class STpeExpr {
    def name: String

    def args: List[STpeExpr] = Nil
  }

  type STpeExprs = List[STpeExpr]

  /** Represents scala.reflect.internal.Types.NoType | NoPrefix */
  case class STpeEmpty() extends STpeExpr {
    def name = "Empty"
  }

  /** Represents scala.reflect.internal.Types.ConstantType */
  case class STpeConst(const: SConst) extends STpeExpr {
    def name = "Constant"
  }

  /** A class for this-types of the form <sym>.this.type */
  case class STpeThis(fullNameString: String) extends STpeExpr {
    def name = s"$fullNameString.this.type"
  }

  /** <pre>.<single>.type */
  case class STpeSingle(pre: STpeExpr, name: String) extends STpeExpr

  /** Invocation of a trait with arguments */
  case class STraitCall(val name: String, override val args: List[STpeExpr] = Nil) extends STpeExpr {
    override def toString = name + args.asTypeParams()
    def isDef = (name == "Def")
    def toTypeApply = STypeApply(this, Nil)
  }

  case class STpePrimitive(val name: String, defaultValueString: String) extends STpeExpr {
    override def toString = name
  }

  case class STpeTypeBounds(lo: STpeExpr, hi: STpeExpr) extends STpeExpr {
    override def name = "Bounds"

    override def toString = ">:" + lo + "<:" + hi
  }

  val TpeAny = STpePrimitive("Any", "AnyElement.defaultRepValue")
  val TpeAnyRef = STpePrimitive("AnyRef", "AnyRefElement.defaultRepValue")
  val TpeUnit = STpePrimitive("Unit", "()")
  val TpeShort = STpePrimitive("Short", "0")
  val TpeInt = STpePrimitive("Int", "0")
  val TpeLong = STpePrimitive("Long", "0l")
  val TpeByte = STpePrimitive("Byte", "0.toByte")
  val TpeBoolean = STpePrimitive("Boolean", "false")
  val TpeFloat = STpePrimitive("Float", "0.0f")
  val TpeDouble = STpePrimitive("Double", "0.0")
  val TpeString = STpePrimitive("String", "\"\"")
  val TpeChar = STpePrimitive("Char", "0.toChar")
  val TpeNothing = STpePrimitive("Nothing", "???")

  val STpePrimitives = Map(
    "Any" -> TpeAny,
    "AnyRef" -> TpeAnyRef,
    "Nothing" -> TpeNothing,
    "Unit" -> TpeUnit,
    "Short" -> TpeShort,
    "Int" -> TpeInt,
    "Long" -> TpeLong,
    "Byte" -> TpeByte,
    "Boolean" -> TpeBoolean,
    "Float" -> TpeFloat,
    "Double" -> TpeDouble,
    "String" -> TpeString,
    "Char" -> TpeChar
  )

  case class STpeTuple(override val args: List[STpeExpr]) extends STpeExpr {
    def name = "Tuple" + args.length

    override def toString = args.mkString("(", ", ", ")")
  }

  case class STpeStruct(fields: List[(String, STpeExpr)]) extends STpeExpr {
    def name = "Struct"

    override def toString = fields.map { case (n, t) => s"$n: $t" }.mkString("{", "; ", "}")
  }

  case class STpeFunc(domain: STpeExpr, range: STpeExpr) extends STpeExpr {
    def name = "Function1"

    override def args = List(domain, range)

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
          case None =>
            STraitCall(n, args map { _.applySubst(subst) })
        }
      case STpeTuple(items) =>
        STpeTuple(items map { _.applySubst(subst) })
      case _ => self
    }

    def unRep(module: SUnitDef, isVirtualized: Boolean): Option[STpeExpr] = self match {
      case t if !isVirtualized => Some(t)
      case STraitCall("Elem", Seq(t)) =>  // Elem[t] --> tpe
        Some(self)
      case module.context.RepTypeOf(t) => Some(t)
      case _ => None
    }

    def isRep(module: SUnitDef, isVirtualized: Boolean) = unRep(module, isVirtualized) match {
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
      val body = items map (_.toString)
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
    def name = tparams.mkString("[", ",", "]") + params.mkString("(", ",", ")") + resultType
  }

  // TpePath universe ------------------------------------------------------------------------------
  sealed abstract class STpePath {
  }

  case object SNilPath extends STpePath

  sealed abstract class SBasedPath extends STpePath {
    def base: STpeExpr
  }

  case class STuplePath(base: STpeExpr, index: Int, tail: STpePath) extends SBasedPath

  case class SDomPath(base: STpeExpr, tail: STpePath) extends SBasedPath

  case class SRangePath(base: STpeExpr, tail: STpePath) extends SBasedPath

  case class SThunkPath(base: STpeExpr, tail: STpePath) extends SBasedPath

  case class SStructPath(base: STpeExpr, fieldName: String, tail: STpePath) extends SBasedPath

  case class SEntityPath(base: STpeExpr, entity: SEntityDef, tyArg: STpeArg, tail: STpePath) extends SBasedPath

  object STpePath {
    def findInEntity(e: SEntityDef, tc: STraitCall, argName: String)
                    (implicit context: AstContext): Option[STpePath] = {
      val args = tc.args
      for (i <- args.indices) {
        find(args(i), argName) match {
          case Some(tailPath) =>
            return Some(SEntityPath(tc, e, e.tpeArgs(i), tailPath))
          case None =>
        }
      }
      None
    }

    def find(tpe: STpeExpr, argName: String)(implicit context: AstContext): Option[STpePath] = tpe match {
      case STpePrimitive(_, _) => None
      case STpeFunc(d, r) =>
        find(d, argName) match {
          case Some(tailPath) => Some(SDomPath(tpe, tailPath))
          case None => find(r, argName) match {
            case Some(tailPath) => Some(SRangePath(tpe, tailPath))
            case None => None
          }
        }
      case t@STpeTuple(_) =>
        def findInTuple(t: STpeTuple): Option[STpePath] = {
          for ((item, i) <- t.args.zipWithIndex) {
            find(item, argName) match {
              case Some(tailPath) =>
                return Some(STuplePath(t, i, tailPath))
              case None =>
            }
          }
          None
        }
        findInTuple(t)
      case STraitCall("Rep", List(tT)) =>
        find(tT, argName)
      case STraitCall("Thunk", List(tT)) =>
        find(tT, argName).map(tail => SThunkPath(tpe, tail))
      case context.TypeDef(_, context.RepTypeOf(STraitCall(en @ context.Entity(_, e), args))) =>
        findInEntity(e, STraitCall(en, args), argName)
      case s@STpeStruct(_) =>
        def findInStruct(s: STpeStruct): Option[STpePath] = {
          for ((fn, ft) <- s.fields) {
            find(ft, argName) match {
              case Some(tailPath) =>
                return Some(SStructPath(s, fn, tailPath))
              case None =>
            }
          }
          None
        }
        findInStruct(s)
      case STraitCall(`argName`, Nil) =>
        Some(SNilPath)
      case tc@STraitCall(context.Entity(_, e), args) =>
        findInEntity(e, tc, argName)
      case _ => None
    }
  }

  // SAnnotation universe --------------------------------------------------------------------------
  trait SAnnotation {
    def annotationClass: String
    def args: List[SExpr]
  }

  /** Annotation that can be attached to any STmplDef */
  case class STmplAnnotation(annotationClass: String, args: List[SExpr]) extends SAnnotation

  case class SMethodAnnotation(annotationClass: String, args: List[SExpr]) extends SAnnotation

  case class SArgAnnotation(annotationClass: String, args: List[SExpr]) extends SAnnotation

  case class STypeArgAnnotation(annotationClass: String, args: List[SExpr]) extends SAnnotation

  final val ConstructorAnnotation    = classOf[Constructor].getSimpleName
  final val ExternalAnnotation       = classOf[External].getSimpleName
  final val ArgListAnnotation        = classOf[ArgList].getSimpleName
  final val ContainerTypeAnnotation  = classOf[ContainerType].getSimpleName
  final val FunctorTypeAnnotation    = classOf[FunctorType].getSimpleName
  final val ReifiedTypeArgAnnotation = classOf[Reified].getSimpleName

  def externalTmplAnnotation(externalTmplName: String) =
    STmplAnnotation(ExternalAnnotation, List(SConst(externalTmplName,Some(TpeString))))

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

  case class SArgSection(args: List[SExpr])
  implicit def toArgSection(args: List[SExpr]): SArgSection = SArgSection(args)
  implicit def fromArgSection(section: SArgSection): List[SExpr] = section.args

  case class SApply(fun: SExpr, ts: List[STpeExpr],
                    argss: List[SArgSection],
                    override val exprType: Option[STpeExpr] = None) extends SExpr

  case class SExprApply(fun: SExpr, ts: List[STpeExpr],
                        override val exprType: Option[STpeExpr] = None) extends SExpr

  case class STypeApply(tpe: STraitCall, ts: List[SExpr] = Nil) extends SExpr {
    override val exprType: Option[STpeExpr] = Some(tpe)
  }

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

//    def isExtractableArg(module: SModuleDef, tpeArg: STpeArg): Boolean = {
//      allArgs.exists(a => STpePath.find(module, a.tpe, tpeArg.name).isDefined)
//    }

    def explicitArgs = argSections.flatMap(_.args.filterNot(_.impFlag))

    def allArgs = argSections.flatMap(_.args)

    def getOriginal: Option[SMethodDef] = {
      annotations.collectFirst {
        case mannot@SMethodAnnotation("Constructor", _) => mannot.args collectFirst {
          case SAssign(SIdent("original", _), origMethod: SMethodDef, _) => origMethod
        }
      }.flatten
    }

    def cleanedArgs: List[SMethodArgs] = getOriginal match {
      case Some(method) =>
        def existsClassTag(tpeArgs: List[STpeExpr]): Boolean = {
          val relatedClassTag = (getOriginal map (_.argSections map (_.args))).toList.flatten.flatten collectFirst {
            case marg@SMethodArg(_, _, _, STraitCall("ClassTag", origTpeArgs), _, _, _) if origTpeArgs == tpeArgs => marg
          }
          !relatedClassTag.isEmpty
        }

        def isAdded(arg: SMethodArg): Boolean = arg match {
          case SMethodArg(_, _, _, STraitCall("Elem" | "Cont", tpeArgs), _, _, _) => !existsClassTag(tpeArgs)
          case _ => false
        }

        val (currImp, currNonImp) = argSections.splitArgSections()

        val newCurrImp = currImp map { s => s.copy(args = s.args.filterNot(isAdded(_))) } filter {
          !_.args.isEmpty
        }
        currNonImp ++ newCurrImp
      case None => argSections
    }
  }

  object SMethodDef {
    def emptyArgSection = List(SMethodArgs(Nil))
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
                      bound: Option[STpeExpr] = None,
                      contextBound: List[String] = Nil,
                      tparams: List[STpeArg] = Nil,
                      flags: Long = ModifierFlags.PARAM,
                      annotations: List[STypeArgAnnotation] = Nil) {
    def isHighKind = tparams.nonEmpty
    def classOrMethodArgName: String = if (isHighKind) "c" + name else "e" + name
    def descName: String = if (isHighKind) "Cont" else "Elem"

    val variance =
      if (hasFlag(ModifierFlags.COVARIANT))
        Covariant
      else if (hasFlag(ModifierFlags.CONTRAVARIANT))
        Contravariant
      else
        Invariant

    def isCovariant = variance == Covariant

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
          lastArgs.collect {
            case SMethodArg(true, _, _, b@STraitCall(_, List(STraitCall(tname, _))), _, _, _) if tname == name => b
          }
        case None => Nil
      }
    }

    def hasElemBound(args: List[SMethodArgs]) = getArgBounds(args) exists (_.name == "Elem")

    def hasContBound(args: List[SMethodArgs]) = getArgBounds(args) exists (_.name == "Cont")

    def hasWeakTypeTagBound(args: List[SMethodArgs]) = getArgBounds(args) exists (_.name == "WeakTypeTag")
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
  object SMethodOrClassArg {
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

  type Module = SUnitDef

  /** Correspond to TmplDef syntax construct of Scala.
    * (See http://scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html)
    */
  abstract class SEntityDef extends SBodyItem {
    def name: String

    def tpeArgs: List[STpeArg]

    def ancestors: List[STypeApply]

    def body: List[SBodyItem]

    def selfType: Option[SSelfTypeDef]

    def companion: Option[SEntityDef]

    def isTrait: Boolean

    def annotations: List[STmplAnnotation]

    def args: SClassArgs

    def implicitArgs: SClassArgs

    def findMethod(name: String): Option[SMethodDef] = {
      body.collectFirst { case m: SMethodDef if m.name == name => m }
    }

    def findVal(name: String): Option[SValDef] = {
      body.collectFirst { case v: SValDef if v.name == name => v }
    }

    def tpeArgIndex(tpeArgName: String) = {
      tpeArgs.zipWithIndex.find { case (a, i) => a.name == tpeArgName }.get._2
    }

    def firstAncestorType = ancestors.headOption.map(_.tpe)

    def isHighKind = tpeArgs.exists(_.isHighKind)

    def isInheritedDeclared(propName: String, module: SUnitDef) = {
      getInheritedDeclaredFields(module).contains(propName)
    }

    def isInheritedDefined(propName: String, module: SUnitDef) = {
      getInheritedDefinedFields(module).contains(propName)
    }

    def getMethodsWithAnnotation(annClass: String) = body.collect {
      case md: SMethodDef if md.annotations.exists(a => a.annotationClass == annClass) => md
    }

    def getFieldDefs: List[SMethodDef] = body.collect {
      case md: SMethodDef if md.allArgs.isEmpty => md
    }

    def getAncestorTraits(module: SUnitDef): List[SEntityDef] = {
      ancestors.collect { case STypeApply(STraitCall(module.context.Entity(m, e),_), _) => e }
//      ancestors.filter(a => module.isEntity(a.tpe.name)).map(a => module.getEntity(a.tpe.name))
    }

    def getAvailableFields(module: SUnitDef): Set[String] = {
      getFieldDefs.map(_.name).toSet ++ getAncestorTraits(module).flatMap(_.getAvailableFields(module))
    }

    def getAvailableMethodDefs(module: SUnitDef): Seq[SMethodDef] = {
      getFieldDefs ++ getAncestorTraits(module).flatMap(_.getAvailableMethodDefs(module))
    }

    def getInheritedMethodDefs(module: SUnitDef): Seq[SMethodDef] = {
      getAncestorTraits(module).flatMap(_.getAvailableMethodDefs(module))
    }

    def getInheritedDeclaredFields(module: SUnitDef): Set[String] = {
      getInheritedMethodDefs(module).collect { case md if md.body.isEmpty => md.name }.toSet
    }

    def getInheritedDefinedFields(module: SUnitDef): Set[String] = {
      getInheritedMethodDefs(module).collect { case md if md.body.isDefined => md.name }.toSet
    }

    def getDeclaredElems(module: SUnitDef): List[(String, STpeExpr)] = {
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

    def clean: SEntityDef
  }

  case class STraitDef(
                        name: String,
                        tpeArgs: List[STpeArg],
                        ancestors: List[STypeApply],
                        body: List[SBodyItem],
                        selfType: Option[SSelfTypeDef],
                        companion: Option[SEntityDef],
                        annotations: List[STmplAnnotation] = Nil) extends SEntityDef {
    def isTrait = true

    val args = SClassArgs(Nil)
    lazy val implicitArgs: SClassArgs = {
      val implicitMethods = body.collect {
        case SMethodDef(name, _, _, Some(elemOrCont), _, _, _, _, _, true) =>
          (name, elemOrCont)
      }
      val args: List[Either[STpeArg, SClassArg]] = tpeArgs.map { a =>
        val optMeth: Option[(String, STraitCall)] = implicitMethods.collectFirst {
          case (methName, tpeRes@STraitCall(_, List(STraitCall(name, _)))) if name == a.name =>
            (methName, tpeRes)
          case (methName, tpeRes@STraitCall(_, List(STpeAnnotated(STraitCall(name, _), _)))) if name == a.name =>
            (methName, tpeRes)
        }
        optMeth match {
          case None =>
            Left(a)
          case Some((methName, tpeRes)) =>
            Right(SClassArg(true, false, true, methName, tpeRes, None, Nil, true))
        }
      }
      val missingElems = args.filter(_.isLeft)
      if (missingElems.nonEmpty)
        println /*sys.error*/ (s"implicit def eA: Elem[A] should be declared for all type parameters of ${name}: missing ${missingElems.mkString(", ")}")
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

  implicit class STmplDefOps(td: SEntityDef) {
  }

  case class SClassDef(
                        name: String,
                        tpeArgs: List[STpeArg],
                        args: SClassArgs,
                        implicitArgs: SClassArgs,
                        ancestors: List[STypeApply],
                        body: List[SBodyItem],
                        selfType: Option[SSelfTypeDef],
                        companion: Option[SEntityDef],
                        isAbstract: Boolean,
                        annotations: List[STmplAnnotation] = Nil) extends SEntityDef {
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
                         ancestors: List[STypeApply],
                         body: List[SBodyItem]) extends SEntityDef {
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

  type Entity = SEntityDef

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
    STpePrimitives.contains(name)
  }
  def isStandardType(name: String): Boolean = {
    Set("Tuple", "Function").exists(name.startsWith(_)) ||
      Set("ClassTag").contains(name)
  }

  class AstContext(val configs: List[UnitConfig], val parsers: ScalanParsers[Global], okLoadModules: Boolean = false) {

    /** Mapping of external type names to their wrappers. */
    private val wrappers = MMap[String, WrapperDescr]()

    /** Mapping of W-entities to the corresponding wrapped type name ("WArray" -> "Array") */
    private[scalan] val entityToWrapper = MMap[String, String]()

    /** Mapping of <packageName>.<moduleName> to definition.
      * Initial set of modules in loaded from configs and later new modules can be added. */
    private[scalan] val modules = MMap[String, SUnitDef]()

    def loadModulesFromResources(): Unit = {
      for (c <- configs) {
        val m = parsers.loadModuleDefFromResource(c.entityFile)
        addModule(m)
      }
    }

    def loadModulesFromFolders(): Unit = {
      for (c <- configs) {
        val file = c.getFile
        try {
          val m = parsers.parseEntityModule(file)(new parsers.ParseCtx(c.isVirtualized)(this))
          addModule(m)
        } catch {
          case t: Throwable =>
            val fullName = new File(FileUtil.currentWorkingDir, file.getPath)
            throw new RuntimeException(s"Error loading module from $fullName", t)
        }
      }
    }

    def updateWrapper(typeName: String, descr: WrapperDescr) = {
      wrappers(typeName) = descr
      val entityName = descr.module.traits(0).name
      entityToWrapper(entityName) = typeName
    }

    def externalTypes = wrappers.keySet

    def hasWrapper(typeName: String) = wrappers.contains(typeName)
    def getWrapper(typeName: String) = wrappers.get(typeName)

    def forEachWrapper(action: ((String, WrapperDescr)) => Unit) = {
      wrappers.foreach(action)
    }

    def transformWrappers(transformer: ((String, WrapperDescr)) => WrapperDescr) = {
      wrappers.transform(scala.Function.untupled(transformer))
    }

    /** The types that shouldn't be Rep[].
      * For example List("Elem", "Cont", "ClassTag") */
    val typeClasses = Set("Elem", "Cont", "ClassTag", "Functor")

    def isEntity(name: String): Boolean = {
      val res = for (m <- modules.values; e <- m.traits if e.name == name) yield ()
      res.nonEmpty
    }
    def isEntityCompanion(name: String): Boolean = {
      val res = for (m <- modules.values; e <- m.traits; c <- e.companion if c.name == name) yield ()
      res.nonEmpty
    }
    def isClass(name: String): Boolean = {
      val res = for (m <- modules.values; c <- m.classes if c.name == name) yield ()
      res.nonEmpty
    }
    def isClassCompanion(name: String): Boolean = {
      val res = for (m <- modules.values; c <- m.classes; comp <- c.companion if comp.name == name) yield ()
      res.nonEmpty
    }
    def isModule(name: String): Boolean = {
      modules.valuesIterator.map(_.name).toSet.contains(name)
    }

    def allModules: Iterator[SUnitDef] = wrappers.valuesIterator.map(_.module) ++ modules.valuesIterator

    //TODO refactor to use Name for more precise ModuleEntity search
    def findModuleEntity(entityName: String): Option[(Module, Entity)] = {
      def isEqualName(m: SUnitDef, shortName: String, fullName: String): Boolean =
        fullName == shortName || fullName == s"${m.packageName}.$entityName.$shortName"

      def findByName(m: SUnitDef, es: List[SEntityDef]) =
        es.find(e => isEqualName(m, e.name, entityName))

      val res = allModules collectFirst scala.Function.unlift { m =>
        findByName(m, m.traits)
          .orElse(findByName(m, m.classes))
          .map((m, _))
      }
      res
    }

    def typeDefs: Map[String, STpeDef] = {
      val defs = for {
          m <- allModules
          t <- m.typeDefs
        }
        yield t.name -> t
      defs.toMap
    }

    def hasModule(packageName: String, moduleName: String): Boolean = {
      val key = Name.fullNameString(packageName, moduleName)
      modules.contains(key)
    }

    def getModule(packageName: String, moduleName: String): SUnitDef = {
      val key = Name.fullNameString(packageName, moduleName)
      modules(key)
    }



    def addModule(module: SUnitDef) = {
      val key = module.getModuleKey
      modules(key) = module
    }

    object TypeDef {
      /** Recognizes usage of STpeDef and substitutes args to rhs */
      def unapply(tpe: STpeExpr): Option[(STpeDef, STpeExpr)] = tpe match {
        case STraitCall(n, args) =>
          typeDefs.get(n).map { td =>
            val subst = td.tpeArgs.map(_.name).zip(args).toMap
            (td, td.rhs.applySubst(subst))
          }
        case _ => None
      }
    }

    object RepTypeOf {
      def unapply(tpe: STpeExpr): Option[STpeExpr] = tpe match {
        case STraitCall("Rep", Seq(t)) =>   // Rep[t] --> t
          Some(t)
        case STraitCall("RFunc", Seq(a, b)) =>  // RFunc[a,b] --> a => b
          Some(STpeFunc(a, b))
        case TypeDef(td, RepTypeOf(t)) => // type RepCol[args] = Rep[Col[args]] then RepCol[args] --> Col[args]
          Some(t)
        case _ => None
      }
    }

    object Entity {
      def unapply(name: String): Option[(Module, Entity)] =
        findModuleEntity(name)
    }

    object WrapperEntity {
      def unapply(name: String): Option[(SEntityDef, String)] = name match {
        case Entity(_, e) =>
          e.getAnnotation(ExternalAnnotation) match {
            case Some(STmplAnnotation(_, List(SConst(externalName: String, _)))) => Some((e, externalName))
            case _ => None
          }
        case _ => None
      }
    }

  }

  case class SUnitDef(packageName: String,
                      imports: List[SImportStat],
                      name: String,
                      typeDefs: List[STpeDef],
                      traits: List[STraitDef],
                      classes: List[SClassDef],
                      methods: List[SMethodDef],
                      selfType: Option[SSelfTypeDef],
                      ancestors: List[STypeApply],
                      origModuleTrait: Option[STraitDef], // original module trait declared
                      isVirtualized: Boolean,
                      okEmitOrigModuleTrait: Boolean = true)
                     (@transient implicit val context: AstContext) {
    //TODO unify Module names
    def getModuleKey: String = Name.fullNameString(packageName, name)
    def getModuleTraitName: String = SUnitDef.moduleTraitName(name)

    def getEntity(name: String): SEntityDef = {
      findEntity(name).getOrElse {
        sys.error(s"Cannot find entity with name $name: available entities ${traits.map(_.name)}")
      }
    }

    def findEntity(name: String): Option[Entity] = {
      traits.collectFirst { case e if e.name == name => e }
          .orElse(classes.collectFirst { case c if c.name == name => c })
    }

    def isEntity(name: String) = traits.exists(e => e.name == name)

    def isClass(name: String) = classes.exists(c => c.name == name)

    def allEntities = traits ++ classes

    private def hasDeclaredImplFor(traitName: String, decls: Option[SDeclaredImplementations]) = {
      decls match {
        case Some(impls) => impls.declarations.contains(traitName)
        case None => false
      }
    }

    def dependencies: Seq[SUnitDef] = {
      Seq() // TODO collect dependencies for the module
    }

    def clean = {
      val _entities = traits.map(_.clean)
      val _concreteSClasses = classes.map(_.clean)
      copy(
        imports = Nil,
        typeDefs = Nil,
        traits = _entities,
        classes = _concreteSClasses,
        methods = Nil,
        origModuleTrait = None,
        ancestors = Nil
      )
    }

    def printAst(ast: SUnitDef): Unit = {
      val entityNames = ast.traits.map(_.name).mkString(",")
      val concreteClassNames = ast.classes.map(_.name).mkString(",")
      print(
        s"""
          | Package name: ${ast.packageName}
          | Module name: ${ast.name}
          | Entity: $entityNames
          | Concrete Classes: $concreteClassNames
      """)
    }
  }

  object SUnitDef {
    /** Module trait name related to main trait */
    def moduleTraitName(mainTraitName: String) = mainTraitName + "Module"
    def tpeUseExpr(arg: STpeArg): STpeExpr = STraitCall(arg.name, arg.tparams.map(tpeUseExpr(_)))
  }

  /** Helper class to represent an entity in a module.
    * STmplDef cannot have direct reference to module due to immutability of ScalanAst.
    * This class implements equality and can be used as a key in a Map and as element of Set. */
  class ModuleEntity(val module: SUnitDef, val entity: SEntityDef) {
    override def equals(other: Any) = (this eq other.asInstanceOf[AnyRef]) || (other match {
      case other: ModuleEntity =>
         module.packageName == other.module.packageName  &&
         module.name == other.module.name &&
         entity.name == other.entity.name
      case _ => false
    })
    override def hashCode = Objects.hash(module.packageName, module.name, entity.name)
  }
  object ModuleEntity {
    def apply(m: Module, e: SEntityDef) = new ModuleEntity(m, e)
    def unapply(me: ModuleEntity) = Some((me.module, me.entity))
  }

  object TypeDescTpe {
    val DescNames = Set("Elem", "Cont", "Functor")
    def unapply(tpe: STpeExpr): Option[(String, STpeExpr)] = tpe match {
      case STraitCall(tname, List(STpeAnnotated(arg,_))) if DescNames.contains(tname) => Some((tname, arg))
      case STraitCall(tname, List(arg)) if DescNames.contains(tname) => Some((tname, arg))
      case _ => None
    }
  }

  object TypeDescArg {
    def unapply(arg: SMethodOrClassArg): Option[(String, String)] = arg.tpe match {
      case TypeDescTpe(descName, STraitCall(typeName, Nil)) => Some((descName, typeName))
      case _ => None
    }
  }

  case class WrapperConfig(name: String, annotations: List[String] = Nil)

  case class NonWrapper(name: String)

  object WrapperConfig {
    def default(name: String) = WrapperConfig(name)
  }

  case class WrapperDescr(module: SUnitDef, ownerChain: List[String], config: WrapperConfig)

  case class KernelType(name: String, confKey: String)

  object KernelType {
    def apply(name: String): KernelType = {
      if (ConfigUtil.joinPath(name) == name)
        KernelType(name, name.toLowerCase)
      else
        throw new IllegalArgumentException(s"${name.toLowerCase} is not a legal unquoted configuration key, supply one explicitly")
    }

    val Scala = KernelType("Scala")
    val Cpp   = KernelType("C++", "cpp")
    val Lua   = KernelType("Lua")
  }

  def optimizeMethodImplicits(m: SMethodDef)(implicit ctx: AstContext): SMethodDef = {
    val explicitArgs = m.explicitArgs
    val newSections = m.argSections.filterMap(as => {
      val newArgs = as.args.filter {
        case arg@TypeDescArg(_, tyName) if arg.impFlag =>
          !canBeExtracted(explicitArgs, tyName)
        case _ => true
      }
      if (newArgs.nonEmpty) Some(SMethodArgs(newArgs)) else None
    })
    m.copy(argSections = newSections)
  }

  def optimizeTraitImplicits(t: STraitDef)(implicit context: AstContext): STraitDef = {
    val newBody = t.body.map {
      case m: SMethodDef => optimizeMethodImplicits(m)
      case item => item
    }
    val newCompanion = t.companion.map(optimizeComponentImplicits(_))
    t.copy(
      body = newBody,
      companion = newCompanion
    )
  }

  def optimizeObjectImplicits(objDef: SObjectDef): SObjectDef = objDef

  def optimizeComponentImplicits(t: SEntityDef)(implicit context: AstContext): SEntityDef = t match {
    case c: SClassDef => optimizeClassImplicits(c)
    case o: SObjectDef => optimizeObjectImplicits(o)
    case t: STraitDef => optimizeTraitImplicits(t)
  }

  def canBeExtracted(args: List[SMethodOrClassArg], tyName: String)(implicit context: AstContext) = {
    val res = args.exists(a => STpePath.find(a.tpe, tyName).isDefined)
    res
  }

  def optimizeClassImplicits(c: SClassDef)(implicit ctx: AstContext): SClassDef = {
    if (c.args.args.isEmpty) c
    else {
      val newArgs = c.implicitArgs.args.filter { _ match {
        case TypeDescArg(_,tyName) =>
          val explicitArgs = c.args.args
          !canBeExtracted(explicitArgs, tyName)
        case _ => true
      }}
      c.copy(
        implicitArgs = SClassArgs(newArgs)
      )
    }
  }

  def optimizeModuleImplicits(module: SUnitDef): SUnitDef = {
    implicit val ctx = module.context
    val newEntities = module.traits.map(e => optimizeTraitImplicits(e))
    val newClasses = module.classes.map(c => optimizeClassImplicits(c))
    module.copy(
      traits = newEntities,
      classes = newClasses
    )
  }
}