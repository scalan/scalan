package scalan.meta

import java.lang.annotation.Annotation

import com.fasterxml.jackson.annotation.JsonSubTypes.Type
import com.fasterxml.jackson.annotation.JsonTypeInfo.{Id, As}
import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}
import com.typesafe.config.ConfigUtil

import scala.reflect.internal.ModifierFlags
import PrintExtensions._
import scala.collection.mutable.{Map => MMap}
import scalan._
import scalan.util.{Covariant, Contravariant, Invariant}
import scalan.util.CollectionUtil._
import ScalanAstExtensions._

object ScalanAst {

  // STpe universe --------------------------------------------------------------------------

  /** Type expressions */
  @JsonTypeInfo(use = Id.NAME, include = As.PROPERTY, property = "type")
  @JsonSubTypes(Array(
    new Type(value = classOf[STpeEmpty], name = "STpeEmpty"),
    new Type(value = classOf[STpeConst], name = "STpeConst"),
    new Type(value = classOf[STpePrimitive], name = "STpePrimitive"),
    new Type(value = classOf[STpeSingleton], name = "STpeSingleton"),
    new Type(value = classOf[STpeAnnotated], name = "STpeAnnotated"),
    new Type(value = classOf[STpeStruct], name = "STpeStruct"),
    new Type(value = classOf[STpeSelectFromTT], name = "STpeSelectFromTT"),
    new Type(value = classOf[STpeCompound], name = "STpeCompound"),
    new Type(value = classOf[STpeMethod], name = "STpeMethod"),
    new Type(value = classOf[STpeBind], name = "STpeBind"),
    new Type(value = classOf[STpeThis], name = "STpeThis"),
    new Type(value = classOf[STpeTuple], name = "STpeTuple"),
    new Type(value = classOf[STpeExistential], name = "STpeExistential"),
    new Type(value = classOf[STpeSingle], name = "STpeSingle"),
    new Type(value = classOf[STpeFunc], name = "STpeFunc"),
    new Type(value = classOf[STraitCall], name = "STraitCall"),
    new Type(value = classOf[STpeTypeBounds], name = "STpeTypeBounds")
  ))
  sealed abstract class STpeExpr {
    def name: String

    def tpeSExprs: List[STpeExpr] = Nil
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
  case class STraitCall(val name: String, override val tpeSExprs: List[STpeExpr] = Nil) extends STpeExpr {
    override def toString = name + tpeSExprs.asTypeParams()
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
    "Any" -> STpePrimitive("Any", "AnyElement.defaultRepValue"),
    "AnyRef" -> STpePrimitive("AnyRef", "AnyRefElement.defaultRepValue"),
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

  case class STpeTuple(override val tpeSExprs: List[STpeExpr]) extends STpeExpr {
    def name = "Tuple" + tpeSExprs.length

    override def toString = tpeSExprs.mkString("(", ", ", ")")
  }

  case class STpeStruct(fields: List[(String, STpeExpr)]) extends STpeExpr {
    def name = "Struct"

    override def toString = fields.map { case (n, t) => s"$n: $t" }.mkString("{", "; ", "}")
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
          case None => STraitCall(n, args map {
            _.applySubst(subst)
          })
        }
      case STpeTuple(items) => STpeTuple(items map {
        _.applySubst(subst)
      })
      case _ => self
    }

    def unRep(module: SModuleDef, isVirtualized: Boolean): Option[STpeExpr] = self match {
      case t if !isVirtualized => Some(t)
      case STraitCall("Elem", Seq(t)) =>  // Elem[t] --> self
        Some(self)
      case STraitCall("Rep", Seq(t)) =>   // Rep[t] --> t
        Some(t)
      case STraitCall("RFunc", Seq(a, b)) =>  // RFunc[a,b] --> a => b
        Some(STpeFunc(a, b))
      case module.TypeSynonim(entityName, tyargs) => // RepCol[args] --> Col[args]
        Some(STraitCall(entityName, tyargs))
//      case STraitCall(name, args) =>
//        def withoutPrefix(prefix: String): Option[String] = {
//          val indexAfterPrefix = prefix.length
//          if (name.startsWith(prefix) && name(indexAfterPrefix).isUpper)
//            Some(name.substring(indexAfterPrefix))
//          else
//            None
//        }
//        // handle cases like RSeg, RepInt, IntRep
//        val unReppedName = withoutPrefix("R").orElse(withoutPrefix("Rep").orElse(Some(name.stripSuffix("Rep")))).filter(_ != name)
      case _ => None
    }

    def isRep(module: SModuleDef, isVirtualized: Boolean) = unRep(module, isVirtualized) match {
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

  case class SEntityPath(base: STpeExpr, entity: STraitOrClassDef, tyArg: STpeArg, tail: STpePath) extends SBasedPath

  object STpePath {
    def findInEntity(module: SModuleDef, e: STraitOrClassDef,
                     tc: STraitCall, argName: String): Option[STpePath] = {
      val args = tc.tpeSExprs
      for (i <- args.indices) {
        find(module, args(i), argName) match {
          case Some(tailPath) =>
            return Some(SEntityPath(tc, e, e.tpeArgs(i), tailPath))
          case None =>
        }
      }
      None
    }

    def find(module: SModuleDef, tpe: STpeExpr, argName: String): Option[STpePath] = tpe match {
      case STpePrimitive(_, _) => None
      case STpeFunc(d, r) =>
        find(module, d, argName) match {
          case Some(tailPath) => Some(SDomPath(tpe, tailPath))
          case None => find(module, r, argName) match {
            case Some(tailPath) => Some(SRangePath(tpe, tailPath))
            case None => None
          }
        }
      case t@STpeTuple(_) =>
        def findInTuple(t: STpeTuple): Option[STpePath] = {
          for ((item, i) <- t.tpeSExprs.zipWithIndex) {
            find(module, item, argName) match {
              case Some(tailPath) =>
                return Some(STuplePath(t, i, tailPath))
              case None =>
            }
          }
          None
        }
        findInTuple(t)
      case STraitCall("Rep", List(tT)) =>
        find(module, tT, argName)
      case STraitCall("Thunk", List(tT)) =>
        find(module, tT, argName).map(tail => SThunkPath(tpe, tail))
      case module.TypeSynonim(en @ module.Entity(e), args) =>
        findInEntity(module, e, STraitCall(en, args), argName)
      case s@STpeStruct(_) =>
        def findInStruct(s: STpeStruct): Option[STpePath] = {
          for ((fn, ft) <- s.fields) {
            find(module, ft, argName) match {
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
      case tc@STraitCall(module.Entity(e), args) =>
        findInEntity(module, e, tc, argName)
      case tc@STraitCall(module.WrapperEntity(e, _), args) =>
        findInEntity(module, e, tc, argName)
      case _ => None
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

  case class STypeArgAnnotation(annotationClass: String, args: List[SExpr]) extends SAnnotation

  final val ConstructorAnnotation    = classOf[Constructor].getSimpleName
  final val ExternalAnnotation       = classOf[External].getSimpleName
  final val ArgListAnnotation        = classOf[ArgList].getSimpleName
  final val ContainerTypeAnnotation  = classOf[ContainerType].getSimpleName
  final val FunctorTypeAnnotation    = classOf[FunctorType].getSimpleName
  final val ReifiedTypeArgAnnotation = classOf[Reified].getSimpleName

  // SExpr universe --------------------------------------------------------------------------
  @JsonTypeInfo(use = Id.NAME, include = As.PROPERTY, property = "type")
  @JsonSubTypes(Array(
    new Type(value = classOf[SEmpty], name = "SEmpty"),
    new Type(value = classOf[SConst], name = "SConst"),
    new Type(value = classOf[SIdent], name = "SIdent"),
    new Type(value = classOf[SAssign], name = "SAssign"),
    new Type(value = classOf[SApply], name = "SApply"),
    new Type(value = classOf[SExprApply], name = "SExprApply"),
    new Type(value = classOf[SCase], name = "SCase"),
    new Type(value = classOf[SSuper], name = "SSuper"),
    new Type(value = classOf[SFunc], name = "SFunc"),
    new Type(value = classOf[STuple], name = "STuple"),
    new Type(value = classOf[SMatch], name = "SMatch"),
    new Type(value = classOf[SAscr], name = "SAscr"),
    new Type(value = classOf[STypeApply], name = "STypeApply"),
    new Type(value = classOf[SBlock], name = "SBlock"),
    new Type(value = classOf[SSelect], name = "SSelect"),
    new Type(value = classOf[SIf], name = "SIf"),
    new Type(value = classOf[SAnnotated], name = "SAnnotated"),
    new Type(value = classOf[SValDef], name = "SValDef"),
    new Type(value = classOf[SObjectDef], name = "SObjectDef"),
    new Type(value = classOf[SClassDef], name = "SClassDef"),
    new Type(value = classOf[STraitDef], name = "STraitDef"),
    new Type(value = classOf[STpeDef], name = "STpeDef"),
    new Type(value = classOf[SImportStat], name = "SImportStat"),
    new Type(value = classOf[SMethodDef], name = "SMethodDef"),
    new Type(value = classOf[SThis], name = "SThis"),
    new Type(value = classOf[SContr], name = "SContr"),
    new Type(value = classOf[SLiteral], name = "SLiteral")
  ))
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

  type Module = SModuleDef

  abstract class STraitOrClassDef extends SBodyItem {
    def name: String

    def tpeArgs: List[STpeArg]

    def ancestors: List[STypeApply]

    def body: List[SBodyItem]

    def selfType: Option[SSelfTypeDef]

    def companion: Option[STraitOrClassDef]

    def isTrait: Boolean

    def annotations: List[STraitOrClassAnnotation]

    def args: SClassArgs

    def implicitArgs: SClassArgs

    def tpeArgIndex(tpeArgName: String) = {
      tpeArgs.zipWithIndex.find { case (a, i) => a.name == tpeArgName }.get._2
    }

    def firstAncestorType = ancestors.headOption.map(_.tpe)

    def isWrapper = firstAncestorType match {
      case Some(TypeWrapperTpe(_)) => true
      case _ => false
    }

    def isHighKind = tpeArgs.exists(_.isHighKind)

    def isInheritedDeclared(propName: String, module: SModuleDef) = {
      getInheritedDeclaredFields(module).contains(propName)
    }

    def isInheritedDefined(propName: String, module: SModuleDef) = {
      getInheritedDefinedFields(module).contains(propName)
    }

    def getMethodsWithAnnotation(annClass: String) = body.collect {
      case md: SMethodDef if md.annotations.exists(a => a.annotationClass == annClass) => md
    }

    def getFieldDefs: List[SMethodDef] = body.collect {
      case md: SMethodDef if md.allArgs.isEmpty => md
    }

    def getAncestorTraits(module: SModuleDef): List[STraitOrClassDef] = {
      ancestors.filter(a => module.isEntity(a.tpe.name)).map(a => module.getEntity(a.tpe.name))
    }

    def getAvailableFields(module: SModuleDef): Set[String] = {
      getFieldDefs.map(_.name).toSet ++ getAncestorTraits(module).flatMap(_.getAvailableFields(module))
    }

    def getAvailableMethodDefs(module: SModuleDef): Seq[SMethodDef] = {
      getFieldDefs ++ getAncestorTraits(module).flatMap(_.getAvailableMethodDefs(module))
    }

    def getInheritedMethodDefs(module: SModuleDef): Seq[SMethodDef] = {
      getAncestorTraits(module).flatMap(_.getAvailableMethodDefs(module))
    }

    def getInheritedDeclaredFields(module: SModuleDef): Set[String] = {
      getInheritedMethodDefs(module).collect { case md if md.body.isEmpty => md.name }.toSet
    }

    def getInheritedDefinedFields(module: SModuleDef): Set[String] = {
      getInheritedMethodDefs(module).collect { case md if md.body.isDefined => md.name }.toSet
    }

    def getConcreteClasses = body.collect {
      case c: SClassDef if !c.hasAnnotation("InternalType") => c
    }

    def getDeclaredElems(module: SModuleDef): List[(String, STpeExpr)] = {
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

  object TypeWrapperTpe {
    def apply(wrappedType: STraitCall, wrapperType: STraitCall) =
      STraitCall(TypeWrapperDefName, List(wrappedType, wrapperType))
    def unapply(tc: STraitCall): Option[STraitCall] = tc match {
      case STraitCall(TypeWrapperDefName, List(w@STraitCall(_, _), _)) => Some(w)
      case _ => None
    }
  }

  /** Extracts first argument of TypeWrapperDef from ancestors */
  object WrapperEntity {
    def unapply(e: STraitOrClassDef): Option[STraitCall] =
      e.ancestors.map(_.tpe).collectFirst { case TypeWrapperTpe(w) => w }
  }

  case class STraitDef(
                        name: String,
                        tpeArgs: List[STpeArg],
                        ancestors: List[STypeApply],
                        body: List[SBodyItem],
                        selfType: Option[SSelfTypeDef],
                        companion: Option[STraitOrClassDef],
                        annotations: List[STraitOrClassAnnotation] = Nil) extends STraitOrClassDef {
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

  final val TypeWrapperDefName = "TypeWrapperDef"

  implicit class STraitOrClassDefOps(td: STraitOrClassDef) {
    def optBaseType: Option[STraitCall] = WrapperEntity.unapply(td)

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
                        ancestors: List[STypeApply],
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
                         ancestors: List[STypeApply],
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

  class AstContext(val configs: List[CodegenConfig]) {

    /** Mapping of external type names to their wrappers. */
    val wrappers = MMap[String, WrapperDescr]()

    val modules = MMap[String, SModuleDef]()

    /** The types that shouldn't be Rep[].
      * For example List("Elem", "Cont", "ClassTag") */
    val typeClasses = Set("Elem", "Cont", "ClassTag", "Functor")

    def isEntity(name: String): Boolean = {
      val res = for (m <- modules.values; e <- m.entities if e.name == name) yield ()
      res.nonEmpty
    }
    def isEntityCompanion(name: String): Boolean = {
      val res = for (m <- modules.values; e <- m.entities; c <- e.companion if c.name == name) yield ()
      res.nonEmpty
    }
    def isClass(name: String): Boolean = {
      val res = for (m <- modules.values; c <- m.concreteSClasses if c.name == name) yield ()
      res.nonEmpty
    }
    def isClassCompanion(name: String): Boolean = {
      val res = for (m <- modules.values; c <- m.concreteSClasses; comp <- c.companion if comp.name == name) yield ()
      res.nonEmpty
    }
    def isModule(name: String): Boolean = {
      modules.valuesIterator.map(_.name).toSet.contains(name)
    }

    def allModules: Iterator[SModuleDef] = wrappers.valuesIterator.map(_.module) ++ modules.valuesIterator

    def findEntityInOtherModules(entityName: String, referencedInModule: SModuleDef): Option[(SModuleDef, STraitOrClassDef)] = {
      allModules collectFirst scala.Function.unlift { m =>
        if (m.name == referencedInModule.name) None
        else m.findEntity(entityName, globalSearch = false).map { e => (m, e) }
      }
    }
    def entityTypeSynonyms: Map[String, String] = {
      allModules
        .flatMap(m => m.entityRepSynonym.map((m, _)))
        .map { case (m, syn) => syn.name -> m.entityOps.name }
        .toMap
    }

    def getModule(packageName: String, unitName: String): SModuleDef = {
      val key = s"$packageName.$unitName"
      modules(key)
    }

    def addModule(unitName: String, module: SModuleDef) = {
      val key = s"${module.packageName}.$unitName"
      modules(key) = module
    }
  }

  case class SModuleDef(packageName: String,
                        imports: List[SImportStat],
                        name: String,
                        entityRepSynonym: Option[STpeDef],
                        entityOps: STraitDef,
                        entities: List[STraitDef],
                        concreteSClasses: List[SClassDef],
                        methods: List[SMethodDef],
                        selfType: Option[SSelfTypeDef],
                        ancestors: List[STypeApply],
                        origModuleTrait: Option[STraitDef],  // original module trait declared
                        isVirtualized: Boolean,
                        okEmitOrigModuleTrait: Boolean = true)
                       (@transient implicit val context: AstContext) {
    def getModuleTraitName: String = SModuleDef.moduleTraitName(name)
    def getFullName(shortName: String): String = s"$packageName.$name.$shortName"

    def isEqualName(shortName: String, fullName: String): Boolean =
      if (fullName == getFullName(shortName)) true
      else shortName == fullName

    def findEntity(entityName: String, globalSearch: Boolean = false): Option[STraitOrClassDef] = {
      def isEqualName(shortName: String, fullName: String): Boolean =
        fullName == shortName || fullName == s"$packageName.$entityName.$shortName"

      def findByName(es: List[STraitOrClassDef]) =
        es.find(e => isEqualName(e.name, entityName))

      val local = findByName(entities)
        .orElse(findByName(concreteSClasses))
      val res = local.orElse {
        if (globalSearch)
          context.findEntityInOtherModules(entityName, this).map(_._2)
        else None
      }
      res
    }

    def findWrapperEntity(wrappedTypeName: String): Option[(STraitOrClassDef, STraitCall)] = {
      entities.collectFirst {
        case e@ScalanAst.WrapperEntity(w) if w.name == wrappedTypeName => (e, w)
      }
    }

    def getEntity(name: String): STraitOrClassDef = {
      findEntity(name, globalSearch = true).getOrElse {
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
        origModuleTrait = None,
        ancestors = Nil
      )
    }

    def printAst(ast: SModuleDef): Unit = {
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

    object Entity {
      def unapply(name: String): Option[STraitOrClassDef] = findEntity(name, globalSearch = true)
    }

    object WrapperEntity {
      def unapply(name: String): Option[(STraitOrClassDef, STraitCall)] = findWrapperEntity(name)
    }

    object TypeSynonim {
      def unapply(tpe: STpeExpr): Option[(String, List[STpeExpr])] = tpe match {
        case STraitCall(n, args) =>
          val typeSynonyms = context.entityTypeSynonyms ++ entityRepSynonym.map(syn => syn.name -> entityOps.name)
          typeSynonyms.get(n).map(entityName => (entityName, args))
        case _ => None
      }
    }

  }

  object SModuleDef {
    /** Module trait name related to main trait */
    def moduleTraitName(mainTraitName: String) = mainTraitName + "Module"
    def tpeUseExpr(arg: STpeArg): STpeExpr = STraitCall(arg.name, arg.tparams.map(tpeUseExpr(_)))
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

  case class WrapperDescr(module: SModuleDef, ownerChain: List[String], config: WrapperConfig)

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

  def optimizeMethodImplicits(m: SMethodDef, module: SModuleDef): SMethodDef = {
    val explicitArgs = m.explicitArgs
    val newSections = m.argSections.filterMap(as => {
      val newArgs = as.args.filter {
        case arg@TypeDescArg(_, tyName) if arg.impFlag =>
          !canBeExtracted(module, explicitArgs, tyName)
        case _ => true
      }
      if (newArgs.nonEmpty) Some(SMethodArgs(newArgs)) else None
    })
    m.copy(argSections = newSections)
  }

  def optimizeTraitImplicits(t: STraitDef, module: SModuleDef): STraitDef = {
    val newBody = t.body.map {
      case m: SMethodDef => optimizeMethodImplicits(m, module)
      case item => item
    }
    val newCompanion = t.companion.map(optimizeComponentImplicits(_, module))
    t.copy(
      body = newBody,
      companion = newCompanion
    )
  }

  def optimizeObjectImplicits(t: SObjectDef, module: SModuleDef): SObjectDef = {
    t
  }

  def optimizeComponentImplicits(t: STraitOrClassDef, module: SModuleDef): STraitOrClassDef = t match {
    case c: SClassDef => optimizeClassImplicits(c, module)
    case o: SObjectDef => optimizeObjectImplicits(o, module)
    case t: STraitDef => optimizeTraitImplicits(t, module)
  }

  def canBeExtracted(module: SModuleDef, args: List[SMethodOrClassArg], tyName: String) = {
    val res = args.exists(a => STpePath.find(module, a.tpe, tyName).isDefined)
    res
  }

  def optimizeClassImplicits(c: SClassDef, module: SModuleDef): SClassDef = {
    if (c.args.args.isEmpty) c
    else {
      val newArgs = c.implicitArgs.args.filter { _ match {
        case TypeDescArg(_,tyName) =>
          val explicitArgs = c.args.args
          !canBeExtracted(module, explicitArgs, tyName)
        case _ => true
      }}
      c.copy(
        implicitArgs = SClassArgs(newArgs)
      )
    }
  }

  def optimizeModuleImplicits(module: SModuleDef): SModuleDef = {
    val newEntities = module.entities.map(e => optimizeTraitImplicits(e, module))
    val newClasses = module.concreteSClasses.map(c => optimizeClassImplicits(c, module))
    module.copy(
      entityOps = newEntities(0),
      entities = newEntities,
      concreteSClasses = newClasses
    )(module.context)
  }
}