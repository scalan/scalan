package scalan.meta

import scalan.meta.PrintExtensions._
import scala.collection.mutable.ArrayBuffer
import scalan.util.{StringUtil, ScalaNameUtil}
import scalan.meta.ScalanAst._

class MetaCodegen extends ScalanAstExtensions {

  def dataType(ts: List[STpeExpr]): String = ts match {
    case Nil => "Unit"
    case t :: Nil => t.toString
    case t :: ts => s"($t, ${dataType(ts)})"
  }

  def pairify(fs: List[String]): String = fs match {
    case Nil => "unit"
    case f :: Nil => f
    case f :: fs => s"Pair($f, ${pairify(fs)})"
  }

  def zeroSExpr(entity: STraitOrClassDef)(t: STpeExpr): String = t match {
    case STpePrimitive(_, defaultValueString) => defaultValueString
    case STraitCall(name, args) if entity.tpeArgs.exists(a => a.name == name && a.isHighKind) =>
      val arg = args(0)
      arg match {
        case STraitCall(name2, _) if entity.tpeArgs.exists(a => a.name == name2) =>
          s"c$name.lift(${args.rep(a => s"e$a")}).defaultRepValue"
        case _ =>
          s"c$name.lift(${args.rep(a => s"element[$a]")}).defaultRepValue"
      }
    case tc@STraitCall(name, args) => {
      val isBT = entity.optBaseType.exists(bt => bt.name == name)
      if (isBT) {
        s"DefaultOf${tc.name + tc.tpeSExprs.asTypeParams()}"
      } else {
        s"element[$t].defaultRepValue"
      }
    }
    case STpeTuple(items) => pairify(items.map(zeroSExpr(entity)))
    case STpeFunc(domain, range) => s"""constFun[$domain, $range](${zeroSExpr(entity)(range)})"""
    case t => throw new IllegalArgumentException(s"Can't generate zero value for $t")
  }

  def entityElemMethodName(name: String) = StringUtil.lowerCaseFirst(name) + "Element"

  // TODO remove this hack
  private[this] val highOrderTpes = Set("Array", "List", "ArrayBuffer", "Thunk", "Collection", "Seq")

  def tpeToElement(t: STpeExpr, env: List[STpeArg]): String = t match {
    case STpePrimitive(name,_) => name + "Element"
    case STpeTuple(List(a, b)) => s"pairElement(${tpeToElement(a, env)},${tpeToElement(b, env)})"
    case STpeFunc(a, b) => s"funcElement(${tpeToElement(a, env)},${tpeToElement(b, env)})"
    case STraitCall("$bar", List(a,b)) => s"sumElement(${tpeToElement(a, env)},${tpeToElement(b, env)})"
    case STraitCall(name, Nil) if STpePrimitives.contains(name) => name + "Element"
    case STraitCall(name, Nil) if highOrderTpes.contains(name) =>
      s"container[$name]"
    case STraitCall(name, args) if env.exists(_.name == name) =>
      val a = env.find(_.name == name).get
      if (!a.isHighKind)
        s"element[$t]"
      else
      if (args.isEmpty)
        s"container[$t]"
      else
        s"element[$t]"
    case STraitCall(name, args) =>
      val method = entityElemMethodName(name)
      val argsStr = args.rep(tpeToElement(_, env))
      method + args.nonEmpty.opt(s"($argsStr)")
    case _ => sys.error(s"Don't know how to construct Elem string for type $t")
  }

  class EntityTypeBuilder(entity: STraitOrClassDef) {
    private var _args: ArrayBuffer[String] = ArrayBuffer.empty
    private var _forSomeTypes: ArrayBuffer[String] = ArrayBuffer.empty

    {
      var iHKind = 1
      for (a <- entity.tpeArgs) {
        if (a.isHighKind) {
          val tyName = "F" + iHKind
          _args += tyName
          _forSomeTypes += s"type $tyName[_]"
          iHKind += 1
        }
        else {
          _args += "_"
        }
      }
    }

    def elemTypeName = {
      s"${entity.name}Elem[${_args.rep()}]${_forSomeTypes.opt(ts => s"forSome {${ts.rep(_.toString, ";")}}") }"
    }
  }

  def emitImplicitElemDeclByTpePath(prefixExpr: String, tailPath: STpePath) = {
    def emit(prefix: String, tailPath: STpePath, typed: Boolean): String = tailPath match {
      case SNilPath => prefix
      case STuplePath(_, i, t) => i match {
        case 0 =>
          if (typed)
            emit(s"$prefix.eFst", t, typed)
          else
            emit(s"$prefix.asInstanceOf[PairElem[_,_]].eFst", t, typed)
        case 1 =>
          if (typed)
            emit(s"$prefix.eSnd", t, typed)
          else
            emit(s"$prefix.asInstanceOf[PairElem[_,_]].eSnd", t, typed)
        case _ =>
          sys.error(s"Unsupported tuple type ($prefix, $tailPath)")
      }
      case SDomPath(_, t) =>
        if (typed)
          emit(s"$prefix.eDom", t, typed)
        else
          emit(s"$prefix.asInstanceOf[FuncElem[_,_]].eDom", t, typed)
      case SRangePath(_, t) =>
        if (typed)
          emit(s"$prefix.eRange", t, typed)
        else
          emit(s"$prefix.asInstanceOf[FuncElem[_,_]].eRange", t, typed)
      case SStructPath(_, fn, t) =>
        emit(s"""$prefix.asInstanceOf[StructElem[_]]("$fn")""", t, false)
      case SEntityPath(STraitCall(name, args), e, tyArgName, t) =>
        val argIndex = e.tpeArgs.zipWithIndex.find { case (a, i) => a.name == tyArgName }.get._2
        val argTy = args(argIndex)
        val kind = if (e.tpeArgs(argIndex).isHighKind) "Cont" else "Elem"
        emit(s"""$prefix.typeArgs("$tyArgName")._1.as$kind[$argTy]""", t, true)
      case _ => sys.error(s"emit($tailPath)")
    }
    emit(prefixExpr, tailPath, true)
  }

  /** Build element extraction expression for each type argument.
    * @param m the module we are working in
    * @param dataArgs data arguments which we can use to extract elements from
    * @param tpeArgs the type arguments for which elements should be extracted
    * @return a list with either Some(element extraction expression) or None for some type arguments
    */
  def extractImplicitElems(m: SModuleDef, dataArgs: List[SClassArg], tpeArgs: List[STpeArg], argSubst: Map[String, String] = Map()): List[Option[String]] = {
    def subst(arg: String) = argSubst.getOrElse(arg, arg)
    tpeArgs.map { ta =>
      val paths = for {
            da <- dataArgs
            argTpe <- PartialFunction.condOpt(da.tpe) { case STraitCall("Rep", List(argTpe)) => argTpe }
            path <- STpePath.find(m, argTpe, ta.name)
          } yield (da, path)

      if (paths.nonEmpty) {
        val (da, path) = paths.head
        val expr = emitImplicitElemDeclByTpePath(s"${subst(da.name)}.elem", path)
        Some(expr)
      }
      else {
        // the element cannot be extracted from data
        None
      }
    }
  }

  def methodExtractorsString(module: SModuleDef, config: CodegenConfig, e: STraitOrClassDef) = {
    def methodExtractorsString1(e: STraitOrClassDef, isCompanion: Boolean) = {
      val methods = e.body.collect { case m: SMethodDef => m}
      val overloadIdsByName = collection.mutable.Map.empty[String, Set[Option[String]]].withDefaultValue(Set())
      methods.foreach { m =>
        val methodName = m.name
        val overloadId = m.overloadId
        val overloadIds = overloadIdsByName(methodName)
        if (overloadIds.contains(overloadId)) {
          sys.error(s"Duplicate overload id for method ${e.name}.$methodName: ${overloadId}. Use scalan.OverloadId annotation with different values for each overload (one overload can be unannotated).")
        } else {
          overloadIdsByName(methodName) = overloadIds + overloadId
        }
      }

      def reasonToSkipMethod(m: SMethodDef): Option[String] = {
        (m.explicitArgs.filter { arg => arg.tpe.isInstanceOf[STpeFunc] && config.isAlreadyRep } match {
          case Seq() => None
          case nonEmpty => Some(s"Method has function arguments ${nonEmpty.rep(_.name)}")
        }).orElse {
          m.name match {
            case "toString" | "hashCode" if m.allArgs.isEmpty =>
              Some("Overrides Object method")
            case "equals" | "canEqual" if m.allArgs.length == 1 =>
              Some("Overrides Object method")
            case _ => None
          }
        }.orElse {
          m.tpeRes.filter(!_.isRep(module, config)).map {
            returnTpe => s"Method's return type $returnTpe is not a Rep"
          }
        }
      }

      def methodExtractor(m: SMethodDef) = {
        reasonToSkipMethod(m) match {
          case Some(reason) =>
            s"    // WARNING: Cannot generate matcher for method `${m.name}`: $reason"
          case _ =>
            // DummyImplicit and Overloaded* are ignored, since
            // their values are never useful
            val methodArgs = m.allArgs.takeWhile { arg =>
              arg.tpe match {
                case STraitCall(name, _) =>
                  !(name == "DummyImplicit" || name.startsWith("Overloaded"))
                case _ => true
              }
            }
            val typeVars = (e.tpeArgs ++ m.tpeArgs).map(_.declaration).toSet
            val returnType = {
              val receiverType = s"Rep[${e.name + e.tpeArgs.asTypeParams(_.name)}]"
              val argTypes = methodArgs.map { arg =>
                if (config.isAlreadyRep || arg.isTypeDesc)
                  arg.tpe.toString
                else
                  "Rep[" + arg.tpe.toString + "]"
              }
              val receiverAndArgTypes = ((if (isCompanion) Nil else List(receiverType)) ++ argTypes) match {
                case Seq() => "Unit"
                case Seq(single) => single
                case many => many.mkString("(", ", ", ")")
              }
              s"Option[$receiverAndArgTypes${typeVars.opt(typeVars => s" forSome {${typeVars.map("type " + _).mkString("; ")}}")}]"
            }
            val overloadId = m.overloadId
            val cleanedMethodName = ScalaNameUtil.cleanScalaName(m.name)
            val matcherName = {
              overloadId match {
                case None => cleanedMethodName
                case Some(id) =>
                  // make a legal identifier containing overload id
                  if (ScalaNameUtil.isOpName(cleanedMethodName)) {
                    id + "_" + cleanedMethodName
                  } else {
                    cleanedMethodName + "_" + id
                  }
              }
            }

            val matchResult = ((if (isCompanion) Nil else List("receiver")) ++ methodArgs.map(_.name)) match {
              case Seq() => "()"
              case Seq(single) => single
              case many => many.mkString("(", ", ", ")")
            }

            val methodPattern = {
              // _* is for dummy implicit arguments
              val methodArgsPattern = if (methodArgs.isEmpty) "_" else s"Seq(${methodArgs.rep(_.name)}, _*)"
              val typeArgsNum =
                if (isCompanion) {
                  0
                } else if (e.isInstanceOf[STraitDef]) {
                  e.tpeArgs.length + 1
                } else {
                  e.tpeArgs.length
                }
              val traitElem = s"${e.name}Elem${Seq.fill(typeArgsNum)("_").asTypeParams()}"
              val annotationCheck =
                if (overloadIdsByName(m.name).size == 1) {
                  // nothing to check if method isn't overloaded
                  ""
                } else {
                  overloadId match {
                    case None =>
                      " && method.getAnnotation(classOf[scalan.OverloadId]) == null"
                    case Some(id) =>
                      s""" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "$id" }"""
                  }
                }

              val elemCheck = if (isCompanion) {
                s"receiver.elem == $traitElem"
              } else if (e.isHighKind) {
                // same as isInstanceOf[$traitElem], but that won't compile
                s"(receiver.elem.asInstanceOf[Elem[_]] match { case _: $traitElem => true; case _ => false })"
              } else {
                s"receiver.elem.isInstanceOf[$traitElem]"
              }
              s"""MethodCall(receiver, method, $methodArgsPattern, _) if $elemCheck && method.getName == "${m.name}"$annotationCheck"""
            }
            // TODO we can use name-based extractor to improve performance when we switch to Scala 2.11
            // See http://hseeberger.github.io/blog/2013/10/04/name-based-extractors-in-scala-2-dot-11/

            s"""    object $matcherName {
              |      def unapply(d: Def[_]): $returnType = d match {
              |        case $methodPattern =>
              |          Some($matchResult).asInstanceOf[$returnType]
              |        case _ => None
              |      }
              |      def unapply(exp: Exp[_]): $returnType = exp match {
              |        case Def(d) => unapply(d)
              |        case _ => None
              |      }
              |    }""".stripAndTrim
        }
      }

      s"""  object ${e.name}Methods {
        |${methods.filterNot(_.isTypeDesc).map(methodExtractor).mkString("\n\n")}
        |  }""".stripMargin
    }

    s"""${methodExtractorsString1(e, false)}
      |
      |${e.companion.opt(methodExtractorsString1(_, true))}""".stripMargin
  }

  abstract class TemplateData(val module: SModuleDef, val entity: STraitOrClassDef) {
    val name = entity.name
    val tpeArgs = entity.tpeArgs
    val tpeArgNames = tpeArgs.names
    val tpeArgsDecl = tpeArgs.declString
    val tpeArgsUse = tpeArgs.useString
    val typeDecl = name + tpeArgsDecl
    def typeDecl(suffix: String) = name + suffix + tpeArgsDecl
    val typeUse = name + tpeArgsUse
    def typeUse(suffix: String) = name + suffix + tpeArgsUse
    val implicitArgs = entity.implicitArgs.args
    def implicitArgsDecl(prefix: String = "", p: SClassArg => Boolean = _ => true) =
      implicitArgs.filter(p).opt(args => s"(implicit ${args.rep(a => s"$prefix${a.name}: ${a.tpe}")})")
    def implicitArgsDeclConcreteElem = {
      implicitArgs.opt(args => s"(implicit ${args.rep(a => {
                                 val declared = entity.isInheritedDeclared(a.name, module)
                                 val defined = entity.isInheritedDefined(a.name, module)
                                 s"${(declared || defined).opt("override ")}val ${a.name}: ${a.tpe}"
                                           })})")
    }
    val implicitArgsUse = implicitArgs.opt(args => s"(${args.rep(_.name)})")
    val implicitArgsOrParens = if (implicitArgs.nonEmpty) implicitArgsUse else "()"
    val optBaseType = entity.optBaseType
//    val baseTypeName = optBaseType.map(_.name).getOrElse(name)
    val baseTypeName = entity.baseTypeName
    val baseInstanceName = entity.baseInstanceName
    val baseTypeDecl = baseTypeName + tpeArgsDecl
    val baseTypeUse = baseTypeName + tpeArgsUse
    val firstAncestorType = entity.firstAncestorType
    val entityRepSynonymOpt = module.entityRepSynonym
    val allArgs = entity.args.args ++ entity.implicitArgs.args

    def entityRepSynonym = entityRepSynonymOpt match {
      case Some(s) => s
      case None => STpeDef("Rep" + name, tpeArgs, STraitCall("Rep", List(STraitCall(name, tpeArgs.map(_.toTraitCall)))))
    }

    def isCont = tpeArgs.length == 1 && entity.hasAnnotation(ContainerTypeAnnotation)
    def isFunctor = tpeArgs.length == 1 && entity.hasAnnotation(FunctorTypeAnnotation)
    def isWrapper = entity.isWrapper

    def boundedTpeArgString(withTags: Boolean = false) = tpeArgs.getBoundedTpeArgString(withTags)

    def tpeSubstStr = tpeArgs.flatMap { tpeArg =>
      val tyArgName = tpeArg.name
      val argOpt = allArgs.find { a =>
        a.tpe match {
          case STraitCall("Elem", List(STpeAnnotated(STraitCall(`tyArgName`, _), _))) => true
          case STraitCall("Elem", List(STraitCall(`tyArgName`, _))) => true
          case STraitCall("Cont", List(STraitCall(`tyArgName`, _))) => true
          case _ => false
        }
      }
      argOpt.map { arg =>
        s"${StringUtil.quote(tyArgName)} -> (${arg.name} -> scalan.util.${tpeArg.variance})"
      }
    }.rep()

    def companionName = name + "Companion"
    def companionAbsName = name + "CompanionCtor"
  }

  case class EntityTemplateData(m: SModuleDef, t: STraitDef) extends TemplateData(m, t) {
    def elemTypeUse(toType: String = typeUse) = s"${name}Elem[${join(tpeArgNames, toType)}]"
    val typesWithElems = boundedTpeArgString(false)
  }

  case class ConcreteClassTemplateData(m: SModuleDef, c: SClassDef) extends TemplateData(m, c) {
    val elemTypeUse = name + "Elem" + tpeArgsUse

    // methods to extract elements from data arguments
    class ExtractionBuilder(argSubst: Map[String, String]) {
      val extractionExprs = extractImplicitElems(m, c.args.args, c.tpeArgs, argSubst)
      val extractableArgs: Map[String,(STpeArg, String)] = c.tpeArgs.zip(extractionExprs)
        .collect { case (arg, Some(expr)) => (arg.name, (arg, expr)) }.toMap
      def isExtractable(a: SClassArg): Boolean = a.tpe match {
        case STraitCall("Elem", List(STraitCall(tyArgName, Nil))) =>
          extractableArgs.contains(tyArgName)
        case STraitCall("Cont", List(STraitCall(tyArgName, Nil))) =>
          extractableArgs.contains(tyArgName)
        case _ => false
      }
      val extractableImplicits = {
        extractableArgs.map { case (tyArgName, (arg, expr)) =>
          val kind = if (arg.isHighKind) "c" else "e"
          s"implicit val $kind$tyArgName = $expr"
        }.mkString(";\n")
      }
    }

    def extractionBuilder(argSubst: Map[String, String] = Map()) = new ExtractionBuilder(argSubst)
  }
}

object ScalanCodegen extends MetaCodegen
