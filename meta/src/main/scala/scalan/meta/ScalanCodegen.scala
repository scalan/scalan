package scalan.meta

import scala.annotation.tailrec
import scalan.meta.Base.!!!
import scalan.meta.PrintExtensions._
import scalan.meta.ScalanAst._
import scalan.util.{ScalaNameUtil, Serialization, StringUtil}

class MetaCodegen extends ScalanAstExtensions {

  abstract class TemplateData(val module: SEntityModuleDef, val entity: STraitOrClassDef) {
    val name = entity.name
    val tpeArgs = entity.tpeArgs
    val tpeArgNames = tpeArgs.names
    val tpeArgsDecl = tpeArgs.declString
    val tpeArgsUse = tpeArgs.useString
    val typeDecl = name + tpeArgsDecl
    val typeUse = name + tpeArgsUse
    val implicitArgs = entity.implicitArgs.args
    def implicitArgsDecl(prefix: String = "") =
      implicitArgs.opt(args => s"(implicit ${args.rep(a => s"$prefix${a.name}: ${a.tpe}")})")
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
    val baseTypeName = optBaseType.map(_.name).getOrElse(name)
    val baseTypeDecl = baseTypeName + tpeArgsDecl
    val baseTypeUse = baseTypeName + tpeArgsUse
    val firstAncestorType = entity.ancestors.headOption
    val entityRepSynonymOpt = module.entityRepSynonym
    val allArgs = entity.args.args ++ entity.implicitArgs.args

    def entityRepSynonym = entityRepSynonymOpt match {
      case Some(s) => s
      case None => STpeDef("Rep" + name, tpeArgs, STraitCall("Rep", List(STraitCall(name, tpeArgs.map(_.toTraitCall)))))
    }

    def isCont = tpeArgs.length == 1 && entity.hasAnnotation(ContainerTypeAnnotation)
    def isFunctor = tpeArgs.length == 1 && entity.hasAnnotation(FunctorTypeAnnotation)

    def isWrapper = firstAncestorType match {
      case Some(STraitCall("TypeWrapper", _)) => true
      case _ => false
    }

    def boundedTpeArgString(withTags: Boolean = false) = tpeArgs.getBoundedTpeArgString(withTags)

    def tpeSubst = tpeArgs.map { a =>
      val tyArgName = a.name
      val argNameOpt = allArgs.collectFirst(a => a.tpe match {
        case STraitCall("Elem", List(STpeAnnotated(STraitCall(`tyArgName`,_),_))) => Left(a.name)
        case STraitCall("Elem", List(STraitCall(`tyArgName`,_))) => Left(a.name)
        case STraitCall("Cont" | "Container", List(STraitCall(`tyArgName`,_))) => Right(a.name)
      })
      (tyArgName, argNameOpt)
    }

    def tpeSubstStr = tpeSubst.filter(_._2.isDefined).rep {
      case (n, Some(v)) => StringUtil.quote(n) + " -> " + v.fold(l => l, r => r)
      case (n, _) => !!!(s"No substitution for $n") // impossible due to filter above
    }

    def companionName = name + "Companion"
    def companionAbsName = name + "CompanionAbs"
  }

  case class EntityTemplateData(m: SEntityModuleDef, t: STraitDef) extends TemplateData(m, t) {
    def elemTypeUse(toType: String = typeUse) = s"${name}Elem[${join(tpeArgNames, toType)}]"
  }

  case class ConcreteClassTemplateData(m: SEntityModuleDef, c: SClassDef) extends TemplateData(m, c) {
    val elemTypeUse = name + "Elem" + tpeArgsUse
  }
}

object ScalanCodegen extends MetaCodegen

class EntityFileGenerator(val codegen: MetaCodegen, module: SEntityModuleDef, config: CodegenConfig) {
  import codegen._

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

  private val entity = module.entityOps

  def zeroSExpr(t: STpeExpr): String = t match {
    case STpePrimitive(_, defaultValueString) => defaultValueString
    case STraitCall(name, args)
      if entity.tpeArgs.exists(a => a.name == name && a.isHighKind) =>
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
    case STpeTuple(items) => pairify(items.map(zeroSExpr))
    case STpeFunc(domain, range) => s"""constFun[$domain, $range](${zeroSExpr(range)})"""
    case t => throw new IllegalArgumentException(s"Can't generate zero value for $t")
  }

  def typeToIdentifier(t: STpeExpr): String = {
    def mkId(name: String, parts: Seq[STpeExpr]) =
      (name +: parts).mkString("_")

    t match {
      case STpePrimitive(name, _) => name
      case STraitCall(name, args) => mkId(name, args)
      case STpeTuple(items) => mkId("Tuple", items)
      //case STpeSum(items) => mkId("Sum", items)
      case STpeFunc(domain, range) => mkId("Func", Seq(domain, range))
      case STpeTypeBounds(lo, hi) => mkId("Bounds", Seq(lo, hi))
      case _ => t.name
    }
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
    case _ => sys.error(s"Don't know how to construct Elem for type $t")
  }

  val e = EntityTemplateData(module, entity)
//  import e.{baseTypeName, baseTypeUse, companionAbsName, companionName, optBaseType, typeDecl, typeUse}
  val typesWithElems = e.boundedTpeArgString(false)

  def getCompanionMethods = entity.companion.filter(_ => e.optBaseType.isDefined).map { comp =>
    val externalConstrs = comp.getMethodsWithAnnotation(ConstructorAnnotation)
    val externalMethods = comp.getMethodsWithAnnotation(ExternalAnnotation)
    (externalConstrs, externalMethods)
  }

  def filterByExplicitDeclaration(traitName: String, ms: List[SMethodDef]): List[SMethodDef] =
    module.stdDslImpls match {
      case Some(impl) =>
        ms.filterNot(impl.containsMethodDef(traitName, _))
      case None => ms
    }

  def methodArgsUse(md: SMethodDef) = md.argSections.rep(sec => {
    val inParens = sec.args.rep { a =>
      if (a.tpe.isTupledFunc)
        s"scala.Function.untupled(${a.name})"
      else if (a.isArgList)
        s"${a.name}: _*"
      else
        a.name
    }
    s"($inParens)"
  }, "")

  def externalMethod(md: SMethodDef) = {
    val msgExplicitRetType = "External methods should be declared with explicit type of returning value (result type)"
    lazy val msgRepRetType = s"Invalid method $md. External methods should have return type of type Rep[T] for some T."
    val allArgs = md.allArgs
    val returnType = md.tpeRes.getOrElse(!!!(msgExplicitRetType))
    val unreppedReturnType = returnType.unRep(module, config).getOrElse(!!!(msgRepRetType))
    val argClassesStr = allArgs.rep(_ => s", classOf[AnyRef]", "")
    val elemClassesStr = (for {
      a <- md.tpeArgs
      cb <- a.contextBound
    } yield s", classOf[$cb[${a.name}]]").mkString
    val finalArgClasses = argClassesStr + elemClassesStr
    val elemArgs = for {
      a <- md.tpeArgs
      cb <- a.contextBound
    } yield s"${if (cb == "Elem") "element" else "weakTypeTag"}[${a.name}]"
    val finalArgs =
      join(allArgs.rep(a => s"${a.name}.asInstanceOf[AnyRef]"), elemArgs)
    s"""
       |    ${md.declaration(config, md.body.isDefined)} =
       |      methodCallEx[$unreppedReturnType](self,
       |        this.getClass.getMethod("${md.name}"$finalArgClasses),
       |        List($finalArgs))
       |""".stripMargin
  }

// from Scalanizer:
//  def externalMethod(md: SMethodDef) = {
//    val msgExplicitRetType = "External methods should be declared with explicit type of returning value (result type)"
//    lazy val msgRepRetType = s"Invalid method $md. External methods should have return type of type Rep[T] for some T."
//    val allArgs = md.allArgs
//    val returnType = md.tpeRes.getOrElse(!!!(msgExplicitRetType))
//    val typesDecl = md.tpeArgs.getBoundedTpeArgString(false)
//    val unreppedReturnType = returnType.unRep(module, config).getOrElse(!!!(msgRepRetType))
//    val argClassesStr = allArgs.rep(a => s", classOf[AnyRef]", "")
//    val elemClassesStr = (for {
//      a <- md.tpeArgs
//      cb <- a.contextBound
//    } yield s", classOf[$cb[${a.name}]]").rep()
//    val elemArgs = (for {
//      a <- md.tpeArgs
//      cb <- a.contextBound
//    } yield s"${if (cb == "Elem") "element" else "weakTypeTag"}[${a.name}]")
//    val compoundArgs = !(allArgs.isEmpty || elemArgs.isEmpty)
//
//    s"""
//      |    ${if (md.body.isDefined) "override def" else "def"} ${md.name}$typesDecl${md.argSections.rep(methodArgSection(_), "")}: Rep[${unreppedReturnType.toString}] =
//      |      methodCallEx[$unreppedReturnType](self,
//      |        this.getClass.getMethod("${md.name}"$argClassesStr$elemClassesStr),
//      |        List(${allArgs.rep(a => s"${a.name}.asInstanceOf[AnyRef]")}${compoundArgs.opt(", ")}${elemArgs.rep()}))
//      |""".stripMargin
//  }

  def externalConstructor(md: SMethodDef) = {
    val allArgs = md.allArgs
    s"""
       |    ${md.declaration(config, false)} =
       |      newObjEx[${e.typeUse}](${allArgs.rep(_.name)})
       |""".stripMargin
  }

  def externalStdMethod(md: SMethodDef, isInstance: Boolean) = {
    val msgExplicitRetType = "External methods should be declared with explicit type of returning value (result type)"
    val tyRet = md.tpeRes.getOrElse(!!!(msgExplicitRetType))
    val methodNameAndArgs = md.name + md.tpeArgs.useString + methodArgsUse(md)

    val obj = if (isInstance) "wrappedValue" else e.baseTypeName

    val methodBody = {
      val methodCall = s"$obj.$methodNameAndArgs"
      tyRet.unRep(module, config) match {
        case Some(STraitCall(name, _)) if name == e.name =>
          s"${e.name}Impl($methodCall)"
        case _ =>
          methodCall
      }
    }

    s"""
       |    ${md.declaration(config, true)} =
       |      $methodBody
       """.stripMargin
  }

  def externalStdConstructor(md: SMethodDef) = {
    s"""
       |    ${md.declaration(config, true)} =
       |      ${e.name}Impl(new ${e.baseTypeUse}${methodArgsUse(md)})
       |""".stripMargin
  }

  def entityProxy(e: EntityTemplateData) = {
    val entityName = e.name
    val typesDecl = e.tpeArgsDecl
    s"""
       |  // single proxy for each type family
       |  implicit def proxy$entityName${typesDecl}(p: Rep[${e.typeUse}]): ${e.typeUse} = {
       |    proxyOps[${e.typeUse}](p)(scala.reflect.classTag[${e.typeUse}])
       |  }
       |""".stripAndTrim
  }

  def selfTypeString(suffix: String) =
    module.selfType.opt(t => s"self: ${t.tpe}${suffix} =>")

  private val classes = module.concreteSClasses

  def extraBody(entity: STraitOrClassDef): String = ""

  def extraTraitAbs: String = ""

  def getTraitAbs = {
    val entityCompOpt = entity.companion
    val hasCompanion = entityCompOpt.isDefined
    val proxyBT = e.optBaseType.opt { bt =>
      s"""
         |  // TypeWrapper proxy
         |  //implicit def proxy${e.baseTypeName}${typesWithElems}(p: Rep[${e.baseTypeUse}]): ${e.typeUse} =
         |  //  proxyOps[${e.typeUse}](p.asRep[${e.typeUse}])
         |
         |  implicit def unwrapValueOf${e.typeDecl}(w: Rep[${e.typeUse}]): Rep[${e.baseTypeUse}] = w.wrappedValue
         |""".stripAndTrim
    }

    // note: currently can't cache them properly due to cyclical dependency between
    // baseType elem and wrapper elem
    val baseTypeElem = e.optBaseType.opt { bt =>
      val defOrVal = if (e.tpeArgs.isEmpty) "lazy val" else "def"
      val declaration =
        s"implicit $defOrVal ${StringUtil.lowerCaseFirst(bt.name)}Element${typesWithElems}: Elem[${e.baseTypeUse}]"
      val wrapperElemType = if (e.isCont)
        "WrapperElem1[_, _, CBase, CW] forSome { type CBase[_]; type CW[_] }"
      else
        "WrapperElem[_, _]"
      s"""
         |  $declaration =
         |    element[${e.typeUse}].asInstanceOf[$wrapperElemType].baseElem.asInstanceOf[Elem[${e.baseTypeUse}]]
         |""".stripAndTrim
    }

    def familyCont(e: EntityTemplateData) = {
      def container(name: String, isFunctor: Boolean, isWrapper: Boolean) = {
        val contType = if (isFunctor) "Functor" else "Cont"
        s"""\n
            |  implicit lazy val container$name: $contType[$name] = new $contType[$name] {
            |    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[$name[A]]
            |    def lift[A](implicit evA: Elem[A]) = element[$name[A]]
            |    def unlift[A](implicit eFT: Elem[$name[A]]) =
            |      cast${e.name}Element(eFT${(!isWrapper).opt(s".asInstanceOf[Elem[${e.name}[A]]]")}).eA
            |    def getElem[A](fa: Rep[$name[A]]) = ${
          if (isWrapper)
            s"fa.selfType1"
          else
            s"""!!!("Operation is not supported by $name container " + fa)""" }
            |    def unapply[T](e: Elem[_]) = e match {
            |      ${if (isWrapper)
          s"case e: ${e.name}Elem[_,_] => Some(e.asElem[${e.name}[T]])"
        else
          s"case e: BaseTypeElem1[_,_,_] if e.wrapperElem.isInstanceOf[${e.name}Elem[_,_]] => Some(e.asElem[$name[T]])"
        }
            |      case _ => None
            |    }
            |    ${isFunctor.opt(s"def map[A:Elem,B:Elem](xs: Rep[$name[A]])(f: Rep[A] => Rep[B]) = xs.map(fun(f))")}
            |  }
           """.stripMargin
      }

      val entityElem = e.elemTypeUse()
      s"""
         |  implicit def cast${e.name}Element${e.tpeArgsDecl}(elem: Elem[${e.typeUse}]): $entityElem =
         |    elem.asInstanceOf[$entityElem]
         |
         |  ${e.optBaseType.opt(bt => container(bt.name, false, false))}
         |
         |  ${container(e.name, e.isFunctor, true)}
         |
         |  case class ${e.name}Iso[A, B](innerIso: Iso[A, B]) extends Iso1UR[A, B, ${e.name}] {
         |    lazy val selfType = new ConcreteIsoElem[${e.name}[A], ${e.name}[B], ${e.name}Iso[A, B]](eFrom, eTo).
         |      asInstanceOf[Elem[IsoUR[${e.name}[A], ${e.name}[B]]]]
         |    def cC = container[${e.name}]
         |    def from(x: Rep[${e.name}[B]]) = x.map(innerIso.fromFun)
         |    def to(x: Rep[${e.name}[A]]) = x.map(innerIso.toFun)
         |  }
         |
         |  def ${StringUtil.lowerCaseFirst(e.name)}Iso[A, B](innerIso: Iso[A, B]) =
         |    reifyObject(${e.name}Iso[A, B](innerIso)).asInstanceOf[Iso1[A, B, ${e.name}]]
         |""".stripAndTrim
    }

    def implicitTagsFromElems(t: TemplateData) = t.implicitArgs.flatMap(arg => arg.tpe match {
      case STraitCall(name, List(tpe)) if name == "Elem" =>
        Some(s"      implicit val tag${typeToIdentifier(tpe)} = ${arg.name}.tag")
      case _ => None
    }).mkString("\n")

    def familyElem(e: EntityTemplateData) = {
      val wildcardElem = s"${e.name}Elem[${Array.fill(e.tpeArgs.length + 1)("_").mkString(", ")}]"
      val toArgName = {
        // no point converting to set, since it's small and contains is unlikely to be called more than 2 times
        val takenNames = e.name :: e.tpeArgs.map(_.name)
        (Iterator.single("To") ++ Iterator.from(0).map("To" + _)).filterNot(takenNames.contains).next()
      }

      val elemTypeDecl = s"${e.name}Elem[${join(e.tpeArgs.decls, s"$toArgName <: ${e.typeUse}")}]"

      val (optParent, parentElem) = e.firstAncestorType match {
        case Some(STraitCall("Def", _)) =>
          val parentElem =
            if (e.isCont) {
              s"EntityElem1[${e.tpeArgsUse}, $toArgName, ${e.name}](${e.tpeArgNames.rep("e" + _)}, container[${e.name}])"
            } else {
              s"EntityElem[$toArgName]"
            }
          (None, parentElem)
        case Some(STraitCall("TypeWrapper", _)) =>
          val parentElem =
            if (e.isCont) {
              s"WrapperElem1[${join(e.tpeArgNames, toArgName, e.baseTypeName, e.name)}](${e.tpeArgNames.rep("_e" + _)}, container[${e.baseTypeName}], container[${e.name}])"
            } else {
              s"WrapperElem[${e.baseTypeUse}, $toArgName]"
            }
          (None, parentElem)
        case Some(parent @ STraitCall(parentName, parentTpeArgs)) =>
          (Some(parent), s"${parentName}Elem[${join(parentTpeArgs, toArgName)}]")
        case Some(p) => !!!(s"Unsupported parent type $p of the entity ${e.name}")
        case None => !!!(s"Entity ${e.name} must extend Def, TypeWrapper, or another entity")
      }
      val overrideIfHasParent = optParent.ifDefined("override ")
      val elemMethodDefinition = {
        val elemMethodName = entityElemMethodName(e.name)
        if (!module.methods.exists(_.name == elemMethodName)) {
          val elemType = e.elemTypeUse()
          val elemMethodBody =
            if (e.isWrapper) {
              s"""    elemCache.getOrElseUpdate(
                  |      (classOf[$elemType], ${e.implicitArgsUse.opt(x => s"Seq$x", "Nil")}),
                  |      new $elemType).asInstanceOf[Elem[${e.typeUse}]]"""
            } else
              s"    cachedElem[$elemType]${e.implicitArgsOrParens}"
          s"""
             |  implicit def $elemMethodName${e.tpeArgsDecl}${e.implicitArgsDecl()}: Elem[${e.typeUse}] =
             |$elemMethodBody
             |""".stripMargin
        } else ""
      }

      val baseTypeElem = e.optBaseType.opt { bt =>
        val thisElem = s"this.asInstanceOf[Elem[${e.typeUse}]]"
        if (e.isCont) {
          val elems = e.tpeArgNames.rep(ty => s"element[$ty]")
          s"""
             |    lazy val baseElem =
             |      new BaseTypeElem1[${join(e.tpeArgNames, e.baseTypeName, e.typeUse)}]($thisElem)(
             |        $elems, container[${bt.name}])
             |""".stripAndTrim
        } else {
          val weakTagsForTpeArgs = e.tpeArgNames.map { argName =>
            s"      implicit val w$argName = element[$argName].tag"
          }.mkString("\n")
          s"""
             |    lazy val baseElem = {
             |      $weakTagsForTpeArgs
             |      new BaseTypeElem[${e.baseTypeUse}, ${e.typeUse}]($thisElem)
             |    }
             |""".stripAndTrim
        }
      }

      val eTo =
        s"    lazy val eTo: Elem[_] = new ${e.name}ImplElem${e.tpeArgsUse}(iso${e.name}Impl${e.implicitArgsUse})${e.implicitArgsUse}"

      s"""
         |  // familyElem
         |  class $elemTypeDecl${e.implicitArgsDecl("_")}
         |    extends $parentElem {
         |${e.implicitArgs.opt(_.rep(a => s"    ${(e.entity.isInheritedDeclared(a.name, e.module)).opt("override ")}def ${a.name} = _${a.name}", "\n"))}
         |    ${overrideIfHasParent}lazy val parent: Option[Elem[_]] = ${optParent.opt(p => s"Some(${tpeToElement(p, e.tpeArgs)})", "None")}
         |    ${overrideIfHasParent}lazy val typeArgs = TypeArgs(${e.tpeSubstStr})
         |    override def isEntityType = true
         |    override lazy val tag = {
         |${implicitTagsFromElems(e)}
         |      weakTypeTag[${e.typeUse}].asInstanceOf[WeakTypeTag[$toArgName]]
         |    }
         |    override def convert(x: Rep[Def[_]]) = {
         |      implicit val e$toArgName: Elem[$toArgName] = this
         |      val conv = fun {x: Rep[${e.typeUse}] => convert${e.name}(x) }
         |      tryConvert(element[${e.typeUse}], this, x, conv)
         |    }
         |
         |    def convert${e.name}(x: Rep[${e.typeUse}]): Rep[$toArgName] = {
         |      x.selfType1${e.t.isHighKind.opt(".asInstanceOf[Elem[_]]")} match {
         |        case _: $wildcardElem => x.asRep[$toArgName]
         |        case e => !!!(s"Expected $$x to have $wildcardElem, but got $$e", x)
         |      }
         |    }
         |${e.isWrapper.opt(baseTypeElem)}
         |${e.isWrapper.opt(eTo)}
         |    override def getDefaultRep: Rep[$toArgName] = ???
         |  }
         |$elemMethodDefinition
         |""".stripAndTrim
    }

    val companionExtraBody = entityCompOpt.opt(extraBody)

    def companionAbs(e: EntityTemplateData) = {
      val hasCompanion = e.entity.companion.isDefined
      s"""
         |  implicit case object ${e.companionName}Elem extends CompanionElem[${e.companionAbsName}] {
         |    lazy val tag = weakTypeTag[${e.companionAbsName}]
         |    protected def getDefaultRep = ${e.name}
         |  }
         |
         |  abstract class ${e.companionAbsName} extends CompanionDef[${e.companionAbsName}]${hasCompanion.opt(s" with ${e.companionName}")} {
         |    def selfType = ${e.companionName}Elem
         |    override def toString = "${e.name}"
         |    $companionExtraBody
         |  }
         |  def ${e.name}: Rep[${e.companionAbsName}]
         |${hasCompanion.opt
            s"""
               |  implicit def proxy${e.companionAbsName}(p: Rep[${e.companionAbsName}]): ${e.companionAbsName} =
               |    proxyOps[${e.companionAbsName}](p)
               |""".stripAndTrim
          }
         |""".stripAndTrim
    }

    val subEntities = for { entity <- module.entities.drop(1) } yield {
      val templateData = EntityTemplateData(module, entity)

      s"""
         |${entityProxy(templateData)}
         |${familyElem(templateData)}
         |${extraBody(entity)}
         |${companionAbs(templateData)}
         |""".stripMargin
    }

    val concreteClasses = for { clazz <- classes } yield {
      val className = clazz.name
      val c = ConcreteClassTemplateData(module, clazz)
      import c.{implicitArgsOrParens, implicitArgsUse, tpeArgsDecl, tpeArgsUse}
      val fields = clazz.args.argNames
      val fieldsWithType = clazz.args.argNamesAndTypes(config)
      val fieldTypes = clazz.args.argUnrepTypes(module, config)
      val implicitArgsDecl = c.implicitArgsDecl()
      val parent = clazz.ancestors.head
      val parentTpeArgsStr = parent.tpeSExprs.rep()

      val elemTypeDecl = c.name + "Elem" + tpeArgsDecl

      lazy val defaultImpl = e.optBaseType match {
        case Some(bt) if className == s"${e.name}Impl" =>
          val externalMethods = entity.getMethodsWithAnnotation(ExternalAnnotation)
          val externalMethodsStr = externalMethods.rep(md => externalMethod(md), "\n    ")
          val implicitArgsWithVals = c.implicitArgsDecl("val ")
          s"""
             |  // default wrapper implementation
             |  abstract class ${e.name}Impl${tpeArgsDecl}(val wrappedValue: Rep[${e.baseTypeUse}])${implicitArgsWithVals} extends ${e.typeUse} with Def[${e.name}Impl${tpeArgsDecl}] {
             |    lazy val selfType = element[${e.name}Impl${tpeArgsDecl}]
             |    $externalMethodsStr
             |  }
             |  trait ${e.name}ImplCompanion
             |""".stripAndTrim
        case Some(_) => ""
        case None =>
          s"""
             |  abstract class Abs${c.typeDecl}
             |      (${fieldsWithType.rep()})${implicitArgsDecl}
             |    extends ${c.typeUse}(${fields.rep()})${clazz.selfType.opt(t => s" with ${t.tpe}")} with Def[${c.typeUse}] {
             |    lazy val selfType = element[${c.typeUse}]
             |  }
             |""".stripAndTrim
      }

      val eFrom = {
        val elemMethodName = StringUtil.lowerCaseFirst(className + "DataElem")
        if (module.methods.exists(_.name == elemMethodName))
          s"()($elemMethodName)"
        else {
          @tailrec
          def implElem(args: List[String])(str0: String): String = {
            val size = args.length
            if (size > 2) {
              val str = str0 + s"pairElement(element[${args(0)}], "
              implElem(args.drop(1))(str)
            }
            else {
              if (size > 1) str0 + s"pairElement(element[${args(0)}], element[${args(1)}])"
              else str0 + s"(element[${args(0)}])"
            }
          }
          val args = fieldTypes.map(_.toString)
          args.length match {
            case n if n >= 2 =>
              val impls = implElem(args)("")
              val sk = ")" * (n - 2)
              impls + sk
            case 1 => s"element[${args(0)}]"
            case 0 => "UnitElement"
          }
        }
      }

      val parentElem = tpeToElement(parent, c.tpeArgs)
      val hasCompanion = clazz.companion.isDefined
      val dataTpe = s"${className}Data$tpeArgsUse"
      val concreteElemSuperType = if (e.isCont)
        s"ConcreteElem1[${join(parentTpeArgsStr, dataTpe, c.typeUse, parent.name)}]"
      else
        s"ConcreteElem[$dataTpe, ${c.typeUse}]"

      def converterBody = {
        val entity = module.getEntity(parent.name)
        val entityFields = entity.getAvailableFields(module)
        val classFields = clazz.args.args.map(_.name)
        val missingFields = classFields.filterNot(entityFields.contains(_))
        if (missingFields.isEmpty) {
          val args = classFields.rep(f => s"x.$f")
          s"$className($args)"
        }
        else {
          val msg = s"from ${entity.name} to ${clazz.name}: missing fields $missingFields"
          s"""|// Converter is not generated by meta
              |!!!("Cannot convert $msg")""".stripMargin
        }
      }

      // TODO is the eTo override needed?

      // note: ${className}Iso.eTo doesn't call cachedElem because
      // they are already cached via Isos + lazy val and this would lead to stack overflow
      val isoProductArity = c.implicitArgs.length
      val isoProductElementBody = isoProductArity match {
        case 0 => "???"
        case 1 => c.implicitArgs(0).name
        case _ =>
          val cases = c.implicitArgs.zipWithIndex.map { case (arg, i) =>
            s"      case $i => ${arg.name}"
          }.mkString("\n")
          s"n match {\n$cases\n    }"
      }
      s"""
         |$defaultImpl
         |  // elem for concrete class
         |  class $elemTypeDecl(val iso: Iso[$dataTpe, ${c.typeUse}])${c.implicitArgsDeclConcreteElem}
         |    extends ${parent.name}Elem[${join(parentTpeArgsStr, c.typeUse)}]
         |    with $concreteElemSuperType {
         |    override lazy val parent: Option[Elem[_]] = Some($parentElem)
         |    override lazy val typeArgs = TypeArgs(${c.tpeSubstStr})
         |    ${e.isWrapper.opt("override lazy val eTo: Elem[_] = this")}
         |    override def convert${parent.name}(x: Rep[$parent]) = $converterBody
         |    override def getDefaultRep = $className(${fieldTypes.rep(zeroSExpr(_))})
         |    override lazy val tag = {
         |${implicitTagsFromElems(c)}
         |      weakTypeTag[${c.typeUse}]
         |    }
         |  }
         |
         |  // state representation type
         |  type ${className}Data${tpeArgsDecl} = ${dataType(fieldTypes)}
         |
         |  // 3) Iso for concrete class
         |  class ${className}Iso${tpeArgsDecl}${implicitArgsDecl}
         |    extends EntityIso[$dataTpe, ${c.typeUse}] with Def[${className}Iso$tpeArgsUse] {
         |    override def from(p: Rep[${c.typeUse}]) =
         |      ${fields.map(fields => "p." + fields).opt(s => if (s.toList.length > 1) s"(${s.rep()})" else s.rep(), "()")}
         |    override def to(p: Rep[${dataType(fieldTypes)}]) = {
         |      val ${pairify(fields)} = p
         |      $className(${fields.rep()})
         |    }
         |    lazy val eFrom = $eFrom
         |    lazy val eTo = new ${c.elemTypeUse}(self)
         |    lazy val selfType = new ${className}IsoElem$tpeArgsUse$implicitArgsUse
         |    def productArity = $isoProductArity
         |    def productElement(n: Int) = $isoProductElementBody
         |  }
         |  case class ${className}IsoElem${tpeArgsDecl}(${c.implicitArgs.rep(a => s"${a.name}: ${a.tpe}")}) extends Elem[${className}Iso$tpeArgsUse] {
         |    def isEntityType = true
         |    def getDefaultRep = reifyObject(new ${className}Iso${tpeArgsUse}()$implicitArgsUse)
         |    lazy val tag = {
         |${implicitTagsFromElems(c)}
         |      weakTypeTag[${className}Iso$tpeArgsUse]
         |    }
         |    lazy val typeArgs = TypeArgs(${c.tpeSubstStr})
         |  }
         |  // 4) constructor and deconstructor
         |  class ${c.companionAbsName} extends CompanionDef[${c.companionAbsName}]${hasCompanion.opt(s" with ${c.companionName}")} {
         |    def selfType = ${className}CompanionElem
         |    override def toString = "$className"
         |${(fields.length != 1).opt(s"""
                                        |    @scalan.OverloadId("fromData")
                                        |    def apply${tpeArgsDecl}(p: Rep[$dataTpe])${implicitArgsDecl}: Rep[${c.typeUse}] =
                                        |      iso$className${implicitArgsUse}.to(p)""".stripAndTrim)}
         |    @scalan.OverloadId("fromFields")
         |    def apply${tpeArgsDecl}(${fieldsWithType.rep()})${implicitArgsDecl}: Rep[${c.typeUse}] =
         |      mk$className(${fields.rep()})
         |    ${extraBody(clazz)}
         |    def unapply${tpeArgsDecl}(p: Rep[$parent]) = unmk$className(p)
         |  }
         |  lazy val ${c.name}Rep: Rep[${c.companionAbsName}] = new ${c.companionAbsName}
         |  lazy val ${c.name}: ${c.companionAbsName} = proxy${className}Companion(${c.name}Rep)
         |  implicit def proxy${className}Companion(p: Rep[${c.companionAbsName}]): ${c.companionAbsName} = {
         |    proxyOps[${c.companionAbsName}](p)
         |  }
         |
         |  implicit case object ${className}CompanionElem extends CompanionElem[${c.companionAbsName}] {
         |    lazy val tag = weakTypeTag[${c.companionAbsName}]
         |    protected def getDefaultRep = $className
         |  }
         |
         |  implicit def proxy${c.typeDecl}(p: Rep[${c.typeUse}]): ${c.typeUse} =
         |    proxyOps[${c.typeUse}](p)
         |
         |  implicit class Extended${c.typeDecl}(p: Rep[${c.typeUse}])$implicitArgsDecl {
         |    def toData: Rep[$dataTpe] = iso$className${implicitArgsUse}.from(p)
         |  }
         |
         |  // 5) implicit resolution of Iso
         |  implicit def iso${c.typeDecl}${implicitArgsDecl}: Iso[$dataTpe, ${c.typeUse}] =
         |    reifyObject(new ${className}Iso${tpeArgsUse}()$implicitArgsUse)
         |
         |  // 6) smart constructor and deconstructor
         |  def mk${c.typeDecl}(${fieldsWithType.rep()})${implicitArgsDecl}: Rep[${c.typeUse}]
         |  def unmk${c.typeDecl}(p: Rep[$parent]): Option[(${fieldTypes.opt(fieldTypes => fieldTypes.rep(t => s"Rep[$t]"), "Rep[Unit]")})]
         |""".stripAndTrim
    }

    s"""
       |// Abs -----------------------------------
       |trait ${module.name}Abs extends ${config.baseContextTrait.opt(t => s"$t with ")}${module.name} {
       |  ${selfTypeString("")}
       |
       |${entityProxy(e)}
       |
       |$proxyBT
       |
       |$baseTypeElem
       |
       |${if (e.isCont) familyCont(e) else ""}
       |
       |${familyElem(e)}
       |
       |$extraTraitAbs
       |
       |${companionAbs(e)}
       |
       |${subEntities.mkString("\n\n")}
       |
       |${concreteClasses.mkString("\n\n")}
       |
       |  registerModule(${module.name}_Module)
       |}
       |""".stripAndTrim
  }

  def getSClassStd(clazz: SClassDef) = {
    val c = ConcreteClassTemplateData(module, clazz)
    val fields = clazz.args.argNames
    val fieldsWithType = clazz.args.argNamesAndTypes(config)
    val implicitArgsDecl = c.implicitArgsDecl()

    val externalMethods = entity.getMethodsWithAnnotation(ExternalAnnotation)
    val externalMethodsStr = filterByExplicitDeclaration(c.name, externalMethods).rep(md => externalStdMethod(md, true), "\n")

    val parent     = clazz.ancestors.head

    s"""
       |  case class Std${c.typeDecl}
       |      (${fieldsWithType.rep(f => s"override val $f")})${implicitArgsDecl}
       |    extends ${e.optBaseType.isEmpty.opt("Abs")}${c.typeUse}(${fields.rep()})${module.hasStdImplFor(c.name).opt(s" with ${c.name}Decls${c.tpeArgsUse}")} {
       |$externalMethodsStr
       |  }
       |
       |  def mk${c.typeDecl}
       |    (${fieldsWithType.rep()})$implicitArgsDecl: Rep[${c.typeUse}] =
       |    new Std${c.typeUse}(${fields.rep()})
       |  def unmk${c.typeDecl}(p: Rep[$parent]) = p match {
       |    case p: ${c.typeUse} @unchecked =>
       |      Some((${fields.rep(f => s"p.$f")}))
       |    case _ => None
       |  }
       |""".stripAndTrim
  }

  def getSClassExp(clazz: SClassDef) = {
    val c = ConcreteClassTemplateData(module, clazz)
    import c._
    val fields = clazz.args.argNames
    val fieldsWithType = clazz.args.argNamesAndTypes(config)
    val parent     = clazz.ancestors.head

    s"""
       |  case class Exp${c.typeDecl}
       |      (${fieldsWithType.rep(f => s"override val $f")})${implicitArgsDecl()}
       |    extends ${e.optBaseType.isEmpty.opt("Abs")}${c.typeUse}(${fields.rep()})
       |    ${c.module.hasExpImplFor(c.name).opt(s" with ${c.name}Decls${c.tpeArgsUse}")}
       |
       |${methodExtractorsString(clazz)}
       |
       |  def mk${c.typeDecl}
       |    (${fieldsWithType.rep()})${implicitArgsDecl()}: Rep[${c.typeUse}] =
       |    new Exp${c.typeUse}(${fields.rep()})
       |  def unmk${c.typeDecl}(p: Rep[$parent]) = p.elem.asInstanceOf[Elem[_]] match {
       |    case _: ${c.elemTypeUse} @unchecked =>
       |      Some((${fields.rep(f => s"p.asRep[${c.typeUse}].$f")}))
       |    case _ =>
       |      None
       |  }
       |""".stripAndTrim
  }

  def getTraitStd = {
    val classesStd = classes.map(getSClassStd)
    val proxyBTStd = e.optBaseType.opt(bt =>
      s"""
         |  // override proxy if we deal with TypeWrapper
         |  //override def proxy${e.baseTypeName}${typesWithElems}(p: Rep[${e.baseTypeUse}]): ${e.typeUse} =
         |  //  proxyOpsEx[${e.baseTypeUse}, ${e.typeUse}, Std${e.name}Impl${e.tpeArgsUse}](p, bt => Std${e.name}Impl(bt))
         |""".stripAndTrim
    )
    val baseTypeToWrapperConvertionStd = e.optBaseType.opt(bt =>
      s"""
         |  implicit def wrap${e.baseTypeName}To${e.name}${typesWithElems}(v: ${e.baseTypeUse}): ${e.typeUse} = ${e.name}Impl(v)
         |""".stripAndTrim
    )

    val companionMethods = getCompanionMethods.opt { case (constrs, methods) =>
      filterByExplicitDeclaration(e.name, constrs).rep(md => externalStdConstructor(md), "\n") +
        filterByExplicitDeclaration(e.name, methods).filter(_.body.isEmpty).rep(md => externalStdMethod(md, false), "\n")
    }

    def companionStd(e: EntityTemplateData) = {
      s"""
        |  lazy val ${e.name}: Rep[${e.companionAbsName}] = new ${e.companionAbsName} {
        |    $companionMethods
        |  }
       """.stripMargin
    }

    val companionStdString  = (for { entity <- module.entities } yield {
      val e = EntityTemplateData(module, entity)
      companionStd(e)
    }).mkString("\n\n")

    s"""
       |// Std -----------------------------------
       |trait ${module.name}Std extends ${config.seqContextTrait.opt(t => s"$t with ")}${module.name}Dsl {
       |  ${selfTypeString("Std")}
       |
       |${companionStdString}
       |
       |$proxyBTStd
       |
       |${classesStd.mkString("\n\n")}
       |
       |$baseTypeToWrapperConvertionStd
       |}
       |""".stripAndTrim
  }

  def familyView(e: EntityTemplateData) = {
    s"""
       |  case class View${e.name}[A, B](source: Rep[${e.name}[A]], override val innerIso: Iso[A, B])
       |    extends View1[A, B, ${e.name}](${StringUtil.lowerCaseFirst(e.name)}Iso(innerIso)) {
       |    override def toString = s"View${e.name}[$${innerIso.eTo.name}]($$source)"
       |    override def equals(other: Any) = other match {
       |      case v: View${e.name}[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
       |      case _ => false
       |    }
       |  }
       |""".stripAndTrim
  }

  def emitContRules(e: EntityTemplateData) = {
    if (e.isCont) {
      s"""
         |    case ${e.name}Methods.map(xs, Def(IdentityLambda())) => xs
         |
         |    case view1@View${e.name}(Def(view2@View${e.name}(arr, innerIso2)), innerIso1) =>
         |      val compIso = composeIso(innerIso1, innerIso2)
         |      implicit val eAB = compIso.eTo
         |      View${e.name}(arr, compIso)
         |
         |    // Rule: W(a).m(args) ==> iso.to(a.m(unwrap(args)))
         |    case mc @ MethodCall(Def(wrapper: Exp${e.name}Impl[_]), m, args, neverInvoke) if !isValueAccessor(m) =>
         |      val resultElem = mc.selfType
         |      val wrapperIso = getIsoByElem(resultElem)
         |      wrapperIso match {
         |        case iso: Iso[base,ext] =>
         |          val eRes = iso.eFrom
         |          val newCall = unwrapMethodCall(mc, wrapper.wrappedValue, eRes)
         |          iso.to(newCall)
         |      }
         |
         |    case ${e.name}Methods.map(xs, f) => (xs, f) match {
         |      case (xs: ${e.entityRepSynonym.name}[a] @unchecked, LambdaResultHasViews(f, iso: Iso[b, c])) =>
         |        val f1 = f.asRep[a => c]
         |        implicit val eB = iso.eFrom
         |        val s = xs.map(f1 >> iso.fromFun)
         |        val res = View${e.name}(s, iso)
         |        res
         |      case (HasViews(source, Def(contIso: ${e.name}Iso[a, b])), f: RFunc[_, c]@unchecked) =>
         |        val f1 = f.asRep[b => c]
         |        val iso = contIso.innerIso
         |        implicit val eC = f1.elem.eRange
         |        source.asRep[${e.name}[a]].map(iso.toFun >> f1)
         |      case _ =>
         |        super.rewriteDef(d)
         |    }""".stripMargin
    }
    else ""
  }

  def emitRewriteDef(e: EntityTemplateData) = {
    if (e.isCont) {
      s"""
         |  object UserType${e.name} {
         |    def unapply(s: Exp[_]): Option[Iso[_, _]] = {
         |      s.elem match {
         |        case e: ${e.name}Elem[a,to] => e.eItem match {
         |          case UnpackableElem(iso) => Some(iso)
         |          case _ => None
         |        }
         |        case _ => None
         |      }
         |    }
         |  }
         |
         |  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
         |    case Def(view: View${e.name}[_, _]) =>
         |      Some((view.source, view.iso))
         |    case UserType${e.name}(iso: Iso[a, b]) =>
         |      val newIso = ${StringUtil.lowerCaseFirst(e.name)}Iso(iso)
         |      val repr = reifyObject(UnpackView(s.asRep[${e.name}[b]], newIso))
         |      Some((repr, newIso))
         |    case _ =>
         |      super.unapplyViews(s)
         |  }).asInstanceOf[Option[Unpacked[T]]]
         |
         |  ${e.entityRepSynonymOpt.isEmpty.opt(e.entityRepSynonym.declaration)}
         |
         |  override def rewriteDef[T](d: Def[T]) = d match {
         |    ${emitContRules(e)}
         |    case _ => super.rewriteDef(d)
         |  }
         """.stripMargin
    }
    else ""
  }

  def getTraitExp = {
    val companionMethods = getCompanionMethods.opt { case (constrs, methods) =>
      constrs.rep(md => externalConstructor(md), "\n    ") +
        methods.rep(md => externalMethod(md), "\n    ")
    }

    val extractorsForTraits = (for { entity <- module.entities.drop(1) } yield {
      methodExtractorsString(entity)
    }).mkString("\n\n")

    val concreteClassesString = classes.map(getSClassExp)

    def companionExp(e: EntityTemplateData) = {
      s"""
        |  lazy val ${e.name}: Rep[${e.companionAbsName}] = new ${e.companionAbsName} {
        |    $companionMethods
        |  }
       """.stripMargin
    }

    val companionExpString  = (for { entity <- module.entities } yield {
      val e = EntityTemplateData(module, entity)
      companionExp(e)
    }).mkString("\n\n")

    s"""
       |// Exp -----------------------------------
       |trait ${module.name}Exp extends ${config.stagedContextTrait.opt(t => s"$t with ")}${module.name}Dsl {
       |  ${selfTypeString("Exp")}
       |
       |$companionExpString
       |
       |${if (e.isCont) familyView(e) else ""}
       |
       |$extractorsForTraits
       |
       |${concreteClassesString.mkString("\n\n")}
       |
       |${methodExtractorsString(entity)}
       |${emitRewriteDef(e)}
       |}
       |""".stripAndTrim
  }

  def methodExtractorsString(e: STraitOrClassDef) = {
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
        (m.explicitArgs.filter { arg => arg.tpe.isInstanceOf[STpeFunc] } match {
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
              //println(s"    WARNING: Cannot generate matcher for method `${e.name}.${m.name}`: $reason")
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
                val receiverType = s"Rep[${e.name + typeArgString(e.tpeArgs.map(_.name))}]"
                val argTypes = methodArgs.map{ arg =>
                    if (config.isAlreadyRep || arg.isElemOrCont)
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
            }
            val typeVars = (e.tpeArgs ++ m.tpeArgs).map(_.declaration).toSet
            val returnType = {
              val receiverType = s"Rep[${e.name + e.tpeArgs.asTypeParams(_.name)}]"
              val argTypes =
                if (config.isAlreadyRep)
                  methodArgs.map(_.tpe.toString)
                else
                  methodArgs.map(a => s"Rep[${a.tpe}]")
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

  def getFileHeader = {
    s"""
       |package ${module.packageName}
       |
      |${(module.imports ++ config.extraImports.map(SImportStat(_))).distinct.rep(i => s"import ${i.name}", "\n")}
       |
      |package impl {
       |
      |""".stripAndTrim
  }

  def getDslTraits = {
    Seq(
      (module.hasDsl, "", "Abs"),
      (module.hasDslStd, "Std", "Std"),
      (module.hasDslExp, "Exp", "Exp")).collect {
      case (hasDslTrait, dslTraitSuffix, traitSuffix) if !hasDslTrait =>
        val DslName = s"${module.name}Dsl"
        val selfTypeStr = module.selfType match {
          case Some(SSelfTypeDef(_, List(STraitCall(DslName, Nil)))) => ""
          case _ => s" {${selfTypeString(dslTraitSuffix)}}"
        }
        s"trait $DslName$dslTraitSuffix extends impl.${module.name}$traitSuffix$selfTypeStr"
    }.mkString("\n")
  }

  def emitModuleSerialization = {
    s"""
       |object ${module.name}_Module extends scalan.ModuleInfo {
       |  val dump = "${Serialization.save(module.clean)}"
       |}
       """.stripMargin
  }

  def getImplFile: String = {
    val topLevel = List(
      getFileHeader,
      getTraitAbs,
      getTraitStd,
      getTraitExp,
      emitModuleSerialization,
      "}", // closing brace for `package impl {`
      getDslTraits
    )
    topLevel.mkString("", "\n\n", "\n").
      // clean empty lines
      replaceAll(""" +\r?\n""", "\n").
      // remove 3 or more consecutive linebreaks
      replaceAll("""(\r?\n){3,}""", "\n\n").
      // remove empty lines before and after braces
      replaceAll("""\r?\n\r?\n( *\})""", "\n$1").
      replaceAll("""( *\{)\r?\n\r?\n""", "$1\n")
  }
}
