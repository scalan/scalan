package scalan.meta

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scalan.meta.Base.!!!
import scalan.meta.PrintExtensions._
import scalan.meta.ScalanAst._
import scalan.util.{Serialization, StringUtil, ScalaNameUtil}

class ModuleFileGenerator(val codegen: MetaCodegen, module: SModuleDef, config: CodegenConfig) {
  import codegen._

  val e = EntityTemplateData(module, module.entityOps)

  def getCompanionMethods(e: EntityTemplateData) = e.entity.companion.filter(_ => e.optBaseType.isDefined).map { comp =>
    val externalConstrs = comp.getMethodsWithAnnotation(ConstructorAnnotation)
    val externalMethods = comp.getMethodsWithAnnotation(ExternalAnnotation)
    (externalConstrs, externalMethods)
  }

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

  def externalConstructor(method: SMethodDef) = {
    def genConstr(md: SMethodDef) = {
      val allArgs = md.allArgs
      s"""
        |    ${md.declaration(config, false)} =
        |      newObjEx[${e.typeUse}](${allArgs.rep(_.name)})
        |""".stripMargin
    }

    genConstr(method.copy(argSections = method.cleanedArgs))
  }

  def entityProxy(e: EntityTemplateData) = {
    val entityName = e.name
    val typesDecl = e.tpeArgsDecl
    s"""
      |  // entityProxy: single proxy for each type family
      |  implicit def proxy$entityName${typesDecl}(p: Rep[${e.typeUse}]): ${e.typeUse} = {
      |    proxyOps[${e.typeUse}](p)(scala.reflect.classTag[${e.typeUse}])
      |  }
      |""".stripAndTrim
  }

  def getSClassExp(clazz: SClassDef) = {
    val c = ConcreteClassTemplateData(module, clazz)
    import c._
    val fields = clazz.args.argNames
    val fieldsWithType = clazz.args.argNamesAndTypes(config)
    val parent = clazz.ancestors.head
    val b = c.extractionBuilder()

    s"""
      |${methodExtractorsString(c.module, config, clazz)}
      |
      |  def mk${c.typeDecl}
      |    (${fieldsWithType.rep()})${implicitArgsDecl("", !b.isExtractable(_))}: Rep[${c.typeUse}] = {
      |    ${b.extractableImplicits}
      |    new ${c.typeUse("Ctor")}(${fields.rep()})
      |  }
      |  def unmk${c.typeDecl}(p: Rep[$parent]) = p.elem.asInstanceOf[Elem[_]] match {
      |    case _: ${c.elemTypeUse} @unchecked =>
      |      Some((${fields.rep(f => s"p.asRep[${c.typeUse}].$f")}))
      |    case _ =>
      |      None
      |  }
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
    s"""
      |    case view1@View${e.name}(Def(view2@View${e.name}(arr, innerIso2)), innerIso1) =>
      |      val compIso = composeIso(innerIso1, innerIso2)
      |      implicit val eAB = compIso.eTo
      |      View${e.name}(arr, compIso)
      |
         |    case ${e.name}Methods.map(xs, f) => (xs, f) match {
      |      case (_, Def(IdentityLambda())) =>
      |        xs
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

  def emitContainerRewriteDef(e: EntityTemplateData) = {
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
      |    ${e.when(_.isCont, emitContRules)}
      |    case _ => super.rewriteDef(d)
      |  }
      """.stripMargin
  }

  def emitFileHeader = {
    s"""
      |package ${module.packageName}
      |
      |${(module.imports ++ config.extraImports.map(SImportStat(_))).distinct.rep(i => s"import ${i.name}", "\n")}
      |
      |""".stripAndTrim
  }

  def emitTraitDefs(e: EntityTemplateData) = {
    val entityCompOpt = e.entity.companion
    val hasCompanion = entityCompOpt.isDefined
    val proxyBT = e.optBaseType.opt { bt =>
      s"""
        |  //proxyBT: TypeWrapper proxy
        |  //implicit def proxy${e.baseTypeName}${e.typesWithElems}(p: Rep[${e.baseTypeUse}]): ${e.typeUse} =
        |  //  proxyOps[${e.typeUse}](p.asRep[${e.typeUse}])
        |
         |  implicit def unwrapValueOf${e.typeDecl}(w: Rep[${e.typeUse}]): Rep[${e.baseTypeUse}] = w.wrappedValue
        |""".stripAndTrim
    }
    // note: currently can't cache them properly due to cyclical dependency between
    // baseType elem and wrapper elem
    val baseTypeElem = e.optBaseType.opt { bt =>
      // hack to work around arrayElement being defined in Scalan
      // and arrays being wrapped in scalanizer-demo
      val overrideOpt = if (e.baseInstanceName == "Array") "override " else ""
      val defOrVal = if (e.tpeArgs.isEmpty) "lazy val" else "def"
      val declaration =
        s"implicit $overrideOpt$defOrVal ${StringUtil.lowerCaseFirst(bt.name)}Element${e.typesWithElems}: Elem[${e.baseTypeUse}]"
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
            s"""!!!("Operation is not supported by $name container " + fa)"""
        }
          |    def unapply[T](e: Elem[_]) = e match {
          |      ${
          if (isWrapper)
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
        Some(s"      implicit val tag${tpe.toIdentifier} = ${arg.name}.tag")
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
        case Some(parent@STraitCall(parentName, parentTpeArgs)) =>
          (Some(parent), s"${parentName}Elem[${join(parentTpeArgs, toArgName)}]")
        case Some(p) => !!!(s"Unsupported parent type $p of the entity ${e.name}")
        case None => !!!(s"Entity ${e.name} must extend Def, TypeWrapper, or another entity")
      }
      val overrideIfHasParent = optParent.ifDefined("override ")
      val elemMethodName = entityElemMethodName(e.name)
      val elemMethodDefinition = {
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
        |      x.elem${e.t.isHighKind.opt(".asInstanceOf[Elem[_]]")} match {
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
        |    ${entityCompOpt.opt(_ => "")}
        |  }
        |${
        hasCompanion.opt
        s"""
          |  implicit def proxy${e.companionAbsName}(p: Rep[${e.companionAbsName}]): ${e.companionAbsName} =
          |    proxyOps[${e.companionAbsName}](p)
          |""".stripAndTrim
      }
        |""".stripAndTrim
    }

    val subEntities = for {entity <- module.entities.drop(1)} yield {
      val templateData = EntityTemplateData(module, entity)

      s"""
        |${entityProxy(templateData)}
        |${familyElem(templateData)}
        |
         |${companionAbs(templateData)}
        |""".stripMargin
    }
    val concreteClasses = for {clazz <- module.concreteSClasses} yield {
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
          val externalMethods = e.entity.getMethodsWithAnnotation(ExternalAnnotation)
          val externalMethodsStr = externalMethods.rep(md => externalMethod(md), "\n    ")
          val implicitArgsWithVals = c.implicitArgsDecl("val ")
          s"""
            |  // default wrapper implementation
            |  abstract class ${e.name}Impl${tpeArgsDecl}(val wrappedValue: Rep[${e.baseTypeUse}])${implicitArgsWithVals} extends ${e.typeUse} with Def[${e.name}Impl${tpeArgsDecl}] {
            |    lazy val selfType = element[${e.name}Impl${tpeArgsDecl}]
            |    $externalMethodsStr
            |  }
            |  case class ${e.name}ImplCtor${tpeArgsDecl}(override val wrappedValue: Rep[${e.baseTypeUse}])${implicitArgsWithVals} extends ${e.name}Impl${tpeArgsUse}(${fields.rep()}) {
            |  }
            |  trait ${e.name}ImplCompanion
            |""".stripAndTrim
        case Some(_) => ""
        case None =>
          s"""
            |  case class ${c.typeDecl("Ctor")}
            |      (${fieldsWithType.rep(f => s"override val $f")})${implicitArgsDecl}
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
        |    override def getDefaultRep = $className(${fieldTypes.rep(zeroSExpr(e.entity)(_))})
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
        |    override def toString = "${className}Companion"
        |${
        (fields.length != 1).opt({
          val s = c.entity.args.args.zipWithIndex.map { case (a, i) => a.name -> s"p._${i + 1}" }.toMap
          val sb = c.extractionBuilder(s)
          s"""
            |    @scalan.OverloadId("fromData")
            |    def apply${tpeArgsDecl}(p: Rep[$dataTpe])${c.implicitArgsDecl("", !sb.isExtractable(_))}: Rep[${c.typeUse}] = {
            |      ${sb.extractableImplicits}
            |      iso$className${c.tpeArgNames.opt(ns => s"[${ns.rep()}]")}.to(p)
            |    }
            """.stripAndTrim
        })
      }
        |    @scalan.OverloadId("fromFields")
        |    def apply${tpeArgsDecl}(${fieldsWithType.rep()})${val b = c.extractionBuilder(); c.implicitArgsDecl("", !b.isExtractable(_))}: Rep[${c.typeUse}] =
        |      mk$className(${fields.rep()})
        |
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
        |    protected def getDefaultRep = ${className}Rep
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
         |""".stripAndTrim
    }
    val companionMethods = getCompanionMethods(e).opt { case (constrs, methods) =>
      constrs.rep(md => externalConstructor(md), "\n    ") +
        methods.rep(md => externalMethod(md), "\n    ")
    }
    val extractorsForTraits = (for {entity <- module.entities.drop(1)} yield {
      methodExtractorsString(module, config, entity)
    }).mkString("\n\n")

    def companionExp(e: EntityTemplateData) = {
      s"""
        |  lazy val ${e.name}: Rep[${e.companionAbsName}] = new ${e.companionAbsName} {
        |    $companionMethods
        |  }
       """.stripMargin
    }

    val companionExpString = (for {entity <- module.entities} yield {
      val e = EntityTemplateData(module, entity)
      companionExp(e)
    }).mkString("\n\n")

    s"""
      |// Abs -----------------------------------
      |trait ${module.name}Defs extends ${config.baseContextTrait.opt(t => s"$t with ")}${module.name} {
      |  ${module.selfTypeString("")}
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
      |${companionAbs(e)}
      |
      |${subEntities.mkString("\n\n")}
      |
      |${concreteClasses.mkString("\n\n")}
      |
      |  registerModule(${module.name}_Module)
      |
      |$companionExpString
      |
      |${e.when(_.isCont, familyView)}
      |
      |$extractorsForTraits
      |
      |${module.concreteSClasses.map(getSClassExp).mkString("\n\n")}
      |
      |${methodExtractorsString(module, config, e.entity)}
      |${e.when(_.isCont, emitContainerRewriteDef)}
      |}
      |""".stripAndTrim
  }

  def emitModuleSerialization = {
    s"""
      |object ${module.name}_Module extends scalan.ModuleInfo {
      |  val dump = "${Serialization.save(module.clean)}"
      |}
       """.stripMargin
  }

  def emitDslTraits = {
    List(
      List((module.hasDsl, "", "Defs"))).flatten.collect {
      case (hasDslTrait, dslTraitSuffix, traitSuffix) if !hasDslTrait =>
        val DslName = module.name + "Dsl"
        val selfTypeStr = module.selfType match {
          case Some(SSelfTypeDef(_, List(STraitCall(DslName, Nil)))) => ""
          case _ => s" {${module.selfTypeString(dslTraitSuffix)}}"
        }
        s"trait $DslName$dslTraitSuffix extends impl.${module.name}$traitSuffix$selfTypeStr"
    }.mkString("\n")
  }

  def emitImplFile: String = {
    val topLevel = List(
      emitFileHeader,
      "package impl {",
      emitTraitDefs(e),
      emitModuleSerialization,
      "}",
      emitDslTraits
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
