package scalan.meta

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectOutputStream}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import scala.annotation.tailrec
import scalan.meta.Base.!!!
import scalan.meta.PrintExtensions._
import scalan.meta.ScalanAst._
import scalan.util.{ScalaNameUtil, StringUtil, ThreadContextClassLoaderObjectInputStream}

object ScalanCodegen extends SqlCompiler with ScalanAstExtensions {

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
    val implicitArgsUse = implicitArgs.opt(args => s"(${args.rep(_.name)})")
    val implicitArgsOrParens = if (implicitArgs.nonEmpty) implicitArgsUse else "()"
    val optBaseType = entity.optBaseType
    val baseTypeName = optBaseType.map(_.name).getOrElse(name)
    val baseTypeDecl = baseTypeName + tpeArgsDecl
    val baseTypeUse = baseTypeName + tpeArgsUse
    val firstAncestorType = entity.ancestors.headOption
    val entityRepSynonymOpt = module.entityRepSynonym

    def entityRepSynonym = entityRepSynonymOpt match {
      case Some(s) => s
      case None => STpeDef("Rep" + name, tpeArgs, STraitCall("Rep", List(STraitCall(name, tpeArgs.map(_.toTraitCall)))))
    }

    def isContainer = tpeArgs.length == 1 && entity.hasAnnotation(ContainerTypeAnnotation)
    def isFunctor = tpeArgs.length == 1 && entity.hasAnnotation(FunctorTypeAnnotation)

    def isWrapper = firstAncestorType match {
      case Some(STraitCall("TypeWrapper", _)) => true
      case _ => false
    }

    def boundedTpeArgString(withTags: Boolean = false) = tpeArgs.getBoundedTpeArgString(withTags)

    def tpeSubst = tpeArgs.map { a =>
      val tyArgName = a.name
      val implicitArgNameOpt = implicitArgs.collectFirst(a => a.tpe match {
        case STraitCall("Elem",List(STpeAnnotated(STraitCall(`tyArgName`,_),_))) => Left(a.name)
        case STraitCall("Elem",List(STraitCall(`tyArgName`,_))) => Left(a.name)
        case STraitCall(_,List(STraitCall(`tyArgName`,_))) => Right(a.name)
      })
      (tyArgName, implicitArgNameOpt.get)
    }

    def tpeSubstStr = tpeSubst.rep {
      case (n,v) => StringUtil.quote(n) + " -> " + v.fold(l => s"Left($l)", r => s"Right($r.asInstanceOf[SomeCont])")
    }

    def companionName = name + "Companion"
    def companionAbsName = name + "CompanionAbs"
  }

  case class EntityTemplateData(m: SEntityModuleDef, t: STraitDef) extends TemplateData(m, t) {
    val elemTypeDecl = s"${name}Elem[${join(tpeArgs.decls, s"To <: $typeUse")}]"
    def elemTypeUse(toType: String = typeUse) = s"${name}Elem[${join(tpeArgNames, toType)}]"
  }

  case class ConcreteClassTemplateData(m: SEntityModuleDef, c: SClassDef) extends TemplateData(m, c) {
    val elemTypeDecl = name + "Elem" + tpeArgsDecl
    val elemTypeUse = name + "Elem" + tpeArgsUse
  }

  def serialize(obj: Any): String = {
    val bos = new ByteArrayOutputStream()
    try {
      val gzip = new GZIPOutputStream(bos)
      try {
        val objOut = new ObjectOutputStream(gzip)
        try {
          objOut.writeObject(obj)
          objOut.close()
          val str = javax.xml.bind.DatatypeConverter.printBase64Binary(bos.toByteArray)
          str
        } finally objOut.close()
      } finally gzip.close()
    } finally bos.close()
  }

  def loadModule(obj: String): SEntityModuleDef = {
    val bytes = javax.xml.bind.DatatypeConverter.parseBase64Binary(obj)
    val bis = new ByteArrayInputStream(bytes)
    try {
      val gzip = new GZIPInputStream(bis)
      try {
        // required instead of ObjectInputStream to work correctly from SBT, see
        // www.scala-sbt.org/0.13/docs/Running-Project-Code.html
        val objIn = new ThreadContextClassLoaderObjectInputStream(gzip)
        try {
          val module = objIn.readObject().asInstanceOf[SEntityModuleDef]
          module
        } finally objIn.close()
      } finally gzip.close()
    } finally bis.close()
  }

  class EntityFileGenerator(module: SEntityModuleDef, config: CodegenConfig) {

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

    def getDefaultOfBT(tc: STpeExpr) =
      s"DefaultOf${tc.name}" + tc.tpeSExprs.asTypeParams()

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
          s"${getDefaultOfBT(tc)}.value"
          //s"Default.defaultOf[$t]"
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

    def tpeToElement(t: STpeExpr, env: List[STpeArg]): String = t match {
      case STpePrimitive(name,_) => name + "Element"
      case STpeTuple(List(a, b)) => s"pairElement(${tpeToElement(a, env)},${tpeToElement(b, env)})"
      case STpeFunc(a, b) => s"funcElement(${tpeToElement(a, env)},${tpeToElement(b, env)})"
      case STraitCall("$bar", List(a,b)) => s"sumElement(${tpeToElement(a, env)},${tpeToElement(b, env)})"
      case STraitCall(name, Nil) if STpePrimitives.contains(name) => name + "Element"
//      case STraitCall(name, Nil)  => s"element[$name]"
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
      case _ => sys.error(s"Don't know how to construct Element for type $t")
    }

    val e = EntityTemplateData(module, entity)
    import e.{baseTypeName, baseTypeUse, companionAbsName, companionName, optBaseType, typeDecl, typeUse}
    val typesWithElems = e.boundedTpeArgString(false)

    def getCompanionMethods = entity.companion.filter(_ => optBaseType.isDefined).map { comp =>
      val externalConstrs = comp.getMethodsWithAnnotation(ConstuctorAnnotation)
      val externalMethods = comp.getMethodsWithAnnotation(ExternalAnnotation)
      (externalConstrs, externalMethods)
    }

    def filterByExplicitDeclaration(ms: List[SMethodDef]): List[SMethodDef] =
      module.seqDslImpl match {
        case Some(impl) =>
          ms.filterNot(impl.containsMethodDef(_))
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

    def externalConstructor(md: SMethodDef) = {
      val allArgs = md.allArgs
      s"""
        |    ${md.declaration(config, false)} =
        |      newObjEx(classOf[$typeUse], List(${allArgs.rep(a => s"${a.name}.asRep[Any]")}))
        |""".stripMargin
    }

    def externalSeqMethod(md: SMethodDef, isInstance: Boolean) = {
      val msgExplicitRetType = "External methods should be declared with explicit type of returning value (result type)"
      val tyRet = md.tpeRes.getOrElse(!!!(msgExplicitRetType))
      val methodNameAndArgs = md.name + md.tpeArgs.useString + methodArgsUse(md)
      val methodHeader = s"${md.declaration(config, true)} =\n"

      val obj = if (isInstance) "wrappedValueOfBaseType" else baseTypeName

      val methodBody = {
        val methodCall = s"$obj.$methodNameAndArgs"
        tyRet.unRep(module, config) match {
          case Some(STraitCall(name, _)) if name == e.name =>
            s"      ${e.name}Impl($methodCall)\n"
          case _ =>
            s"      $methodCall\n"
        }
      }

      methodHeader + methodBody
    }

    def externalSeqConstructor(md: SMethodDef) = {
      s"""
        |    ${md.declaration(config, true)} =
        |      ${e.name}Impl(new $baseTypeUse${methodArgsUse(md)})
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

    def extractSqlQueries(body: List[SBodyItem]): String = {
      body.collect { case m: SMethodDef =>
        m.body match {
          case Some(call: SApply) =>
            call.fun match {
              case SLiteral(value) if value == "sql" => generateQuery(m)
              case _ => ""
            }
          case _ => ""
        }
      }.mkString("\n\n")
    }

    def selfTypeString(suffix: String) =
      module.selfType.opt(t => s"self: ${t.tpe}${suffix} =>")

    private val classes = module.concreteSClasses

    def getTraitAbs = {
      val sqlDDL = module.methods.map(m =>
        if (!m.tpeRes.isDefined) {
          m.body match {
            case Some(call: SApply) =>
              call.fun match {
                case SLiteral(value) if value == "sql" => call.argss(0)(0).asInstanceOf[SLiteral].value
                case _ => ""
              }
            case _ => ""
          }
        } else "").mkString
      val sqlSchema = if (sqlDDL.isEmpty) "" else generateSchema(sqlDDL)
      val sqlQueries = module.methods.filter(m =>
        if (m.tpeRes.isDefined) {
          m.body match {
            case Some(call: SApply) =>
              call.fun match {
                case SLiteral(value) if value == "sql" => true
                case _ => false
              }
            case _ => false
          }
        } else false).map(m => generateQuery(m)).mkString("\n\n")

      val entityCompOpt = entity.companion
      val hasCompanion = entityCompOpt.isDefined
      val proxyBT = optBaseType.opt { bt =>
        s"""
        |  // TypeWrapper proxy
        |  //implicit def proxy$baseTypeName${typesWithElems}(p: Rep[$baseTypeUse]): $typeUse =
        |  //  proxyOps[$typeUse](p.asRep[$typeUse])
        |
        |  implicit def unwrapValueOf$typeDecl(w: Rep[$typeUse]): Rep[$baseTypeUse] = w.wrappedValueOfBaseType
        |""".stripAndTrim
      }

      val baseTypeElem = optBaseType.opt { bt =>
        s"""
        |  implicit def ${StringUtil.lowerCaseFirst(bt.name)}Element${typesWithElems}: Elem[$baseTypeUse]
        |""".stripAndTrim
      }

      def familyContainer(e: EntityTemplateData) = {
        def container(name: String, isFunctor: Boolean) = {
          val contType = s"Container[$name]${isFunctor.opt(s" with Functor[$name]")}"
          s"""\n
             |  implicit lazy val container$name: $contType = new $contType {
             |    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[$name[A]]
             |    def lift[A](implicit evA: Elem[A]) = element[$name[A]]
             |    ${isFunctor.opt(s"def map[A:Elem,B:Elem](xs: Rep[$name[A]])(f: Rep[A] => Rep[B]) = xs.map(fun(f))")}
             |  }
           """.stripMargin
        }

        val entityElem = e.elemTypeUse()

        s"""
        |  implicit def cast${e.name}Element${e.tpeArgsDecl}(elem: Elem[${e.typeUse}]): $entityElem =
        |    elem.asInstanceOf[$entityElem]
        |
        |  ${optBaseType.opt(bt => container(bt.name, false))}
        |
        |  ${container(e.name, e.isFunctor)}
        |
        |  case class ${e.name}Iso[A, B](iso: Iso[A, B]) extends Iso1[A, B, ${e.name}](iso) {
        |    def from(x: Rep[${e.name}[B]]) = x.map(iso.fromFun)
        |    def to(x: Rep[${e.name}[A]]) = x.map(iso.toFun)
        |  }
        |""".stripAndTrim
      }

      def implicitTagsFromElems(t: TemplateData) = t.implicitArgs.flatMap(arg => arg.tpe match {
        case STraitCall(name, List(tpe)) if name == "Elem" || name == "Element" =>
          Some(s"      implicit val tag${typeToIdentifier(tpe)} = ${arg.name}.tag")
        case _ => None
      }).mkString("\n")

      def familyElem(e: EntityTemplateData) = {
        import e.{module => _, _}
        val wildcardElem = s"${e.name}Elem[${Array.fill(e.tpeArgs.length + 1)("_").mkString(", ")}]"
        val (optParent, parentElem) = e.firstAncestorType match {
          case Some(STraitCall("Reifiable", _)) =>
            val parentElem =
              if (isContainer) {
                s"EntityElem1[${e.tpeArgsUse}, To, ${e.name}](${e.tpeArgNames.rep("e" + _)}, container[${e.name}])"
              } else {
                "EntityElem[To]"
              }
            (None, parentElem)
          case Some(STraitCall("TypeWrapper", _)) =>
            val parentElem =
              if (isContainer) {
                s"WrapperElem1[${join(e.tpeArgNames, "To", e.baseTypeName, e.name)}]()(${e.tpeArgNames.rep("_e" + _)}, container[${e.baseTypeName}], container[${e.name}])"
              } else {
                s"WrapperElem[${e.baseTypeUse}, To]"
              }
            (None, parentElem)
          case Some(parent @ STraitCall(parentName, parentTpeArgs)) =>
            (Some(parent), s"${parentName}Elem[${join(parentTpeArgs, "To")}]")
          case Some(p) => !!!(s"Unsupported parent type $p of the entity ${e.name}")
          case None => !!!(s"Entity ${e.name} must extend Reifiable, TypeWrapper, or another entity")
        }
        val overrideIfHasParent = optParent.ifDefined("override ")
        val elemMethodDefinition = {
          val elemMethodName = entityElemMethodName(e.name)
          if (!module.methods.exists(_.name == elemMethodName)) {
            val elemType = e.elemTypeUse()
            val elemMethodBody =
              if (e.isWrapper)
                s"""    elemCache.getOrElseUpdate(
                   |      (classOf[$elemType], ${e.implicitArgsUse.opt(x => s"Seq$x", "Nil")}),
                   |      new $elemType {
                   |        lazy val eTo = new ${e.name}ImplElem${e.tpeArgsUse}(iso${e.name}Impl${e.implicitArgsUse})${e.implicitArgsUse}
                   |      }).asInstanceOf[Elem[${e.typeUse}]]"""
              else
                s"    cachedElem[$elemType]${e.implicitArgsOrParens}"
            s"""
               |  implicit def $elemMethodName${e.tpeArgsDecl}${e.implicitArgsDecl()}: Elem[${e.typeUse}] =
               |$elemMethodBody
               |""".stripMargin
          } else ""
        }

        s"""
        |  // familyElem
        |  ${isWrapper.opt("abstract ")}class ${e.elemTypeDecl}${e.implicitArgsDecl("_")}
        |    extends $parentElem {
        |${e.implicitArgs.opt(_.rep(a => s"    def ${a.name} = _${a.name}", "\n"))}
        |    ${overrideIfHasParent}lazy val parent: Option[Elem[_]] = ${optParent.opt(p => s"Some(${tpeToElement(p, e.tpeArgs)})", "None")}
        |    ${overrideIfHasParent}lazy val entityDef: STraitOrClassDef = {
        |      val module = getModules("${module.name}")
        |      module.entities.find(_.name == "${e.name}").get
        |    }
        |    ${overrideIfHasParent}lazy val tyArgSubst: Map[String, TypeDesc] = {
        |      Map(${e.tpeSubstStr})
        |    }
        |    override def isEntityType = true
        |    override lazy val tag = {
        |${implicitTagsFromElems(e)}
        |      weakTypeTag[${e.typeUse}].asInstanceOf[WeakTypeTag[To]]
        |    }
        |    override def convert(x: Rep[Reifiable[_]]) = {
        |      implicit val eTo: Elem[To] = this
        |      val conv = fun {x: Rep[${e.typeUse}] => convert${e.name}(x) }
        |      tryConvert(element[${e.typeUse}], this, x, conv)
        |    }
        |
        |    def convert${e.name}(x: Rep[${e.typeUse}]): Rep[To] = {
        |      x.selfType1${e.t.isHighKind.opt(".asInstanceOf[Element[_]]")} match {
        |        case _: $wildcardElem => x.asRep[To]
        |        case e => !!!(s"Expected $$x to have $wildcardElem, but got $$e")
        |      }
        |    }
        |    override def getDefaultRep: Rep[To] = ???
        |  }
        |$elemMethodDefinition
        |""".stripAndTrim
      }

      val companionSql = entityCompOpt.opt(comp => extractSqlQueries(comp.body))
      val companionAbs = s"""
        |  implicit case object ${companionName}Elem extends CompanionElem[${companionName}Abs] {
        |    lazy val tag = weakTypeTag[${companionName}Abs]
        |    protected def getDefaultRep = ${e.name}
        |  }
        |
        |  abstract class ${companionName}Abs extends CompanionBase[${companionName}Abs]${hasCompanion.opt(s" with ${companionName}")} {
        |    override def toString = "${e.name}"
        |    $companionSql
        |  }
        |  def ${e.name}: Rep[${companionName}Abs]
        |${hasCompanion.opt
        s"""
           |  implicit def proxy$companionName(p: Rep[$companionName]): $companionName =
           |    proxyOps[$companionName](p)
           |""".stripAndTrim
      }
          |""".stripAndTrim

      val subEntities = for { entity <- module.entities.drop(1) } yield {
        val templateData = EntityTemplateData(module, entity)

        s"""
        |${entityProxy(templateData)}
        |${familyElem(templateData)}
        |${extractSqlQueries(entity.body)}
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

        lazy val defaultImpl = optBaseType.opt { bt =>
          val externalMethods = entity.getMethodsWithAnnotation(ExternalAnnotation)
          val externalMethodsStr = externalMethods.rep(md => externalMethod(md), "\n    ")
          if (className != s"${e.name}Impl")
            ""
          else {
            val implicitArgsWithVals = c.implicitArgsDecl("val ")
            s"""
            |  // default wrapper implementation
            |  abstract class ${e.name}Impl${tpeArgsDecl}(val wrappedValueOfBaseType: Rep[$baseTypeUse])${implicitArgsWithVals} extends $typeUse {
            |    $externalMethodsStr
            |  }
            |  trait ${e.name}ImplCompanion
            |""".stripAndTrim
          }
        }

        // necessary in cases Scala type inference fails
        val maybeElemHack = {
          val elemMethodName = StringUtil.lowerCaseFirst(className + "DataElem")
          if (module.methods.exists(_.name == elemMethodName))
            s"()($elemMethodName)"
          else {
            @tailrec
            def implElem(args: List[String])(str0: String): String = {
              val size = args.length
              if (size > 2) {
                val str = str0 + s"pairElement(implicitly[Elem[${args(0)}]], "
                implElem(args.drop(1))(str)
              }
              else {
                if (size > 1) str0 + s"pairElement(implicitly[Elem[${args(0)}]], implicitly[Elem[${args(1)}]])"
                else str0 + s"(implicitly[Elem[${args(0)}]])"
              }
            }
            val args = fieldTypes.map(_.toString)
            if (args.length >= 2) {
              val impls = implElem(args)("")
              val sk = ")" * (args.length - 2)
              s"()(${impls + sk})"
            } else ""
          }
        }

        val parentElem = tpeToElement(parent, c.tpeArgs)
        val hasCompanion = clazz.companion.isDefined
        val dataTpe = s"${className}Data$tpeArgsUse"
        val concreteElemSuperType = if (e.isContainer)
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
            println(s"    WARNING: cannot generate converter $msg")
            s"""|// Converter is not generated by meta
                |!!!("Cannot convert $msg")""".stripMargin
          }
        }

        // note: ${className}Iso.eTo doesn't call cachedElem because
        // they are already cached via Isos + lazy val and this would lead to stack overflow
        s"""
        |$defaultImpl
        |  // elem for concrete class
        |  class ${c.elemTypeDecl}(val iso: Iso[$dataTpe, ${c.typeUse}])$implicitArgsDecl
        |    extends ${parent.name}Elem[${join(parentTpeArgsStr, c.typeUse)}]
        |    with $concreteElemSuperType {
        |    override lazy val parent: Option[Elem[_]] = Some($parentElem)
        |    override lazy val entityDef = {
        |      val module = getModules("${module.name}")
        |      module.concreteSClasses.find(_.name == "$className").get
        |    }
        |    override lazy val tyArgSubst: Map[String, TypeDesc] = {
        |      Map(${c.tpeSubstStr})
        |    }
        |    ${e.isWrapper.opt("lazy val eTo = this")}
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
        |    extends Iso[$dataTpe, ${c.typeUse}]$maybeElemHack {
        |    override def from(p: Rep[${c.typeUse}]) =
        |      ${fields.map(fields => "p." + fields).opt(s => if (s.toList.length > 1) s"(${s.rep()})" else s.rep(), "()")}
        |    override def to(p: Rep[${dataType(fieldTypes)}]) = {
        |      val ${pairify(fields)} = p
        |      $className(${fields.rep()})
        |    }
        |    lazy val eTo = new ${c.elemTypeUse}(this)
        |  }
        |  // 4) constructor and deconstructor
        |  abstract class ${c.companionAbsName} extends CompanionBase[${c.companionAbsName}]${hasCompanion.opt(s" with ${className}Companion")} {
        |    override def toString = "$className"
        |${(fields.length != 1).opt(s"""
        |    def apply${tpeArgsDecl}(p: Rep[$dataTpe])${implicitArgsDecl}: Rep[${c.typeUse}] =
        |      iso$className${implicitArgsUse}.to(p)""".stripAndTrim)}
        |    def apply${tpeArgsDecl}(${fieldsWithType.rep()})${implicitArgsDecl}: Rep[${c.typeUse}] =
        |      mk$className(${fields.rep()})
        |    ${extractSqlQueries(clazz.body)}
        |  }
        |  object ${className}Matcher {
        |    def unapply${tpeArgsDecl}(p: Rep[$parent]) = unmk$className(p)
        |  }
        |  def $className: Rep[${c.companionAbsName}]
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
        |    cachedIso[${className}Iso${tpeArgsUse}]$implicitArgsOrParens
        |
        |  // 6) smart constructor and deconstructor
        |  def mk${c.typeDecl}(${fieldsWithType.rep()})${implicitArgsDecl}: Rep[${c.typeUse}]
        |  def unmk${c.typeDecl}(p: Rep[$parent]): Option[(${fieldTypes.opt(fieldTypes => fieldTypes.rep(t => s"Rep[$t]"), "Rep[Unit]")})]
        |""".stripAndTrim
      }

      s"""
       |// Abs -----------------------------------
       |trait ${module.name}Abs extends ${module.name} ${config.baseContextTrait.opt(t => "with " + t)} {
       |  ${selfTypeString("")}
       |
       |${entityProxy(e)}
       |
       |$proxyBT
       |
       |$baseTypeElem
       |
       |${if (e.isContainer) familyContainer(e) else ""}
       |
       |${familyElem(e)}
       |$sqlSchema
       |
       |$sqlQueries
       |
       |$companionAbs
       |
       |${subEntities.mkString("\n\n")}
       |
       |${concreteClasses.mkString("\n\n")}
       |
       |  registerModule(scalan.meta.ScalanCodegen.loadModule(${module.name}_Module.dump))
       |}
       |""".stripAndTrim
    }

    def getSClassSeq(clazz: SClassDef) = {
      val className = clazz.name
      val c = ConcreteClassTemplateData(module, clazz)
      val fields = clazz.args.argNames
      val fieldsWithType = clazz.args.argNamesAndTypes(config)
      val implicitArgsDecl = c.implicitArgsDecl()

      val externalMethods = entity.getMethodsWithAnnotation(ExternalAnnotation)
      val externalMethodsStr = filterByExplicitDeclaration(externalMethods).rep(md => externalSeqMethod(md, true), "\n    ")

      val parent     = clazz.ancestors.head
      val companionAbs = c.companionAbsName

      val userTypeDefs =
        s"""
         |  case class Seq${c.typeDecl}
         |      (${fieldsWithType.rep(f => s"override val $f")})
         |      ${implicitArgsDecl}
         |    extends ${c.typeUse}(${fields.rep()})${clazz.selfType.opt(t => s" with ${t.tpe}")}
         |       ${module.seqDslImpl.ifDefined(s"with Seq$typeUse")} with UserTypeSeq[${c.typeUse}] {
         |    lazy val selfType = element[${c.typeUse}]
         |    $externalMethodsStr
         |  }
         |  lazy val $className = new $companionAbs with UserTypeSeq[$companionAbs] {
         |    lazy val selfType = element[$companionAbs]
         |  }""".stripAndTrim

      val constrDefs =
        s"""
         |  def mk${c.typeDecl}
         |      (${fieldsWithType.rep()})$implicitArgsDecl: Rep[${c.typeUse}] =
         |      new Seq${c.typeUse}(${fields.rep()})${clazz.selfType.opt(t => s" with ${t.tpe}")}
         |  def unmk${c.typeDecl}(p: Rep[$parent]) = p match {
         |    case p: ${c.typeUse} @unchecked =>
         |      Some((${fields.rep(f => s"p.$f")}))
         |    case _ => None
         |  }
         |""".stripAndTrim

      s"""$userTypeDefs\n\n$constrDefs"""
    }

    def getSClassExp(clazz: SClassDef) = {
      val c = ConcreteClassTemplateData(module, clazz)
      import c._
      val fields = clazz.args.argNames
      val fieldsWithType = clazz.args.argNamesAndTypes(config)
      val parent     = clazz.ancestors.head

      val userTypeNodeDefs =
        s"""
         |  case class Exp${c.typeDecl}
         |      (${fieldsWithType.rep(f => s"override val $f")})
         |      ${implicitArgsDecl()}
         |    extends ${c.typeUse}(${fields.rep()})${clazz.selfType.opt(t => s" with ${t.tpe}")} with UserTypeDef[${c.typeUse}] {
         |    lazy val selfType = element[${c.typeUse}]
         |    override def mirror(t: Transformer) = Exp${c.typeUse}(${fields.rep(f => s"t($f)")})
         |  }
         |
         |  lazy val ${c.name}: Rep[${c.companionAbsName}] = new ${c.companionAbsName} with UserTypeDef[${c.companionAbsName}] {
         |    lazy val selfType = element[${c.companionAbsName}]
         |    override def mirror(t: Transformer) = this
         |  }
         |
         |${methodExtractorsString(clazz)}
         |""".stripAndTrim

      val constrDefs =
        s"""
         |  def mk${c.typeDecl}
         |    (${fieldsWithType.rep()})${implicitArgsDecl()}: Rep[${c.typeUse}] =
         |    new Exp${c.typeUse}(${fields.rep()})${clazz.selfType.opt(t => s" with ${t.tpe}")}
         |  def unmk${c.typeDecl}(p: Rep[$parent]) = p.elem.asInstanceOf[Elem[_]] match {
         |    case _: ${c.elemTypeUse} @unchecked =>
         |      Some((${fields.rep(f => s"p.asRep[${c.typeUse}].$f")}))
         |    case _ =>
         |      None
         |  }
         |""".stripAndTrim

      s"""$userTypeNodeDefs\n\n$constrDefs"""
    }

    // note: currently can't cache them properly due to cyclical dependency between
    // baseType elem and wrapper elem
    def baseTypeElem(ctx: String) = optBaseType.opt { bt =>
      val defOrVal = if (e.tpeArgs.isEmpty) "lazy val" else "def"
      val declaration = s"implicit $defOrVal ${StringUtil.lowerCaseFirst(bt.name)}Element${typesWithElems}: Elem[$baseTypeUse]"
      if (e.isContainer) {
        val elems = e.tpeArgNames.rep(ty => s"element[$ty]")
        s"""
          |  $declaration =
          |    new ${ctx}BaseElemEx1[${join(e.tpeArgNames, typeUse, baseTypeName)}](element[$typeUse])(
          |      $elems, container[${bt.name}], ${getDefaultOfBT(bt)})
          |""".stripAndTrim
      } else {
        val weakTagsForTpeArgs = e.tpeArgNames.map { argName =>
          s"implicit val w$argName = element[$argName].tag"
        }.mkString("\n")
        s"""
          |  $declaration = {
          |    $weakTagsForTpeArgs
          |    new ${ctx}BaseElemEx[$baseTypeUse, $typeUse](element[$typeUse])(weakTypeTag[$baseTypeUse], ${getDefaultOfBT(bt)})
          |  }
          |""".stripAndTrim
      }
    }

    def getTraitSeq = {
      val classesSeq = classes.map(getSClassSeq)
      val proxyBTSeq = optBaseType.opt(bt =>
        s"""
         |  // override proxy if we deal with TypeWrapper
         |  //override def proxy$baseTypeName${typesWithElems}(p: Rep[$baseTypeUse]): $typeUse =
         |  //  proxyOpsEx[$baseTypeUse, $typeUse, Seq${e.name}Impl${e.tpeArgsUse}](p, bt => Seq${e.name}Impl(bt))
         |""".stripAndTrim
      )
      val baseTypeToWrapperConvertionSeq = optBaseType.opt(bt =>
        s"""
         |  implicit def wrap${baseTypeName}To${e.name}${typesWithElems}(v: $baseTypeUse): $typeUse = ${e.name}Impl(v)
         |""".stripAndTrim
      )

      val companionMethods = getCompanionMethods.opt { case (constrs, methods) =>
        filterByExplicitDeclaration(constrs).rep(md => externalSeqConstructor(md), "\n    ") +
        filterByExplicitDeclaration(methods).filter(_.body.isEmpty).rep(md => externalSeqMethod(md, false), "\n    ")
      }

      s"""
       |// Seq -----------------------------------
       |trait ${module.name}Seq extends ${module.name}Dsl ${config.seqContextTrait.opt(t => "with " + t)} {
       |  ${selfTypeString("Seq")}
       |  lazy val ${e.name}: Rep[$companionAbsName] = new $companionAbsName with UserTypeSeq[$companionAbsName] {
       |    lazy val selfType = element[$companionAbsName]
       |    $companionMethods
       |  }
       |
       |  $proxyBTSeq
       |
       |${baseTypeElem("Seq")}
       |
       |${classesSeq.mkString("\n\n")}
       |
       |$baseTypeToWrapperConvertionSeq
       |}
       |""".stripAndTrim
    }

    def familyView(e: EntityTemplateData) = {
      s"""
        |  case class View${e.name}[A, B](source: Rep[${e.name}[A]])(iso: Iso1[A, B, ${e.name}])
        |    extends View1[A, B, ${e.name}](iso) {
        |    def copy(source: Rep[${e.name}[A]]) = View${e.name}(source)(iso)
        |    override def toString = s"View${e.name}[$${innerIso.eTo.name}]($$source)"
        |    override def equals(other: Any) = other match {
        |      case v: View${e.name}[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
        |      case _ => false
        |    }
        |  }
        |""".stripAndTrim
    }

    def emitContainerRules(e: EntityTemplateData) = {
      if (e.isContainer) {
        s"""
          |    case ${e.name}Methods.map(xs, Def(IdentityLambda())) => xs
          |
          |    case view1@View${e.name}(Def(view2@View${e.name}(arr))) =>
          |      val compIso = composeIso(view1.innerIso, view2.innerIso)
          |      implicit val eAB = compIso.eTo
          |      View${e.name}(arr)(${e.name}Iso(compIso))
          |
          |    // Rule: W(a).m(args) ==> iso.to(a.m(unwrap(args)))
          |    case mc @ MethodCall(Def(wrapper: Exp${e.name}Impl[_]), m, args, neverInvoke) if !isValueAccessor(m) =>
          |      val resultElem = mc.selfType
          |      val wrapperIso = getIsoByElem(resultElem)
          |      wrapperIso match {
          |        case iso: Iso[base,ext] =>
          |          val eRes = iso.eFrom
          |          val newCall = unwrapMethodCall(mc, wrapper.wrappedValueOfBaseType, eRes)
          |          iso.to(newCall)
          |      }
          |
          |    case ${e.name}Methods.map(xs, f) => (xs, f) match {
          |      case (xs: ${e.entityRepSynonym.name}[a] @unchecked, LambdaResultHasViews(f, iso: Iso[b, c])) =>
          |        val f1 = f.asRep[a => c]
          |        implicit val eA = xs.elem.eItem
          |        implicit val eB = iso.eFrom
          |        val s = xs.map(fun { x =>
          |          val tmp = f1(x)
          |          iso.from(tmp)
          |        })
          |        val res = View${e.name}(s)(${e.name}Iso(iso))
          |        res
          |      case (HasViews(source, contIso: ${e.name}Iso[a, b]), f: Rep[Function1[_, c] @unchecked]) =>
          |        val f1 = f.asRep[b => c]
          |        val iso = contIso.iso
          |        implicit val eA = iso.eFrom
          |        implicit val eB = iso.eTo
          |        implicit val eC = f1.elem.eRange
          |        source.asRep[${e.name}[a]].map(fun { x => f1(iso.to(x)) })
          |      case _ =>
          |        super.rewriteDef(d)
          |    }
           """.stripMargin
      }
      else ""
    }

    def emitRewriteDef(e: EntityTemplateData) = {
      if (e.isContainer) {
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
        |      val newIso = ${e.name}Iso(iso)
        |      val repr = reifyObject(UnpackView(s.asRep[${e.name}[b]])(newIso))
        |      Some((repr, newIso))
        |    case _ =>
        |      super.unapplyViews(s)
        |  }).asInstanceOf[Option[Unpacked[T]]]
        |
        |  ${e.entityRepSynonymOpt.isEmpty.opt(e.entityRepSynonym.declaration)}
        |
        |  override def rewriteDef[T](d: Def[T]) = d match {
        |    ${emitContainerRules(e)}
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

      val concreteClassesString = classes.map(getSClassExp)

      s"""
       |// Exp -----------------------------------
       |trait ${module.name}Exp extends ${module.name}Dsl ${config.stagedContextTrait.opt(t => "with " + t)} {
       |  ${selfTypeString("Exp")}
       |  lazy val ${e.name}: Rep[$companionAbsName] = new $companionAbsName with UserTypeDef[$companionAbsName] {
       |    lazy val selfType = element[$companionAbsName]
       |    override def mirror(t: Transformer) = this
       |    $companionMethods
       |  }
       |
       |${if (e.isContainer) familyView(e) else ""}
       |
       |${baseTypeElem("Exp")}
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
                  s"(receiver.elem.asInstanceOf[Element[_]] match { case _: $traitElem => true; case _ => false })"
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
           |${methods.filterNot(_.isElemOrCont).map(methodExtractor).mkString("\n\n")}
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
        (module.hasDslSeq, "Seq", "Seq"),
        (module.hasDslExp, "Exp", "Exp")).collect {
        case (hasDslTrait, dslTraitSuffix, traitSuffix) if !hasDslTrait =>
          s"trait ${module.name}Dsl${dslTraitSuffix} extends impl.${module.name}${traitSuffix} {${selfTypeString(dslTraitSuffix)}}"
      }.mkString("\n")
    }

    def emitModuleSerialization = {
      s"""
       |object ${module.name + "_"}Module {
       |  val packageName = "${module.packageName}"
       |  val name = "${module.name}"
       |  val dump = "${serialize(module.clean)}"
       |}
       """.stripMargin
    }

    def getImplFile: String = {
      val topLevel = List(
        getFileHeader,
        getTraitAbs,
        getTraitSeq,
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
}
