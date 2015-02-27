package scalan.meta

import scalan.util.{StringUtil, ScalaNameUtil}

trait ScalanCodegen extends ScalanParsers with SqlCompiler with ScalanAstExtensions { ctx: EntityManagement =>
  import PrintExtensions._

  abstract class TemplateData(val module: SEntityModuleDef, val entity: STraitOrClassDef) {
    val name = entity.name
    val tpeArgs = entity.tpeArgs
    val tpeArgDecls = tpeArgs.getTpeArgDecls
    val tpeArgUses = tpeArgs.getTpeArgUse
    val tpeArgDeclString = tpeArgs.getTpeArgDeclString
    val tpeArgUseString = tpeArgs.getTpeArgUseString
    val entityType = name + tpeArgUseString
    val typesDeclPref = tpeArgDecls.opt(_.rep() + ", ")
    val typesUsePref = tpeArgUses.opt(_.rep() + ", ")
    val implicitArgsDecl = entity.implicitArgs.args.opt(args => s"(implicit ${args.rep(a => s"${a.name}: ${a.tpe}")})")
    val implicitArgsUse = entity.implicitArgs.args.opt(args => s"(${args.map(_.name).rep()})")
    val optBT = entity.optBaseType
    val firstAncestorType = entity.ancestors.head
    val entityRepSynonimOpt = module.entityRepSynonym

    def entityRepSynonim = entityRepSynonimOpt match {
      case Some(s) => s
      case None => STpeDef("Rep" + name, tpeArgs, STraitCall("Rep", List(STraitCall(name, tpeArgs.map(_.toTraitCall)))))
    }

    def emitEntityRepSynonim = {
      val td = entityRepSynonim
      td.emitTypeDecl
    }

    def isContainer1 = tpeArgs.length == 1 && entity.hasAnnotation(ContainerTypeAnnotation)

    def boundedTpeArgString(withTags: Boolean = false) = tpeArgs.getBoundedTpeArgString(withTags)

    def tpeArgsImplicitDecl(typeClass: String) = {
      tpeArgs.opt(args =>
        s"(implicit ${args.rep(t => (s"ev${t.name}: $typeClass[${t.name}]"))})")
    }
  }

  case class EntityTemplateData(m: SEntityModuleDef, t: STraitDef) extends TemplateData(m, t)

  case class ConcreteClassTemplateData(m: SEntityModuleDef, c: SClassDef) extends TemplateData(m, c) {
    val classType = name + tpeArgUseString
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

    def getDefaultOfBT(tc: STraitCall) =
      s"DefaultOf${tc.name}" + tc.tpeSExprs.opt(args => s"[${args.rep()}]")

    def zeroSExpr(t: STpeExpr): String = t match {
      case STpePrimitive(_, defaultValueString) => defaultValueString
      case STraitCall(name, args)
        if module.entityOps.tpeArgs.exists(a => a.name == name && a.isHighKind) =>
        s"c$name.lift(${args.rep(a => s"e${a}")}).defaultRepValue"
      case tc@STraitCall(name, args) => {
        val optBT = module.entityOps.optBaseType
        val isBT = optBT.exists(bt => bt.name == name)
        if (isBT) {
          getDefaultOfBT(tc) + ".value"
          //s"Default.defaultOf[$t]"
        } else {
          s"element[$t].defaultRepValue"
        }
      }
      case STpeTuple(items) => pairify(items.map(zeroSExpr))
      case STpeFunc(domain, range) => s"""fun { (x: Rep[${domain}]) => ${zeroSExpr(range)} }"""
      case t => throw new IllegalArgumentException(s"Can't generate zero value for $t")
    }

    def typeArgString(typeArgs: Seq[String]) =
      if (typeArgs.isEmpty) "" else typeArgs.mkString("[", ", ", "]")

    val templateData = EntityTemplateData(module, module.entityOps)
    val optBT = templateData.optBT
    val entityName = templateData.name
    val entityNameBT = optBT.map(_.name).getOrElse(templateData.name)
    val tyArgsDecl = templateData.tpeArgDecls
    val tyArgsUse = templateData.tpeArgUses
    val typesDecl = templateData.tpeArgDeclString
    val typesUse = templateData.tpeArgUseString
    val typesWithElems = templateData.boundedTpeArgString(false)
    val typesWithElemsAndTags = templateData.boundedTpeArgString(true)

    def getCompanionOpt = for {bt <- optBT; comp <- module.entityOps.companion} yield comp

    def getCompanionMethods = getCompanionOpt.map { comp =>
      val externalConstrs = comp.getMethodsWithAnnotation(ConstuctorAnnotation)
      val externalMethods = comp.getMethodsWithAnnotation(ExternalAnnotation)
      (externalConstrs, externalMethods)
    }

    def filterByExplicitDeclaration(ms: List[SMethodDef]) = {
      val filtered = module.seqDslImpl match {
        case Some(impl) =>
          for {m <- ms if !impl.containsMethodDef(m)} yield m
        case None => ms
      }
      filtered
    }

    def methodArgSection(sec: SMethodArgs) = s"(${sec.argNamesAndTypes.rep()})"

    def methodArgsUse(sec: SMethodArgs) = {
      s"(${
        sec.args.rep(a => {
          if (a.tpe.isTupledFunc)
            s"scala.Function.untupled(${a.name})"
          else if (a.isArgList)
            s"${a.name}: _*"
          else
            s"${a.name}"
        })
      })"
    }

    def externalMethod(md: SMethodDef) = {
      val msgExplicitRetType = "External methods should be declared with explicit type of returning value (result type)"
      lazy val msgRepRetType = s"Invalid method $md. External methods should have return type of type Rep[T] for some T."
      val tyRet = md.tpeRes.getOrElse(!!!(msgExplicitRetType))
      val allArgs = md.argSections.flatMap(_.args)
      val typesDecl = md.tpeArgs.getBoundedTpeArgString(false)
      val tyRetStr = tyRet.unRep(module, config).getOrElse(!!!(msgRepRetType))
      val argClassesStr = allArgs.rep(a => s", classOf[AnyRef]", "")
      val elemClassesStr = (for {
        a <- md.tpeArgs
        cb <- a.contextBound
      } yield s", classOf[$cb[${a.name}]]").rep()
      val elemArgs = (for {
        a <- md.tpeArgs
        cb <- a.contextBound
      } yield s"${if (cb == "Elem") "element" else "weakTypeTag"}[${a.name}]")
      val compoundArgs = !(allArgs.isEmpty || elemArgs.isEmpty)
      s"""
        |    def ${md.name}$typesDecl${md.argSections.rep(methodArgSection(_), "")}: ${tyRet.toString} =
        |      methodCallEx[$tyRetStr](self,
        |        this.getClass.getMethod("${md.name}"$argClassesStr$elemClassesStr),
        |        List(${allArgs.rep(a => s"${a.name}.asInstanceOf[AnyRef]")}${compoundArgs.opt(", ")}${elemArgs.rep()}))
        |""".stripMargin
    }

    def externalConstructor(md: SMethodDef) = {
      val msgExplicitRetType = "External constructors should be declared with explicit type of returning value (result type)"
      lazy val msgRepRetType = s"Invalid constructor declaration $md. External constructors should have return type of type Rep[T] for some T."
      val tyRet = md.tpeRes.getOrElse(!!!(msgExplicitRetType))
      val unrepRet = tyRet.unRep(module, config).getOrElse(!!!(msgRepRetType))
      val allArgs = md.argSections.flatMap(_.args)
      val typesDecl = md.tpeArgs.getBoundedTpeArgString(false)
      s"""
        |    def ${md.name}$typesDecl${md.argSections.rep(methodArgSection(_), "")}: ${tyRet.toString} =
        |      newObjEx(classOf[$entityName${typesUse}], List(${allArgs.rep(a => s"${a.name}.asRep[Any]")}))
        |""".stripMargin
    }

    def externalSeqMethod(md: SMethodDef, isInstance: Boolean) = {
      val msgExplicitRetType = "External methods should be declared with explicit type of returning value (result type)"
      val tyRet = md.tpeRes.getOrElse(!!!(msgExplicitRetType))
      val typesDecl = md.tpeArgs.getBoundedTpeArgString(false)
      val typesUse = md.tpeArgs.getTpeArgUseString
      val methodHeader =
        s"""override def ${md.name}$typesDecl${md.argSections.rep(methodArgSection(_), "")}: ${tyRet.toString} =
        |""".stripMargin

      val obj = if (isInstance) "wrappedValueOfBaseType" else entityNameBT

      val methodBody = tyRet.unRep(module, config) match {
        case Some(STraitCall(name, _)) if name == entityName =>
          s"""      ${entityName}Impl($obj.${md.name}$typesUse${md.argSections.rep(methodArgsUse(_), "")})
          |""".stripMargin
        case _ =>
          s"""      $obj.${md.name}$typesUse${md.argSections.rep(methodArgsUse(_), "")}
          |""".stripMargin
      }

      methodHeader + methodBody
    }

    def externalSeqConstructor(md: SMethodDef) = {
      val msgExplicitRetType = "External constructors should be declared with explicit type of returning value (result type)"
      val tyRet = md.tpeRes.getOrElse(!!!(msgExplicitRetType))
      val typesDecl = md.tpeArgs.getBoundedTpeArgString(false)
      s"""
        |    override def ${md.name}$typesDecl${md.argSections.rep(methodArgSection(_), "")}: ${tyRet.toString} =
        |      ${entityName}Impl(new $entityNameBT${typesUse}${md.argSections.rep(methodArgsUse(_), "")})
        |""".stripMargin
    }

    def entityProxy(e: EntityTemplateData) = {
      val entityName = e.name
      val typesDecl = e.tpeArgDeclString
      val typesUse = e.tpeArgUseString
      s"""
        |  // single proxy for each type family
        |  implicit def proxy$entityName${typesDecl}(p: Rep[${e.entityType}]): ${e.entityType} = {
        |    implicit val tag = weakTypeTag[${e.entityType}]
        |    proxyOps[${e.entityType}](p)(TagImplicits.typeTagToClassTag[${e.entityType}])
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

    def getTraitAbs = {
      val sqlDDL = module.methods.map(m =>
        if (!m.tpeRes.isDefined) {
          m.body match {
            case Some(call: SApply) =>
              call.fun match {
                case SLiteral(value) if value == "sql" => call.args(0).asInstanceOf[SLiteral].value
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

      val entityCompOpt = module.entityOps.companion
      val companionName = s"${entityName}Companion"
      val proxyBT = optBT.opt(bt => {
        s"""
        |  // BaseTypeEx proxy
        |  //implicit def proxy$entityNameBT${typesWithElems}(p: Rep[$entityNameBT${typesUse}]): $entityName$typesUse =
        |  //  proxyOps[$entityName${typesUse}](p.asRep[$entityName${typesUse}])
        |
        |  implicit def unwrapValueOf$entityName${typesDecl}(w: Rep[$entityName${typesUse}]): Rep[$entityNameBT${typesUse}] = w.wrappedValueOfBaseType
        |
        |  implicit def default${entityName}Elem${typesWithElems}: Elem[$entityName${typesUse}] = element[${entityName}Impl${typesUse}].asElem[$entityName${typesUse}]
        |""".stripAndTrim
      })

      val baseTypeElem = optBT.opt(bt =>
        if (tyArgsDecl.isEmpty) {
          s"""
          |  implicit def ${bt.name}Element: Elem[${bt.name}]
          |""".stripAndTrim
        }
        else {
          s"""
          |  implicit def ${bt.name}Element${typesWithElemsAndTags}: Elem[$entityNameBT${typesUse}]
          |""".stripAndTrim
        })

      def familyContainer(e: EntityTemplateData) = {
        val typesDecl = e.tpeArgDeclString
        val entityElemType = s"${entityName}Elem[${e.typesUsePref}_,${e.entityType}]"
        s"""
        |  implicit def cast${e.name}Element${typesDecl}(elem: Elem[${e.entityType}]): $entityElemType = elem.asInstanceOf[$entityElemType]
        |  implicit val container${e.name}: Cont[${e.name}] = new Container[${e.name}] {
        |    def tag${typesDecl}${e.tpeArgsImplicitDecl("WeakTypeTag")} = weakTypeTag[${e.entityType}]
        |    def lift${typesDecl}${e.tpeArgsImplicitDecl("Elem")} = element[${e.entityType}]
        |  }
        |  case class ${e.name}Iso[A,B](iso: Iso[A,B]) extends Iso1[A, B, ${e.name}](iso) {
        |    implicit val eA = iso.eFrom
        |    implicit val eB = iso.eTo
        |    def from(x: Rep[${e.name}[B]]) = x.map(iso.from _)
        |    def to(x: Rep[${e.name}[A]]) = x.map(iso.to _)
        |    lazy val defaultRepTo = Default.defaultVal(${e.name}.empty[B])
        |  }
        |""".stripAndTrim
      }

      def familyElem(e: EntityTemplateData) = {
        val entityName = e.name
        val typesUse = e.tpeArgUseString
        val isCont = e.isContainer1
        s"""
        |  abstract class ${e.name}Elem[${e.typesDeclPref}From, To <: ${e.entityType}](iso: Iso[From, To])${e.implicitArgsDecl}
        |    extends ViewElem${isCont.opt("1")}[${isCont.opt(e.typesUsePref)}From, To${isCont.opt(s", $entityName")}](iso) {
        |    override def convert(x: Rep[Reifiable[_]]) = convert$entityName(x.asRep[${e.entityType}])
        |    def convert$entityName(x : Rep[${e.entityType}]): Rep[To]
        |  }
        |""".stripAndTrim
      }

      val companionMethods = getCompanionMethods.opt { case (constrs, methods) =>
        constrs.rep(md => externalConstructor(md), "\n    ") +
        methods.rep(md => externalMethod(md), "\n    ")
      }

      val companionSql = entityCompOpt.opt(comp => extractSqlQueries(comp.body))
      val companionAbs = s"""
        |  trait ${companionName}Elem extends CompanionElem[${companionName}Abs]
        |  implicit lazy val ${companionName}Elem: ${companionName}Elem = new ${companionName}Elem {
        |    lazy val tag = weakTypeTag[${companionName}Abs]
        |    protected def getDefaultRep = $entityName
        |  }
        |
        |  abstract class ${companionName}Abs extends CompanionBase[${companionName}Abs] with ${companionName} {
        |    override def toString = "$entityName"
        |    $companionMethods
        |    $companionSql
        |  }
        |  def $entityName: Rep[${companionName}Abs]
        |  implicit def proxy$companionName(p: Rep[${companionName}]): ${companionName} = {
        |    proxyOps[${companionName}](p)
        |  }
        |""".stripAndTrim

      val subEntities = for { entity <- module.entities.drop(1) } yield {
        val templateData = EntityTemplateData(module, entity)
        val entityName = templateData.name
        val tyArgsDecl = templateData.tpeArgDecls
        val typesDecl = templateData.tpeArgDeclString
        val typesUse = templateData.tpeArgUseString

        s"""
        |${entityProxy(templateData)}
        |${familyElem(templateData)}
        |${extractSqlQueries(entity.body)}
        |""".stripMargin
      }

      val concreteClasses = for { c <- module.concreteSClasses } yield {
        val className = c.name
        val concTemplateData = ConcreteClassTemplateData(module, c)
        val typesDecl = concTemplateData.tpeArgDeclString
        val typesUse = concTemplateData.tpeArgUseString
        val typesWithElems = concTemplateData.boundedTpeArgString(false)
        val fields = c.args.argNames
        val fieldsWithType = c.args.argNamesAndTypes
        val fieldTypes = c.args.argUnrepTypes(module, config)
        val implicitArgs = concTemplateData.implicitArgsDecl
        val useImplicits = concTemplateData.implicitArgsUse
        val implicitArgsWithVals = c.implicitArgs.args.opt(args => s"(implicit ${args.rep(a => s"val ${a.name}: ${a.tpe}")})")
        val parent     = c.ancestors.head
        val parentArgs = parent.tpeSExprs.map(_.toString)
        val parentArgsStr = parentArgs.map(_ + ", ").mkString

        lazy val defaultImpl = optBT.opt(bt => {
          val externalMethods = module.entityOps.getMethodsWithAnnotation(ExternalAnnotation)
          val externalMethodsStr = externalMethods.rep(md => externalMethod(md), "\n    ")
          if (className != s"${entityName}Impl") ""
          else {
            s"""
            |  // default wrapper implementation
            |  abstract class ${entityName}Impl${typesDecl}(val wrappedValueOfBaseType: Rep[${entityNameBT}${typesUse}])${implicitArgsWithVals} extends ${entityName}${typesUse} {
            |    $externalMethodsStr
            |  }
            |  trait ${entityName}ImplCompanion
            |""".stripAndTrim
          }
        })

        // necessary in cases Scala type inference fails
        val maybeElemHack = {
          val elemMethodName = StringUtil.lowerCaseFirst(className + "DataElem")
          if (module.methods.exists(_.name == elemMethodName))
            s"()($elemMethodName)"
          else
            ""
        }

        def converterBody(entity: STraitOrClassDef, conc: SClassDef) = {
          val availableFields = entity.getAvailableFields(module)
          val concFields = conc.args.args.map(_.name)
          val missingFields = concFields.filterNot(availableFields.contains(_))
          if (missingFields.isEmpty) {
            val args = concFields.rep(f => s"x.$f")
            s"$className($args)"
          }
          else {
            val msg = s"from ${entity.name} to ${conc.name}: missing fields ${missingFields}"
            println("    WARNING: cannot generate converter " + msg)
            "// Converter is not generated by meta\n" +
            "!!!(\"Cannot convert " + msg + "\")"
          }
        }

        s"""
        |$defaultImpl
        |  // elem for concrete class
        |  class ${className}Elem${typesDecl}(iso: Iso[${className}Data${typesUse}, $className${typesUse}])$implicitArgsWithVals
        |    extends ${parent.name}Elem[${parentArgsStr}${className}Data${typesUse}, $className${typesUse}](iso) {
        |    def convert${parent.name}(x: Rep[${parent.name}${parentArgs.opt("[" + _.rep() + "]")}]) = ${converterBody(module.getEntity(parent.name), c)}
        |  }
        |
        |  // state representation type
        |  type ${className}Data${typesDecl} = ${dataType(fieldTypes)}
        |
        |  // 3) Iso for concrete class
        |  class ${className}Iso${typesDecl}${implicitArgs}
        |    extends Iso[${className}Data${typesUse}, $className${typesUse}]$maybeElemHack {
        |    override def from(p: Rep[$className${typesUse}]) =
        |      unmk${className}(p) match {
        |        case Some((${fields.opt(fields => fields.rep(), "unit")})) => ${pairify(fields)}
        |        case None => !!!
        |      }
        |    override def to(p: Rep[${dataType(fieldTypes)}]) = {
        |      val ${pairify(fields)} = p
        |      $className(${fields.rep()})
        |    }
        |    lazy val tag = {
        |      weakTypeTag[$className${typesUse}]
        |    }
        |    lazy val defaultRepTo = Default.defaultVal[Rep[$className${typesUse}]]($className(${fieldTypes.rep(zeroSExpr(_))}))
        |    lazy val eTo = new ${className}Elem${typesUse}(this)
        |  }
        |  // 4) constructor and deconstructor
        |  abstract class ${className}CompanionAbs extends CompanionBase[${className}CompanionAbs] with ${className}Companion {
        |    override def toString = "$className"
        |${(fields.length != 1).opt(s"""
        |    def apply${typesDecl}(p: Rep[${className}Data${typesUse}])${implicitArgs}: Rep[$className${typesUse}] =
        |      iso$className${useImplicits}.to(p)""".stripAndTrim)}
        |    def apply${typesDecl}(${fieldsWithType.rep()})${implicitArgs}: Rep[$className${typesUse}] =
        |      mk$className(${fields.rep()})
        |    def unapply${typesWithElems}(p: Rep[$className${typesUse}]) = unmk$className(p)
        |    ${extractSqlQueries(c.body)}
        |  }
        |  def $className: Rep[${className}CompanionAbs]
        |  implicit def proxy${className}Companion(p: Rep[${className}CompanionAbs]): ${className}CompanionAbs = {
        |    proxyOps[${className}CompanionAbs](p)
        |  }
        |
        |  class ${className}CompanionElem extends CompanionElem[${className}CompanionAbs] {
        |    lazy val tag = weakTypeTag[${className}CompanionAbs]
        |    protected def getDefaultRep = $className
        |  }
        |  implicit lazy val ${className}CompanionElem: ${className}CompanionElem = new ${className}CompanionElem
        |
        |  implicit def proxy$className${typesDecl}(p: Rep[$className${typesUse}]): ${className}${typesUse} =
        |    proxyOps[${className}${typesUse}](p)
        |
        |  implicit class Extended$className${typesDecl}(p: Rep[$className${typesUse}])$implicitArgs {
        |    def toData: Rep[${className}Data${typesUse}] = iso$className${useImplicits}.from(p)
        |  }
        |
        |  // 5) implicit resolution of Iso
        |  implicit def iso$className${typesDecl}${implicitArgs}: Iso[${className}Data${typesUse}, $className${typesUse}] =
        |    new ${className}Iso${typesUse}
        |
        |  // 6) smart constructor and deconstructor
        |  def mk$className${typesDecl}(${fieldsWithType.rep()})${implicitArgs}: Rep[$className${typesUse}]
        |  def unmk$className${typesWithElems}(p: Rep[$className${typesUse}]): Option[(${fieldTypes.opt(fieldTypes => fieldTypes.rep(t => s"Rep[$t]"), "Rep[Unit]")})]
        |""".stripAndTrim
      }

      s"""
       |// Abs -----------------------------------
       |trait ${module.name}Abs extends ${config.baseContextTrait} with ${module.name} {
       |  ${module.selfType.opt(t => s"self: ${t.tpe} =>")}
       |${entityProxy(templateData)}
       |$proxyBT
       |$baseTypeElem
       |
       |${if (templateData.isContainer1) familyContainer(templateData) else ""}
       |${familyElem(templateData)}
       |$sqlSchema
       |
       |$sqlQueries
       |
       |$companionAbs
       |
       |${subEntities.mkString("\n\n")}
       |
       |${concreteClasses.mkString("\n\n")}
       |}
       |""".stripAndTrim
    }

    def getSClassSeq(c: SClassDef) = {
      val className = c.name
      val templateData = ConcreteClassTemplateData(module, c)
      val typesDecl = templateData.tpeArgDeclString
      val typesUse = templateData.tpeArgUseString
      val typesWithElems = templateData.boundedTpeArgString(false)
      val fields = c.args.argNames
      val fieldsWithType = c.args.argNamesAndTypes
      val traitWithTypes = templateData.firstAncestorType
      val implicitArgsDecl = templateData.implicitArgsDecl

      val externalMethods = module.entityOps.getMethodsWithAnnotation(ExternalAnnotation)
      val externalMethodsStr = filterByExplicitDeclaration(externalMethods).rep(md => externalSeqMethod(md, true), "\n    ")

      val userTypeDefs =
        s"""
         |  case class Seq$className${typesDecl}
         |      (${fieldsWithType.rep(f => s"override val $f")})
         |      ${implicitArgsDecl}
         |    extends $className${typesUse}(${fields.rep()})${c.selfType.opt(t => s" with ${t.tpe}")}
         |       ${module.seqDslImpl.opt(_ => s"with Seq$entityName${typesUse}")} with UserTypeSeq[$traitWithTypes, $className${typesUse}] {
         |    lazy val selfType = element[${className}${typesUse}].asInstanceOf[Elem[$traitWithTypes]]
         |    $externalMethodsStr
         |  }
         |  lazy val $className = new ${className}CompanionAbs with UserTypeSeq[${className}CompanionAbs, ${className}CompanionAbs] {
         |    lazy val selfType = element[${className}CompanionAbs]
         |  }""".stripAndTrim

      val constrDefs =
        s"""
         |  def mk$className${typesDecl}
         |      (${fieldsWithType.rep()})$implicitArgsDecl =
         |      new Seq$className${typesUse}(${fields.rep()})${c.selfType.opt(t => s" with ${t.tpe}")}
         |  def unmk$className${typesWithElems}(p: Rep[$className${typesUse}]) =
         |    Some((${fields.rep(f => s"p.$f")}))
         |""".stripAndTrim

      s"""$userTypeDefs\n\n$constrDefs"""
    }

    def getSClassExp(c: SClassDef) = {
      val className = c.name
      val d = ConcreteClassTemplateData(module, c)
      val typesDecl = d.tpeArgDeclString
      val typesUse = d.tpeArgUseString
      val typesWithElems = d.boundedTpeArgString(false)
      val fields = c.args.argNames
      val fieldsWithType = c.args.argNamesAndTypes
      val traitWithTypes = d.firstAncestorType
      val implicitArgsDecl = d.implicitArgsDecl
      val userTypeNodeDefs =
        s"""
         |  case class Exp$className${typesDecl}
         |      (${fieldsWithType.rep(f => s"override val $f")})
         |      ${implicitArgsDecl}
         |    extends $className${typesUse}(${fields.rep()})${c.selfType.opt(t => s" with ${t.tpe}")} with UserTypeDef[$traitWithTypes, $className${typesUse}] {
         |    lazy val selfType = element[$className${typesUse}].asInstanceOf[Elem[$traitWithTypes]]
         |    override def mirror(t: Transformer) = Exp$className${typesUse}(${fields.rep(f => s"t($f)")})
         |  }
         |
         |  lazy val $className: Rep[${className}CompanionAbs] = new ${className}CompanionAbs with UserTypeDef[${className}CompanionAbs, ${className}CompanionAbs] {
         |    lazy val selfType = element[${className}CompanionAbs]
         |    override def mirror(t: Transformer) = this
         |  }
         |
         |${methodExtractorsString(c)}
         |""".stripAndTrim

      val constrDefs =
        s"""
         |  def mk$className${typesDecl}
         |    (${fieldsWithType.rep()})$implicitArgsDecl =
         |    new Exp$className${typesUse}(${fields.rep()})${c.selfType.opt(t => s" with ${t.tpe}")}
         |  def unmk$className${typesWithElems}(p: Rep[$className${typesUse}]) =
         |    Some((${fields.rep(f => s"p.$f")}))
         |""".stripAndTrim

      s"""$userTypeNodeDefs\n\n$constrDefs"""
    }

    def baseTypeElem(ctx: String) = optBT.opt(bt =>
      if (tyArgsDecl.isEmpty) {
        s"""
          |  implicit lazy val ${bt.name}Element: Elem[${bt.name}] = new ${ctx}BaseElemEx[${bt.name}, $entityName](element[$entityName])(weakTypeTag[${bt.name}], ${getDefaultOfBT(bt)})
          |""".stripAndTrim
      }
      else {
        s"""
          |  implicit def ${bt.name}Element${typesWithElemsAndTags}: Elem[$entityNameBT${typesUse}] = new ${ctx}BaseElemEx[$entityNameBT${typesUse}, $entityName${typesUse}](element[$entityName${typesUse}])(weakTypeTag[$entityNameBT${typesUse}], ${getDefaultOfBT(bt)})
          |""".stripAndTrim
      })

    def getTraitSeq = {
      val e = module.entityOps
      val entityName = e.name
      val classesSeq = for { c <- module.concreteSClasses } yield getSClassSeq(c)
      val proxyBTSeq = optBT.opt(bt =>
        s"""
         |  // override proxy if we deal with BaseTypeEx
         |  //override def proxy$entityNameBT${typesWithElems}(p: Rep[$entityNameBT${typesUse}]): $entityName$typesUse =
         |  //  proxyOpsEx[$entityNameBT${typesUse},$entityName${typesUse}, Seq${entityName}Impl$typesUse](p, bt => Seq${entityName}Impl(bt))
         |""".stripAndTrim
      )
      val baseTypeToWrapperConvertionSeq = optBT.opt(bt =>
        s"""
         |  implicit def wrap${entityNameBT}To$entityName${typesWithElems}(v: $entityNameBT${typesUse}): $entityName$typesUse = ${entityName}Impl(v)
         |""".stripAndTrim
      )

      val companionMethods = getCompanionMethods.opt { case (constrs, methods) =>
        filterByExplicitDeclaration(constrs).rep(md => externalSeqConstructor(md), "\n    ") +
        filterByExplicitDeclaration(methods).rep(md => externalSeqMethod(md, false), "\n    ")
      }

      s"""
       |// Seq -----------------------------------
       |trait ${module.name}Seq extends ${module.name}Dsl with ${config.seqContextTrait} {
       |  ${module.selfType.opt(t => s"self: ${t.tpe}Seq =>")}
       |  lazy val $entityName: Rep[${entityName}CompanionAbs] = new ${entityName}CompanionAbs with UserTypeSeq[${entityName}CompanionAbs, ${entityName}CompanionAbs] {
       |    lazy val selfType = element[${entityName}CompanionAbs]
       |    $companionMethods
       |  }
       |
       |  $proxyBTSeq
       |
       |  ${baseTypeElem("Seq")}
       |
       |${classesSeq.mkString("\n\n")}
       |
       |$baseTypeToWrapperConvertionSeq
       |}
       |""".stripAndTrim
    }

    def familyView(e: EntityTemplateData) = {
      val typesDecl = e.tpeArgDeclString
      val entityElemType = s"${entityName}Elem[${e.typesUsePref}_,${e.entityType}]"
      s"""
        |  case class View${e.name}[A, B](source: Rep[${e.name}[A]])(iso: Iso1[A, B, ${e.name}])
        |    extends View1[A, B, ${e.name}](iso) {
        |    def copy(source: Rep[${e.name}[A]]) = View${e.name}(source)(iso)
        |    override def toString = s"View${e.name}[${"${innerIso.eTo.name}"}](${"$source"})"
        |    override def equals(other: Any) = other match {
        |      case v: View${e.name}[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
        |      case _ => false
        |    }
        |  }
        |""".stripAndTrim
    }

    def emitContainerRules(e: EntityTemplateData) = {
      val syn = e.entityRepSynonim
      if (e.isContainer1) {
        s"""
          |    case ${e.name}Methods.map(xs, Def(l: Lambda[_, _])) if l.isIdentity => xs
          |    case ${e.name}Methods.map(t: ${e.name}MapArgs[_,c] @unchecked) => t match {
          |      case (xs: ${syn.name}[a]@unchecked, f @ Def(Lambda(_, _, _, UnpackableExp(_, iso: Iso[b, c])))) => {
          |        val f1 = f.asRep[a => c]
          |        implicit val eA = xs.elem.eItem
          |        implicit val eB = iso.eFrom
          |        val s = xs.map( fun { x =>
          |          val tmp = f1(x)
          |          iso.from(tmp)
          |        })
          |        val res = View${e.name}(s)(${e.name}Iso(iso))
          |        res
          |      }
          |      case (Def(view: View${e.name}[a, b]), _) => {
          |        val iso = view.innerIso
          |        val ff = t._2.asRep[b => c]
          |        implicit val eA = iso.eFrom
          |        implicit val eB = iso.eTo
          |        implicit val eC = ff.elem.eRange
          |        view.source.map(fun { x => ff(iso.to(x))})
          |      }
          |      case _ =>
          |        super.rewriteDef(d)
          |    }
          |    case view1@View${e.name}(Def(view2@View${e.name}(arr))) => {
          |      val compIso = composeIso(view2.innerIso, view1.innerIso)
          |      implicit val eAB = compIso.eTo
          |      View${e.name}(arr)(${e.name}Iso(compIso))
          |    }
           """.stripMargin
      }
      else ""
    }

    def emitRewriteDef(e: EntityTemplateData) = {
      if (e.isContainer1) {
        s"""
        |  object UserType${e.name} {
        |    def unapply(s: Exp[_]): Option[Iso[_, _]] = {
        |      s.elem match {
        |        case e: ${e.name}Elem[a,from,to] => e.eItem match {
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
        |  type ${e.name}MapArgs[A,B] = (Rep[${e.name}[A]], Rep[A => B])
        |  ${(!e.entityRepSynonimOpt.isDefined).opt(e.emitEntityRepSynonim)}
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
      val e = module.entityOps
      val entityName = e.name
      val td = EntityTemplateData(module, e)

      val concreteClassesString = module.concreteSClasses.map(getSClassExp)
      
      s"""
       |// Exp -----------------------------------
       |trait ${module.name}Exp extends ${module.name}Dsl with ${config.stagedContextTrait} {
       |  ${module.selfType.opt(t => s"self: ${t.tpe}Exp =>")}
       |  lazy val $entityName: Rep[${entityName}CompanionAbs] = new ${entityName}CompanionAbs with UserTypeDef[${entityName}CompanionAbs, ${entityName}CompanionAbs] {
       |    lazy val selfType = element[${entityName}CompanionAbs]
       |    override def mirror(t: Transformer) = this
       |  }
       |
       |${if (td.isContainer1) familyView(td) else ""}
       |${baseTypeElem("Exp")}
       |${concreteClassesString.mkString("\n\n")}
       |
       |${methodExtractorsString(e)}

       |${emitRewriteDef(td)}
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
          (m.explicitArgs.collect { case SMethodArg(_,_, name, STpeFunc(_, _), _, _) => name} match {
            case Seq() => None
            case nonEmpty => Some(s"Method has function arguments ${nonEmpty.mkString(", ")}")
          }).orElse {
            m.tpeRes.filter(!_.isRep(module, config)).map {
              returnTpe => s"Method's return type $returnTpe is not a Rep"
            }
          }
        }

        def methodExtractor(m: SMethodDef) = {
          reasonToSkipMethod(m) match {
            case Some(reason) =>
              // TODO consider if we really want a warning here. Might depend on severity of reason.
              // val warning = s"Cannot generate matcher for method `${e.name}.${m.name}`: $reason"
              // logger.warn(warning)
              s"    // WARNING: Cannot generate matcher for method `${m.name}`: $reason"
            case _ =>
              // DummyImplicit and Overloaded* are ignored, since
              // their values are never useful
              val methodArgs = m.argSections.flatMap(_.args).takeWhile { arg =>
                arg.tpe match {
                  case STraitCall(name, _) =>
                    !(name == "DummyImplicit" || name.startsWith("Overloaded"))
                  case _ => true
                }
              }
              val typeVars = (e.tpeArgs ++ m.tpeArgs).map(_.declaration).toSet
              val returnType = {
                val receiverType = s"Rep[${e.name + typeArgString(e.tpeArgs.map(_.name))}]"
                val argTypes = methodArgs.map(_.tpe.toString)
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
                val methodArgsPattern = if (methodArgs.isEmpty) "_" else s"Seq(${methodArgs.rep(_.name, ", ")}, _*)"
                val typeArgsNum =
                  if (isCompanion) {
                    0
                  } else if (e.isInstanceOf[STraitDef]) {
                    e.tpeArgs.length + 2
                  } else {
                    e.tpeArgs.length
                  }
                val traitElem = s"${e.name}Elem${typeArgString(Seq.fill(typeArgsNum)("_"))}"
                val annotationCheck =
                  if (overloadIdsByName(m.name).size == 1) {
                    // nothing to check if method isn't overloaded
                    ""
                  } else {
                    overloadId match {
                      case None =>
                        "&& method.getAnnotation(classOf[scalan.OverloadId]) == null"
                      case Some(id) =>
                        s""" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "$id" }"""
                    }
                  }

                s"MethodCall(receiver, method, $methodArgsPattern, _) if " + (if (e.isHighKind) {
                  s"""(receiver.elem match { case ve: ViewElem[_, _] => ve match { case _: $traitElem => true; case _ => false }; case _ => false })"""
                } else {
                  s"receiver.elem.isInstanceOf[$traitElem]"
                }) + s""" && method.getName == "${m.name}"$annotationCheck"""
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
           |${methods.filterNot(_.isElem).map(methodExtractor).mkString("\n\n")}
           |  }""".stripMargin
      }

      s"""${methodExtractorsString1(e, false)}
         |
         |${e.companion.opt(methodExtractorsString1(_, true))}""".stripMargin
    }

    def getFileHeader = {
      s"""
      |package ${module.packageName}
      |package impl
      |
      |${(module.imports ++ config.extraImports.map(SImportStat(_))).rep(i => s"import ${i.name}", "\n")}
      |""".stripAndTrim
    }

    def getImplFile: String = {
      val topLevel = List(
        getFileHeader,
        getTraitAbs,
        getTraitSeq,
        getTraitExp
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
