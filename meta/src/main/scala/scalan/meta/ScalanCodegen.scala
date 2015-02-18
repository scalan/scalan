/**
 * User: Alexander Slesarenko
 * Date: 11/16/13
 */
package scalan.meta

import scalan.util.{StringUtil, ScalaNameUtil}
import ScalanAst._

object Extensions {
  implicit class IterableExtensions[A](val it: Iterable[A]) extends AnyVal
  {
    def opt(show: Iterable[A] => String = _.mkString, default: String = ""): String = 
      if (it.isEmpty) default else show(it)

    def rep(show: A => String = _.toString, sep: String = ", "): String = it.map(show).mkString(sep)

    def enumTypes(show: Int => String) = (1 to it.size).map(show)
  }

  def all[A](a: A): String = a.toString

  implicit class OptionExtensions[A](val opt: Option[A]) extends AnyVal {
    def opt(show: A => String = _.toString, default: String = ""): String = opt match {
      case None => default
      case Some(a) => show(a)
    }
  }
  
  implicit class BooleanExtensions(val opt: Boolean) extends AnyVal {
    def opt(show: => String, default: => String = ""): String = if(opt) show else default
  }
  
  implicit class StringExtensions(val str: String) extends AnyVal {
    def stripAndTrim = str.stripMargin.stripPrefix("\n").stripLineEnd
  }
}

trait ScalanCodegen extends ScalanParsers { ctx: EntityManagement =>

  class EntityFileGenerator(module: SEntityModuleDef, config: CodegenConfig) {
    import Extensions._
    def getTpeArgDecls(args: STpeArgs) = args.map(_.declaration)
    def getTpeArgUse(args: STpeArgs) = args.map(_.name)

    def getTpeArgDeclString(args: STpeArgs) = getTpeArgDecls(args).opt(tyArgs => s"[${tyArgs.rep(t => t)}]")
    def getTpeArgUseString(args: STpeArgs) = getTpeArgUse(args).opt(tyArgs => s"[${tyArgs.rep(t => t)}]")

    def getBoundedTpeArgString(args: STpeArgs, withTags: Boolean = false) =
      args.opt(args =>
        s"[${args.rep(t => (if (t.isHighKind) s"${t.declaration}:Cont" else s"${t.name}:Elem") +
          withTags.opt(":WeakTypeTag"))}]")

    abstract class TemplateData(val name: String, val tpeArgs: List[STpeArg]) {
      val tpeArgDecls = getTpeArgDecls(tpeArgs)
      val tpeArgUses = getTpeArgUse(tpeArgs)
      val tpeArgDeclString = getTpeArgDeclString(tpeArgs)
      val tpeArgUseString = getTpeArgUseString(tpeArgs)
      def boundedTpeArgString(withTags: Boolean = false) = getBoundedTpeArgString(tpeArgs, withTags)
      def nameWithArgs = name + tpeArgUseString
    }
    
    class EntityTemplateData(name: String, tpeArgs: List[STpeArg]) extends TemplateData(name, tpeArgs)
    
    object EntityTemplateData {
      def apply(t: STraitDef) = new EntityTemplateData(t.name, t.tpeArgs)
    }
    
    class ConcreteClassTemplateData(name: String, val args: List[SClassArg], implArgs: List[SClassArg], tpeArgs: List[STpeArg], val baseType: STraitCall) extends TemplateData(name, tpeArgs) {
      val argNames = args.map(a => a.name)
      val argNamesAndTypes = args.map(a => s"${a.name}: ${a.tpe}")
      val argUnrepTypes = args.map(a => a.tpe.unRep(module, config) match {
        case Some(t) => t
        case None => sys.error(s"Invalid field $a. Fields of concrete classes should be of type Rep[T] for some T.")
      })
      val implicitArgs = implArgs.opt(args => s"(implicit ${args.rep(a => s"${a.name}: ${a.tpe}")})")
      val useImplicits = implArgs.opt(args => s"(${args.map(_.name).rep(a => a)})")
      val implicitSignature = implArgs.opt(args => s"(implicit ${args.rep(a => s"${a.name}: ${a.tpe}")})")
    }
    
    object ConcreteClassTemplateData {
      def apply(c: SClassDef) = new ConcreteClassTemplateData(c.name, c.args, c.implicitArgs, c.tpeArgs, c.ancestors.head)
    }

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
      case tc @ STraitCall(name, args) => {
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

    val templateData = EntityTemplateData(module.entityOps)
    val optBT = module.entityOps.optBaseType
    val entityName = templateData.name
    val entityNameBT = optBT.map(_.name).getOrElse(templateData.name)
    val tyArgsDecl = templateData.tpeArgDecls
    val tyArgsUse = templateData.tpeArgUses
    val typesDecl = templateData.tpeArgDeclString
    val typesUse = templateData.tpeArgUseString
    val typesWithElems = templateData.boundedTpeArgString(false)
    val typesWithElemsAndTags = templateData.boundedTpeArgString(true)

    def getCompanionOpt = for { bt <- optBT; comp <- module.entityOps.companion } yield comp

    def getCompanionMethods = getCompanionOpt.map { comp =>
      val externalConstrs = comp.getMethodsWithAnnotation(ExternalConstructor)
      val externalMethods = comp.getMethodsWithAnnotation(ExternalMethod)
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

    def methodArgSection(sec: SMethodArgs) = {
      s"(${sec.args.rep(a => s"${a.name}: ${a.tpe}")})"
    }
    def methodArgsUse(sec: SMethodArgs) = {
      s"(${sec.args.rep(a => {
        if (a.tpe.isTupledFunc)
          s"scala.Function.untupled(${a.name})"
        else if (a.annotations.contains(ArgList))
          s"${a.name}: _*"
        else
          s"${a.name}"
      })})"
    }

    def externalMethod(md: SMethodDef) = {
      val msgExplicitRetType = "External methods should be declared with explicit type of returning value (result type)"
      lazy val msgRepRetType = s"Invalid method $md. External methods should have return type of type Rep[T] for some T."
      val tyRet = md.tpeRes.getOrElse(!!!(msgExplicitRetType))
      val allArgs = md.argSections.flatMap(_.args)
      val typesDecl = getBoundedTpeArgString(md.tpeArgs)
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
      val typesDecl = getBoundedTpeArgString(md.tpeArgs)
      s"""
        |    def ${md.name}$typesDecl${md.argSections.rep(methodArgSection(_), "")}: ${tyRet.toString} =
        |      newObjEx(classOf[$entityNameBT${typesUse}], List(${allArgs.rep(a => s"${a.name}.asRep[Any]")}))
        |""".stripMargin
    }

    def externalSeqMethod(md: SMethodDef, isInstance: Boolean) = {
      val msgExplicitRetType = "External methods should be declared with explicit type of returning value (result type)"
      val tyRet = md.tpeRes.getOrElse(!!!(msgExplicitRetType))
      val typesDecl = getBoundedTpeArgString(md.tpeArgs)
      val typesUse = getTpeArgUseString(md.tpeArgs)
      s"""
        |    override def ${md.name}$typesDecl${md.argSections.rep(methodArgSection(_), "")}: ${tyRet.toString} =
        |      ${if (isInstance) "wrappedValueOfBaseType" else entityNameBT}.${md.name}$typesUse${md.argSections.rep(methodArgsUse(_), "")}
        |""".stripMargin
    }

    def externalSeqConstructor(md: SMethodDef) = {
      val msgExplicitRetType = "External constructors should be declared with explicit type of returning value (result type)"
      val tyRet = md.tpeRes.getOrElse(!!!(msgExplicitRetType))
      val typesDecl = getBoundedTpeArgString(md.tpeArgs)
      s"""
        |    override def ${md.name}$typesDecl${md.argSections.rep(methodArgSection(_), "")}: ${tyRet.toString} =
        |      new $entityNameBT${typesUse}${md.argSections.rep(methodArgsUse(_), "")}
        |""".stripMargin
    }

    def getTraitAbs = {
      val entityCompOpt = module.entityOps.companion
      val companionName = s"${entityName}Companion"
      def entityProxy(entityName: String, typesDecl: String, typesUse: String) =
        s"""
        |  // single proxy for each type family
        |  implicit def proxy$entityName${typesDecl}(p: Rep[$entityName${typesUse}]): $entityName$typesUse =
        |    proxyOps[$entityName${typesUse}](p)
        |""".stripAndTrim


      val proxyBT = optBT.opt(bt => {
        s"""
        |  // BaseTypeEx proxy
        |  implicit def proxy$entityNameBT${typesWithElems}(p: Rep[$entityNameBT${typesUse}]): $entityName$typesUse =
        |    proxyOps[$entityName${typesUse}](p.asRep[$entityName${typesUse}])
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

      def familyElem(entityName: String, tyArgsDecl: List[String], typesUse: String) =
        s"""
        |  abstract class ${entityName}Elem[${tyArgsDecl.opt(tyArgs => s"${tyArgsDecl.rep(t => t)}, ")}From, To <: $entityName$typesUse](iso: Iso[From, To]) extends ViewElem[From, To]()(iso) {
        |    override def convert(x: Rep[Reifiable[_]]) = convert$entityName(x.asRep[$entityName$typesUse])
        |    def convert$entityName(x : Rep[$entityName$typesUse]): Rep[To]
        |  }
        |""".stripAndTrim


      val companionMethods = getCompanionMethods.opt { case (constrs, methods) =>
        constrs.rep(md => externalConstructor(md), "\n    ") +
        methods.rep(md => externalMethod(md), "\n    ")
      }


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
        |  }
        |  def $entityName: Rep[${companionName}Abs]
        |  implicit def proxy$companionName(p: Rep[${companionName}]): ${companionName} = {
        |    proxyOps[${companionName}](p)
        |  }
        |""".stripAndTrim

      val subEntities = for { entity <- module.entities.drop(1) } yield {
        val templateData = EntityTemplateData(entity)
        val entityName = templateData.name
        val tyArgsDecl = templateData.tpeArgDecls
        //val tyArgsUse = templateData.tpeArgUses
        val typesDecl = templateData.tpeArgDeclString
        val typesUse = templateData.tpeArgUseString
        //val typesWithElems = templateData.boundedTpeArgString(false)
        //val typesWithElemsAndTags = templateData.boundedTpeArgString(true)
        s"""
        |${entityProxy(entityName, typesDecl, typesUse)}
        |${familyElem(entityName, tyArgsDecl, typesUse)}
        |""".stripMargin
      }

      val concreteClasses = for { c <- module.concreteSClasses } yield {
        val className = c.name
        val concTemplateData = ConcreteClassTemplateData(c)
        val typesDecl = concTemplateData.tpeArgDeclString
        val typesUse = concTemplateData.tpeArgUseString
        val typesWithElems = concTemplateData.boundedTpeArgString(false)
        val fields = concTemplateData.argNames
        val fieldsWithType = concTemplateData.argNamesAndTypes
        val fieldTypes = concTemplateData.argUnrepTypes
        val implicitArgs = concTemplateData.implicitArgs
        val useImplicits = concTemplateData.useImplicits
        val implicitArgsWithVals = c.implicitArgs.opt(args => s"(implicit ${args.rep(a => s"val ${a.name}: ${a.tpe}")})")
        val parent     = c.ancestors.head
        val parentArgs = parent.tpeSExprs.map(_.toString)
        val parentArgsStr = parentArgs.map(_ + ", ").mkString
        lazy val defaultImpl = optBT.opt(bt => {
          val externalMethods = module.entityOps.getMethodsWithAnnotation(ExternalMethod)
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

        def converterBody(entity: STraitDef, conc: SClassDef) = {
          val availableFields = entity.getAvailableFields(module)
          val concFields = conc.args.map(_.name)
          val missingFields = concFields.filterNot(availableFields.contains(_))
          if (missingFields.isEmpty) {
            val args = concFields.rep(f => s"x.$f")
            s"$className($args)"
          }
          else {
            val msg = s"from ${entity.name} to ${conc.name}: missing fields ${missingFields}"
            println("Warning: cannot generate converter " + msg)
            "// Converter is not generated by meta\n" +
            "!!!(\"Cannot convert " + msg + "\")"
          }
        }

        s"""
        |$defaultImpl
        |  // elem for concrete class
        |  class ${className}Elem${typesWithElems}(iso: Iso[${className}Data${typesUse}, $className${typesUse}]) extends ${parent.name}Elem[${parentArgsStr}${className}Data${typesUse}, $className${typesUse}](iso) {
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
       |${entityProxy(entityName, typesDecl, typesUse)}
       |$proxyBT
       |$baseTypeElem
       |
       |${familyElem(entityName, tyArgsDecl, typesUse)}
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
      val templateData = ConcreteClassTemplateData(c)
      val typesDecl = templateData.tpeArgDeclString
      val typesUse = templateData.tpeArgUseString
      val typesWithElems = templateData.boundedTpeArgString(false)
      val fields = templateData.argNames
      val fieldsWithType = templateData.argNamesAndTypes
      val traitWithTypes = templateData.baseType
      val implicitArgs = templateData.implicitArgs
      val implicitSignature = templateData.implicitSignature

      val externalMethods = module.entityOps.getMethodsWithAnnotation(ExternalMethod)
      val externalMethodsStr = filterByExplicitDeclaration(externalMethods).rep(md => externalSeqMethod(md, true), "\n    ")

      val userTypeDefs =
        s"""
         |  case class Seq$className${typesDecl}
         |      (${fieldsWithType.rep(f => s"override val $f")})
         |      ${implicitSignature}
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
         |      (${fieldsWithType.rep()})$implicitArgs =
         |      new Seq$className${typesUse}(${fields.rep()})${c.selfType.opt(t => s" with ${t.tpe}")}
         |  def unmk$className${typesWithElems}(p: Rep[$className${typesUse}]) =
         |    Some((${fields.rep(f => s"p.$f")}))
         |""".stripAndTrim

      s"""$userTypeDefs\n\n$constrDefs"""
    }

    def getSClassExp(c: SClassDef) = {
      val className = c.name
      val td = ConcreteClassTemplateData(c)
      val typesDecl = td.tpeArgDeclString
      val typesUse = td.tpeArgUseString
      val typesWithElems = td.boundedTpeArgString(false)
      val fields = td.argNames
      val fieldsWithType = td.argNamesAndTypes
      val traitWithTypes = td.baseType
      val implicitArgs = td.implicitArgs
      val implicitSignature = td.implicitSignature
      val userTypeNodeDefs =
        s"""
         |  case class Exp$className${typesDecl}
         |      (${fieldsWithType.rep(f => s"override val $f")})
         |      ${implicitSignature}
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
         |    (${fieldsWithType.rep()})$implicitArgs =
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
      val defs = for { c <- module.concreteSClasses } yield getSClassSeq(c)
      val proxyBT = optBT.opt(bt =>
        s"""
         |  // override proxy if we deal with BaseTypeEx
         |  override def proxy$entityNameBT${typesWithElems}(p: Rep[$entityNameBT${typesUse}]): $entityName$typesUse =
         |    proxyOpsEx[$entityNameBT${typesUse},$entityName${typesUse}, Seq${entityName}Impl$typesUse](p, bt => Seq${entityName}Impl(bt))
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
       |  $proxyBT
       |
       |  ${baseTypeElem("Seq")}
       |
       |${defs.mkString("\n\n")}
       |}
       |""".stripAndTrim
    }

    def getTraitExp = {
      val e = module.entityOps
      val entityName = e.name
      val td = EntityTemplateData(e)

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
       |${baseTypeElem("Exp")}
       |
       |${concreteClassesString.mkString("\n\n")}
       |
       |${methodExtractorsString(e)}
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
          (m.explicitArgs.collect { case SMethodArg(name, STpeFunc(_, _), _, _) => name} match {
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
                  s"""receiver.elem match { case _: $traitElem => true; case _ => false }"""
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
