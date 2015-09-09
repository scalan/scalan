package scalan.meta

import java.io.{ObjectInputStream, ByteArrayInputStream, ObjectOutputStream, ByteArrayOutputStream}
import java.util.zip.{GZIPInputStream,GZIPOutputStream}

import scalan.util.{ThreadContextClassLoaderObjectInputStream, StringUtil, ScalaNameUtil}
import scala.annotation.tailrec
import ScalanAst._

object ScalanCodegen extends SqlCompiler with ScalanAstExtensions {
  import Base.!!!
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
    val implicitArgs = entity.implicitArgs.args
    val implicitArgsDecl = implicitArgs.opt(args => s"(implicit ${args.rep(a => s"${a.name}: ${a.tpe}")})")
    val implicitArgsUse = implicitArgs.opt(args => s"(${args.rep(_.name)})")
    val optBT = entity.optBaseType
    val entityNameBT = optBT.map(_.name).getOrElse(name)
    val firstAncestorType = entity.ancestors.headOption
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
    def isFunctor = tpeArgs.length == 1 && entity.hasAnnotation(FunctorTypeAnnotation)

    def isWrapper = firstAncestorType match {
      case Some(STraitCall("TypeWrapper", _)) => true
      case _ => false
    }

    def boundedTpeArgString(withTags: Boolean = false) = tpeArgs.getBoundedTpeArgString(withTags)

    def tpeArgsImplicitDecl(typeClass: String) = {
      tpeArgs.opt(args =>
        s"(implicit ${args.rep(t => s"ev${t.name}: $typeClass[${t.name}]")})")
    }

    def getImplicitArgName(tyArgName: String): Either[String,String] = {
      val imp = implicitArgs.collect(a => a.tpe match {
        case STraitCall("Elem",List(STpeAnnotated(STraitCall(`tyArgName`,_),_))) => Left(a.name)
        case STraitCall("Elem",List(STraitCall(`tyArgName`,_))) => Left(a.name)
        case STraitCall(_,List(STraitCall(`tyArgName`,_))) => Right(a.name)
      }).headOption.get
      imp
    }
  }

  case class EntityTemplateData(m: SEntityModuleDef, t: STraitDef) extends TemplateData(m, t)

  case class ConcreteClassTemplateData(m: SEntityModuleDef, c: SClassDef) extends TemplateData(m, c) {
    val classType = name + tpeArgUseString
  }

  trait MatcherGenerator {

    object InternalFunctions {

      def whitespaces(title: String) = " " * (title.length + 2)
      def returnType(entity: STraitDef) = {
        val types = entity.tpeArgs.getTpeArgDecls
        types.lastOption match {
          case None => "R"
          case Some(tpe) => tpe + "1"
        }
      }
      def genericSignature(entity: STraitDef) = {
        entity.tpeArgs.getTpeArgDecls.opt(tyArgs => s"[${tyArgs.mkString(", ")}, ${returnType(entity)}]", "[R]")
      }
      def typesBraces(entity: STraitDef) = {
        val types = entity.tpeArgs.getTpeArgDecls
        types.opt(tyArgs => s"[${tyArgs.rep(t => t)}]")
      }
      def typesBraces(tpeArgs: List[STpeArg]) = {
        val types = tpeArgs.getTpeArgDecls
        types.opt(tyArgs => s"[${tyArgs.rep(t => t)}]")
      }
      def title(entity: STraitDef) = s"def match${entity.name}${genericSignature(entity)}"
      def lambdas(module: SEntityModuleDef, whitespace: String) = {
        val entity = module.entityOps
        val classes = module.concreteSClasses
        val types = typesBraces(entity)
        val retType = returnType(entity)
        classes.zipWithIndex.map { case (c, i) =>
          s"\n$whitespace(f$i: Rep[${c.name}$types] => Rep[$retType])"
        }.opt(specs => s"${specs.rep(t => t, s"")}") + s"\n$whitespace(fb: Rep[${entity.name}$types] => Rep[$retType])"
      }
      def fallback = s"      case _ => fb(x)"
      def declaration(module: SEntityModuleDef) = {
        val entity = module.entityOps
        val name = entity.name
        val types = typesBraces(entity)
        val titleDef = title(module.entityOps)
        s"""\n
          |  ${title(entity)}(x: Rep[$name$types])${lambdas(module, whitespaces(titleDef))}: Rep[${returnType(entity)}]
          |""".stripAndTrim
      }
    }
    import InternalFunctions.{declaration, fallback}

    def entityMatcherAbs(module: SEntityModuleDef) = {
      declaration(module)
    }
    def entityMatcherSeq(module: SEntityModuleDef) = {
      val classes = module.concreteSClasses
      val anyType = module.entityOps.tpeArgs.opt(_.map(_ => "_").mkString(", "))
      val cases = classes.zipWithIndex.map { case (c, i) =>
        s"      case x$i: ${c.name}$anyType => f$i(x$i)"
      }.opt(specs => s"${specs.rep(t => t, s"\n")}")
      val body = s""" = {
        |    x match {
        |$cases
        |$fallback
        |    }
        |  }"""
      s"""\n
        |  ${declaration(module)}$body
        |""".stripAndTrim
    }
    def entityMatcherExp(module: SEntityModuleDef) = {
      val classes = module.concreteSClasses
      val types = module.entityOps.tpeArgs.getTpeArgDeclString
      val anyType = module.entityOps.tpeArgs.opt(_.map(_ => "_").mkString(", "))
      val cases = classes.zipWithIndex.map { case (c, i) =>
        s"      case _: ${c.name}Elem$anyType => f$i(x.asRep[${c.name}$types])"
      }.opt(specs => s"${specs.rep(t => t, s"\n")}")
      val body = s""" = {
        |    x.elem.asInstanceOf[Elem[_]] match {
        |$cases
        |$fallback
        |    }
        |  }"""
      s"""\n
        |  ${declaration(module)}$body
        |""".stripAndTrim
    }
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
          gzip.close
          bos.close
          val str = javax.xml.bind.DatatypeConverter.printBase64Binary(bos.toByteArray)
          str
        }
        finally objOut.close()
      }
      finally gzip.close()
    }
    finally bos.close()
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
        }
        finally objIn.close()
      }
      finally gzip.close()
    }
    finally bis.close()
  }

  class EntityFileGenerator(module: SEntityModuleDef, config: CodegenConfig) extends MatcherGenerator {

    //import InternalFunctions._

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
      s"DefaultOf${tc.name}" + tc.tpeSExprs.opt(args => s"[${args.rep()}]")

    def zeroSExpr(t: STpeExpr): String = t match {
      case STpePrimitive(_, defaultValueString) => defaultValueString
      case STraitCall(name, args)
        if module.entityOps.tpeArgs.exists(a => a.name == name && a.isHighKind) =>
        val arg = args(0)
        arg match {
          case STraitCall(name2, _) if module.entityOps.tpeArgs.exists(a => a.name == name2) =>
            s"c$name.lift(${args.rep(a => s"e$a")}).defaultRepValue"
          case _ =>
            s"c$name.lift(${args.rep(a => s"element[$a]")}).defaultRepValue"
        }
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
      case STpeFunc(domain, range) => s"""fun { (x: Rep[$domain]) => ${zeroSExpr(range)} }"""
      case t => throw new IllegalArgumentException(s"Can't generate zero value for $t")
    }

    def typeArgString(typeArgs: Seq[String]) =
      if (typeArgs.isEmpty) "" else typeArgs.mkString("[", ", ", "]")

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
        if (a.tparams.isEmpty)
          s"element[$t]"
        else
          if (args.isEmpty)
            s"container[$t]"
          else
            s"element[$t]"
      case STraitCall(name, args) =>
        val method = entityElemMethodName(name)
        val argsStr = args.rep(tpeToElement(_, env))
        method + args.opt(_ => s"($argsStr)")
      case _ => sys.error(s"Don't know how to construct Element for type $t")
    }

    val templateData = EntityTemplateData(module, module.entityOps)
    val optBT = templateData.optBT
    val entityName = templateData.name
    val entityNameBT = optBT.map(_.name).getOrElse(templateData.name)
    val entityNameBT1 = optBT.getOrElse(templateData.name)
    val tyArgsDecl = templateData.tpeArgDecls
    val tyArgsUse = templateData.tpeArgUses
    val typesDecl = templateData.tpeArgDeclString
    val typesUse = templateData.tpeArgUseString
    val typesWithElems = templateData.boundedTpeArgString(false)

    def getCompanionOpt = for {bt <- optBT; comp <- module.entityOps.companion} yield comp

    def getCompanionMethods = getCompanionOpt.map { comp =>
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

    def methodArgSection(sec: SMethodArgs) = {
      val implicitKeyWord = sec.args.headOption match {
        case Some(arg) if arg.impFlag => "implicit "
        case _ => ""
      }
      s"($implicitKeyWord${sec.argNamesAndTypes(config).rep()})"
    }

    def methodArgsUse(sec: SMethodArgs) = {
      s"(${
        sec.args.rep(a => {
          if (a.tpe.isTupledFunc)
            s"scala.Function.untupled(${a.name})"
          else if (a.isArgList)
            s"${a.name}: _*"
          else if (a.isElemOrCont)
            s"${a.name}.classTag"
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
      val typesDecl = md.tpeArgs.getBoundedTpeArgString(false, md.argSections)
      val unrepRet = tyRet.unRep(module, config).getOrElse(!!!(msgRepRetType))
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
        |    ${if (md.body.isDefined) "override def" else "def"} ${md.name}$typesDecl${md.argSections.rep(methodArgSection(_), "")}: Rep[${unrepRet.toString}] =
        |      methodCallEx[$unrepRet](self,
        |        this.getClass.getMethod("${md.name}"$argClassesStr$elemClassesStr),
        |        List(${allArgs.rep(a => s"${a.name}.asInstanceOf[AnyRef]")}${compoundArgs.opt(", ")}${elemArgs.rep()}))
        |""".stripMargin
    }

    def externalConstructor(method: SMethodDef) = {
      def genConstr(md: SMethodDef) = {
        val msgExplicitRetType = "External constructors should be declared with explicit type of returning value (result type)"
        lazy val msgRepRetType = s"Invalid constructor declaration $md. External constructors should have return type of type Rep[T] for some T."
        val tyRet = md.tpeRes.getOrElse(!!!(msgExplicitRetType))
        val unrepRet = tyRet.unRep(module, config).getOrElse(!!!(msgRepRetType))
        val allArgs = md.argSections.flatMap(_.args)
        val typesDecl = md.tpeArgs.getBoundedTpeArgString(false, md.argSections)
        s"""
        |    def ${md.name}$typesDecl${md.argSections.rep(methodArgSection(_), "")}: Rep[${unrepRet.toString}] =
        |      newObjEx(classOf[$entityName${typesUse}], List(${allArgs.rep(a => s"${a.name}.asRep[Any]")}))
        |""".stripMargin
      }
      genConstr(method.copy(argSections = method.cleanedArgs))
    }

    def
    externalSeqMethod(
                       md: SMethodDef, isInstance: Boolean) = {
      val msgExplicitRetType = "External methods should be declared with explicit type of returning value (result type)"
      val tyRet = md.tpeRes.getOrElse(!!!(msgExplicitRetType))
      val unrepRet = tyRet.unRep(module, config).getOrElse(!!!(msgExplicitRetType))
      val typesDecl = md.tpeArgs.getBoundedTpeArgString(false, md.argSections)
      val typesUse = md.tpeArgs.getTpeArgUseString
      val methodHeader =
        s"""override def ${md.name}$typesDecl${md.argSections.rep(methodArgSection(_), "")}: Rep[${unrepRet.toString}] =
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

    def externalSeqConstructor(method: SMethodDef) = {
      def genConstr(md: SMethodDef) = {
        val msgExplicitRetType = "External constructors should be declared with explicit type of returning value (result type)"
        val tyRet = md.tpeRes.getOrElse(!!!(msgExplicitRetType))
        val unrepRet = tyRet.unRep(module, config).getOrElse(!!!(msgExplicitRetType))
        val typesDecl = md.tpeArgs.getBoundedTpeArgString(false, md.argSections)

        s"""
        |    override def ${md.name}$typesDecl${md.argSections.rep(methodArgSection(_), "")}: Rep[${unrepRet.toString}] =
        |      ${entityName}Impl(new $entityNameBT${typesUse}${md.argSections.rep(methodArgsUse(_), "")})
        |""".stripMargin
      }

      genConstr(method.copy(argSections = method.cleanedArgs))
    }

    def entityProxy(e: EntityTemplateData) = {
      val entityName = e.name
      val typesDecl = e.
        tpeArgDeclString
      val typesUse = e.tpeArgUseString
      s"""
        |  // single proxy for each type family
        |  implicit def proxy$entityName${typesDecl}(p: Rep[${e.entityType}]): ${e.entityType} = {
        |    proxyOps[${e.entityType}](p)(scala.reflect.classTag[${e.entityType}])
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

      val entityCompOpt = module.entityOps.companion
      val hasCompanion = entityCompOpt.isDefined
      val companionName = s"${entityName}Companion"
      val proxyBT = optBT.opt(bt => {
        s"""
        |  // TypeWrapper proxy
        |  //implicit def proxy$entityNameBT${typesWithElems}(p: Rep[$entityNameBT${typesUse}]): $entityName$typesUse =
        |  //  proxyOps[$entityName${typesUse}](p.asRep[$entityName${typesUse}])
        |
        |  implicit def unwrapValueOf$entityName${typesDecl}(w: Rep[$entityName${typesUse}]): Rep[$entityNameBT${typesUse}] = w.wrappedValueOfBaseType
        |""".stripAndTrim
      })

      val baseTypeElem = optBT.opt(bt =>
        if (tyArgsDecl.isEmpty) {
          s"""
          |  implicit def ${StringUtil.lowerCaseFirst(bt.name)}Element: Elem[${bt.name}]
          |""".stripAndTrim
        }
        else {
          s"""
          |  implicit def ${StringUtil.lowerCaseFirst(bt.name)}Element${typesWithElems}: Elem[$entityNameBT${typesUse}]
          |""".stripAndTrim
        })

      def familyContainer(e: EntityTemplateData) = {
        val typesDecl = e.tpeArgDeclString
        val entityElemType = s"${entityName}Elem[${e.typesUsePref}${e.entityType}]"
        val btContainer = optBT.opt(bt =>
          s"""
             |  implicit lazy val container${bt.name}: Cont[${bt.name}] = new Container[${bt.name}] {
             |    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[${bt.name}[A]]
             |    def lift[A](implicit evA: Elem[A]) = element[${bt.name}[A]]
             |  }
           """.stripMargin)

        s"""
        |  implicit def cast${e.name}Element${typesDecl}(elem: Elem[${e.entityType}]): $entityElemType = elem.asInstanceOf[$entityElemType]
        |
        |  $btContainer
        |
        |  implicit lazy val container${e.name}: Cont[${e.name}]${e.isFunctor.opt(s" with Functor[${e.name}]")} = new Container[${e.name}]${e.isFunctor.opt(s" with Functor[${e.name}]")} {
        |    def tag${typesDecl}${e.tpeArgsImplicitDecl("WeakTypeTag")} = weakTypeTag[${e.entityType}]
        |    def lift${typesDecl}${e.tpeArgsImplicitDecl("Elem")} = element[${e.entityType}]
        |    ${e.isFunctor.opt(s"def map[A:Elem,B:Elem](xs: Rep[${e.name}[A]])(f: Rep[A] => Rep[B]) = xs.map(fun(f))")}
        |  }
        |  case class ${e.name}Iso[A,B](iso: Iso[A,B]) extends Iso1[A, B, ${e.name}](iso) {
        |    def from(x: Rep[${e.name}[B]]) = x.map(iso.fromFun)
        |    def to(x: Rep[${e.name}[A]]) = x.map(iso.toFun)
        |    lazy val defaultRepTo = ${e.name}.empty[B]
        |  }
        |""".stripAndTrim
      }

      def familyElem(e: EntityTemplateData) = {
        val entityName = e.name
        val typesUse = e.tpeArgUseString
        val cont = e.isContainer1
        val wildcardElem = s"${e.name}Elem[${"_, " * e.tpeArgDecls.length}_]"
        val isW = e.isWrapper
        val (optParent, parentElemName, parentTyArgs, parentArgs) = e.firstAncestorType match {
          case Some(STraitCall("Reifiable", _)) =>
            (None, s"EntityElem${cont.opt("1")}",
             s"[${cont.opt(e.typesUsePref)}To${cont.opt(s", $entityName")}]",
             s"${cont.opt(s"(${e.tpeArgs.map("e" + _.name + ",").mkString("")}container[$entityName])")}")
          case Some(STraitCall("TypeWrapper", _)) =>
            (None, s"WrapperElem${cont.opt("1")}",
             if (cont)
               s"[${cont.opt(e.typesUsePref)}To${cont.opt(s", ${e.entityNameBT}, $entityName")}]"
             else
               s"[${e.entityNameBT}${e.tpeArgUseString}, To]",
             s"${cont.opt(s"()(${e.tpeArgs.map("e" + _.name + ", ").mkString("")}container[${e.entityNameBT}], container[$entityName])")}")
          case Some(parent @ STraitCall(parentName, parentTypes)) =>
            (Some(parent), s"${parentName}Elem",
             s"[${parentTypes.map(_.toString + ", ").mkString("")}To]",
             "")
          case p => !!!(s"Unsupported parent type $p of the entity ${e.name}")
        }
        val parentElem = s"$parentElemName$parentTyArgs$parentArgs"
        val tpeSubst = e.tpeArgs.map(a => {
          val name = e.getImplicitArgName(a.name)
          (a.name, name)
        })

        s"""
        |  // familyElem
        |  ${isW.opt("abstract ")}class ${e.name}Elem[${e.typesDeclPref}To <: ${e.entityType}]${e.implicitArgs.opt(args => s"(implicit ${args.rep(a => s"val ${a.name}: ${a.tpe}")})")}
        |    extends $parentElem {
        |    ${optParent.opt(_ => "override ")}lazy val parent: Option[Elem[_]] = ${optParent.opt(p => s"Some(${tpeToElement(p, e.tpeArgs)})", "None")}
        |    ${optParent.opt(_ => "override ")}lazy val entityDef: STraitOrClassDef = {
        |      val module = getModules("${module.name}")
        |      module.entities.find(_.name == "${e.name}").get
        |    }
        |    ${optParent.opt(_ => "override ")}lazy val tyArgSubst: Map[String, TypeDesc] = {
        |      Map(${tpeSubst.rep({ case (n,v) => "\"" + n + "\" -> " + v.fold(l => s"Left($l)", r => s"Right($r.asInstanceOf[SomeCont])") })})
        |    }
        |    override def isEntityType = true
        |    override lazy val tag = {
        |${e.implicitArgs.flatMap(arg => arg.tpe match {
          case STraitCall(name, List(tpe)) if name == "Elem" || name == "Element" =>
            Some(s"      implicit val tag${typeToIdentifier(tpe)} = ${arg.name}.tag")
          case _=> None
        }).mkString("\n")}
        |      weakTypeTag[${e.entityType}].asInstanceOf[WeakTypeTag[To]]
        |    }
        |    override def convert(x: Rep[Reifiable[_]]) = {
        |      implicit val eTo: Elem[To] = this
        |      val conv = fun {x: Rep[${e.entityType}] => convert$entityName(x) }
        |      tryConvert(element[${e.entityType}], this, x, conv)
        |    }
        |
        |    def convert$entityName(x : Rep[${e.entityType}]): Rep[To] = {
        |      assert(x.selfType1${e.t.isHighKind.opt(".asInstanceOf[Element[_]]")} match { case _: $wildcardElem => true; case _ => false })
        |      x.asRep[To]
        |    }
        |    override def getDefaultRep: Rep[To] = ???
        |  }
        |${
          val elemMethod = entityElemMethodName(e.name)
          if (!module.methods.exists(_.name == elemMethod)) {
            val tyArgs = e.tpeArgDecls.opt(args => s"[${args.mkString(", ")}]")
            s"""
               |  implicit def $elemMethod$tyArgs${e.implicitArgsDecl}: Elem[${e.entityType}] =
               |${if (e.isWrapper)
            s"""    elemCache.getOrElseUpdate(
               |      (classOf[${e.name}Elem[${e.typesUsePref}${e.entityType}]], ${e.implicitArgsUse.opt(x => s"Seq$x", "Nil")}),
               |      new ${e.name}Elem[${e.typesUsePref}${e.entityType}] {
               |        lazy val eTo = element[${e.name}Impl${e.tpeArgUseString}]
               |      }).asInstanceOf[Elem[${e.entityType}]]"""
                else
              s"    cachedElem[${e.name}Elem[${e.typesUsePref}${e.entityType}]]${e.implicitArgsUse.opt(x => x, "()")}"}
               |""".stripMargin
          } else ""
        }
        |""".stripAndTrim
      }

      val companionSql = entityCompOpt.opt(comp => extractSqlQueries(comp.body))
      val companionAbs = s"""
        |  implicit case object ${companionName}Elem extends CompanionElem[${companionName}Abs] {
        |    lazy val tag = weakTypeTag[${companionName}Abs]
        |    protected def getDefaultRep = $entityName
        |  }
        |
        |  abstract class ${companionName}Abs extends CompanionBase[${companionName}Abs]${hasCompanion.opt(s" with ${companionName}")} {
        |    override def toString = "$entityName"
        |    $companionSql
        |  }
        |  def $entityName: Rep[${companionName}Abs]
        |${hasCompanion.opt
        s"""
           |  implicit def proxy$companionName(p: Rep[${companionName}]): ${companionName} =
           |    proxyOps[${companionName}](p)
           |""".stripAndTrim
      }
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

      val absTypes = templateData.tpeArgUseString
      val absElemTypes = templateData.boundedTpeArgString(false)

      val concreteClasses = for { c <- module.concreteSClasses } yield {
        val className = c.name
        val concTemplateData = ConcreteClassTemplateData(module, c)
        val typesDecl = concTemplateData.tpeArgDeclString
        val typesUse = concTemplateData.tpeArgUseString
        val typesWithElems = concTemplateData.boundedTpeArgString(false)
        val fields = c.args.argNames
        val fieldsWithType = c.args.argNamesAndTypes(config)
        val fieldTypes = c.args.argUnrepTypes(module, config)
        val implicitArgs = concTemplateData.implicitArgsDecl
        val useImplicits = concTemplateData.implicitArgsUse
        val useImplicitsOrParens = if (useImplicits.nonEmpty) useImplicits else "()"
        val implicitArgsWithVals = c.implicitArgs.args.opt(args => s"(implicit ${args.rep(a => s"val ${a.name}: ${a.tpe}")})")
        val parent     = c.ancestors.head
        val parentArgs = parent.tpeSExprs.map(_.toString)
        val parentArgsStr = parentArgs.map(_ + ", ").mkString
        val isCont = templateData.isContainer1

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
        lazy val getImplicitElem: String = {
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
          val args = fieldTypes.map(a => a.toString)//c.implicitArgs.args.map(a => a.name)
          if (args.length >= 2) {
            val impls = implElem(args)("")
            val sk = ")" * (args.length - 2)
            s"()(${impls + sk})"
          } else ""
        }
        val maybeElemHack = {
          val elemMethodName = StringUtil.lowerCaseFirst(className + "DataElem")
          if (module.methods.exists(_.name == elemMethodName))
            s"()($elemMethodName)"
          else
            getImplicitElem
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

        val parentType = parent.tpeSExprs.opt(t => s"[${t.rep()}]")
        val emptyType = ""
        val fullParentType = if (parentType == "") emptyType else parentType
        val wildcardElem = s"${className}Elem${c.tpeArgs.opt(_.map(_ => "_").mkString("[", ", ", "]"))}"
        val parentElem = tpeToElement(parent, concTemplateData.tpeArgs)
        val hasCompanion = c.companion.isDefined
        val tpeSubst = concTemplateData.tpeArgs.map(a => {
          val name = concTemplateData.getImplicitArgName(a.name)
          (a.name, name)
        })

        // note: ${className}Iso.eTo doesn't call cachedElem because
        // they are already cached via Isos + lazy val and this would lead to stack overflow
        s"""
        |$defaultImpl
        |  // elem for concrete class
        |  class ${className}Elem${typesDecl}(val iso: Iso[${className}Data${typesUse}, $className${typesUse}])$implicitArgs
        |    extends ${parent.name}Elem[${parentArgsStr}$className${typesUse}]
        |    with ConcreteElem${isCont.opt("1")}[${isCont.opt(parentArgsStr)}${className}Data${typesUse}, $className${typesUse}${isCont.opt(s", ${parent.name}")}] {
        |    override lazy val parent: Option[Elem[_]] = Some(${parentElem})
        |    override lazy val entityDef = {
        |      val module = getModules("${module.name}")
        |      module.concreteSClasses.find(_.name == "$className").get
        |    }
        |    override lazy val tyArgSubst: Map[String, TypeDesc] = {
        |      Map(${tpeSubst.rep({ case (n,v) => "\"" + n + "\" -> " + v.fold(l => s"Left($l)", r => s"Right($r.asInstanceOf[SomeCont])") })})
        |    }
        |    ${templateData.isWrapper.opt("lazy val eTo = this")}
        |    override def convert${parent.name}(x: Rep[${parent.name}${parentArgs.opt("[" + _.rep() + "]")}]) = ${converterBody(module.getEntity(parent.name), c)}
        |    override def getDefaultRep = super[ConcreteElem${isCont.opt("1")}].getDefaultRep
        |    override lazy val tag = {
        |${c.implicitArgs.args.flatMap(arg => arg.tpe match {
             case STraitCall(name, List(tpe)) if name == "Elem" || name == "Element" =>
               Some (s"      implicit val tag${typeToIdentifier(tpe)} = ${arg.name}.tag")
             case _ => None
          }).mkString("\n")}
        |      weakTypeTag[$className${typesUse}]
        |    }
        |  }
        |
        |  // state representation type
        |  type ${className}Data${typesDecl} = ${dataType(fieldTypes)}
        |
        |  // 3) Iso for concrete class
        |  class ${className}Iso${typesDecl}${implicitArgs}
        |    extends Iso[${className}Data${typesUse}, $className${typesUse}]$maybeElemHack {
        |    override def from(p: Rep[$className${typesUse}]) =
        |      ${fields.map(fields => "p." + fields).opt(s => if (s.toList.length > 1) s"(${s.rep()})" else s.rep(), "()")}
        |    override def to(p: Rep[${dataType(fieldTypes)}]) = {
        |      val ${pairify(fields)} = p
        |      $className(${fields.rep()})
        |    }
        |    lazy val defaultRepTo: Rep[$className${typesUse}] = $className(${fieldTypes.rep(zeroSExpr(_))})
        |    lazy val eTo = new ${className}Elem${typesUse}(this)
        |  }
        |  // 4) constructor and deconstructor
        |  abstract class ${className}CompanionAbs extends CompanionBase[${className}CompanionAbs]${hasCompanion.opt(s" with ${className}Companion")} {
        |    override def toString = "$className"
        |${(fields.length != 1).opt(s"""
        |    def apply${typesDecl}(p: Rep[${className}Data${typesUse}])${implicitArgs}: Rep[$className${typesUse}] =
        |      iso$className${useImplicits}.to(p)""".stripAndTrim)}
        |    def apply${typesDecl}(${fieldsWithType.rep()})${implicitArgs}: Rep[$className${typesUse}] =
        |      mk$className(${fields.rep()})
        |    ${extractSqlQueries(c.body)}
        |  }
        |  object ${className}Matcher {
        |    def unapply${typesDecl}(p: Rep[${parent.name}${fullParentType}]) = unmk$className(p)
        |  }
        |  def $className: Rep[${className}CompanionAbs]
        |  implicit def proxy${className}Companion(p: Rep[${className}CompanionAbs]): ${className}CompanionAbs = {
        |    proxyOps[${className}CompanionAbs](p)
        |  }
        |
        |  implicit case object ${className}CompanionElem extends CompanionElem[${className}CompanionAbs] {
        |    lazy val tag = weakTypeTag[${className}CompanionAbs]
        |    protected def getDefaultRep = $className
        |  }
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
        |    cachedIso[${className}Iso${typesUse}]$useImplicitsOrParens
        |
        |  // 6) smart constructor and deconstructor
        |  def mk$className${typesDecl}(${fieldsWithType.rep()})${implicitArgs}: Rep[$className${typesUse}]
        |  def unmk$className${typesDecl}(p: Rep[${parent.name}${fullParentType}]): Option[(${fieldTypes.opt(fieldTypes => fieldTypes.rep(t => s"Rep[$t]"), "Rep[Unit]")})]
        |""".stripAndTrim
      }

      s"""
       |// Abs -----------------------------------
       |trait ${module.name}Abs extends ${module.name} ${config.baseContextTrait.opt(t => "with " + t)} {
       |  ${selfTypeString("")}
       |
       |${entityProxy(templateData)}
       |
       |$proxyBT
       |
       |$baseTypeElem
       |
       |${if (templateData.isContainer1) familyContainer(templateData) else ""}
       |
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
       |
       |  registerModule(scalan.meta.ScalanCodegen.loadModule(${module.name}_Module.dump))
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
      val fieldsWithType = c.args.argNamesAndTypes(config)
      val implicitArgsDecl = templateData.implicitArgsDecl

      val externalMethods = module.entityOps.getMethodsWithAnnotation(ExternalAnnotation)
      val externalMethodsStr = filterByExplicitDeclaration(externalMethods).rep(md => externalSeqMethod(md, true), "\n    ")

      val parent     = c.ancestors.head
      val parentType = parent.tpeSExprs.opt(t => s"[${t.rep()}]")
      val emptyType = ""
      val fullParentType = if (parentType == "") emptyType else parentType

      val userTypeDefs =
        s"""
         |  case class Seq$className${typesDecl}
         |      (${fieldsWithType.rep(f => s"override val $f")})
         |      ${implicitArgsDecl}
         |    extends $className${typesUse}(${fields.rep()})${c.selfType.opt(t => s" with ${t.tpe}")}
         |       ${module.seqDslImpl.opt(_ => s"with Seq$entityName${typesUse}")} with UserTypeSeq[$className${typesUse}] {
         |    lazy val selfType = element[${className}${typesUse}]
         |    $externalMethodsStr
         |  }
         |  lazy val $className = new ${className}CompanionAbs with UserTypeSeq[${className}CompanionAbs] {
         |    lazy val selfType = element[${className}CompanionAbs]
         |  }""".stripAndTrim

      val constrDefs =
        s"""
         |  def mk$className${typesDecl}
         |      (${fieldsWithType.rep()})$implicitArgsDecl: Rep[$className${typesUse}] =
         |      new Seq$className${typesUse}(${fields.rep()})${c.selfType.opt(t => s" with ${t.tpe}")}
         |  def unmk$className${typesDecl}(p: Rep[${parent.name}${fullParentType}]) = p match {
         |    case p: $className${typesUse} @unchecked =>
         |      Some((${fields.rep(f => s"p.$f")}))
         |    case _ => None
         |  }
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
      val fieldsWithType = c.args.argNamesAndTypes(config)
      val implicitArgsDecl = d.implicitArgsDecl

      val parent     = c.ancestors.head
      val parentType = parent.tpeSExprs.opt(t => s"[${t.rep()}]")
      val emptyType = ""
      val fullParentType = if (parentType == "") emptyType else parentType

      val userTypeNodeDefs =
        s"""
         |  case class Exp$className${typesDecl}
         |      (${fieldsWithType.rep(f => s"override val $f")})
         |      ${implicitArgsDecl}
         |    extends $className${typesUse}(${fields.rep()})${c.selfType.opt(t => s" with ${t.tpe}")} with UserTypeDef[$className${typesUse}] {
         |    lazy val selfType = element[$className${typesUse}]
         |    override def mirror(t: Transformer) = Exp$className${typesUse}(${fields.rep(f => s"t($f)")})
         |  }
         |
         |  lazy val $className: Rep[${className}CompanionAbs] = new ${className}CompanionAbs with UserTypeDef[${className}CompanionAbs] {
         |    lazy val selfType = element[${className}CompanionAbs]
         |    override def mirror(t: Transformer) = this
         |  }
         |
         |${methodExtractorsString(c)}
         |""".stripAndTrim

      val constrDefs =
        s"""
         |  def mk$className${typesDecl}
         |    (${fieldsWithType.rep()})$implicitArgsDecl: Rep[$className${typesUse}] =
         |    new Exp$className${typesUse}(${fields.rep()})${c.selfType.opt(t => s" with ${t.tpe}")}
         |  def unmk$className${typesDecl}(p: Rep[${parent.name}${fullParentType}]) = p.elem.asInstanceOf[Elem[_]] match {
         |    case _: ${className}Elem${typesUse} @unchecked =>
         |      Some((${fields.rep(f => s"p.asRep[$className$typesUse].$f")}))
         |    case _ =>
         |      None
         |  }
         |""".stripAndTrim

      s"""$userTypeNodeDefs\n\n$constrDefs"""
    }

    // note: currently can't cache them properly due to cyclical dependency between
    // baseType elem and wrapper elem
    def baseTypeElem(ctx: String) = optBT.opt(bt =>
      if (tyArgsDecl.isEmpty) {
        s"""
          |  implicit lazy val ${StringUtil.lowerCaseFirst(bt.name)}Element: Elem[${bt.name}] =
          |    new ${ctx}BaseElemEx[${bt.name}, $entityName](element[$entityName])(weakTypeTag[${bt.name}], ${getDefaultOfBT(bt)})
          |""".stripAndTrim
      }
      else if (templateData.isContainer1) {
        val elems = templateData.tpeArgUses.rep(ty => s"element[$ty]")
        s"""
          |  implicit def ${StringUtil.lowerCaseFirst(bt.name)}Element${typesWithElems}: Elem[$entityNameBT${typesUse}] =
          |    new ${ctx}BaseElemEx1[${templateData.typesUsePref}$entityName${typesUse}, $entityNameBT](element[$entityName${typesUse}])(
          |      $elems, container[${bt.name}], ${getDefaultOfBT(bt)})
          |""".stripAndTrim
      }
      else {
        val weakTagsForTpeArgs = templateData.tpeArgUses.map { argName =>
          s"implicit val w$argName = element[$argName].tag;"
        }.reduce(_ + _)
        s"""
          |  implicit def ${StringUtil.lowerCaseFirst(bt.name)}Element${typesWithElems}: Elem[$entityNameBT${typesUse}] = {
          |    $weakTagsForTpeArgs
          |    new ${ctx}BaseElemEx[$entityNameBT${typesUse}, $entityName${typesUse}](element[$entityName${typesUse}])(weakTypeTag[$entityNameBT${typesUse}], ${getDefaultOfBT(bt)})
          |  }
          |""".stripAndTrim
      })

    def getTraitSeq = {
      val e = module.entityOps
      val entityName = e.name
      val classesSeq = for { c <- module.concreteSClasses } yield getSClassSeq(c)
      val proxyBTSeq = optBT.opt(bt =>
        s"""
         |  // override proxy if we deal with TypeWrapper
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
        filterByExplicitDeclaration(methods).filter(_.body.isEmpty).rep(md => externalSeqMethod(md, false), "\n    ")
      }

      s"""
       |// Seq -----------------------------------
       |trait ${module.name}Seq extends ${module.name}Dsl ${config.seqContextTrait.opt(t => "with " + t)} {
       |  ${selfTypeString("Seq")}
       |  lazy val $entityName: Rep[${entityName}CompanionAbs] = new ${entityName}CompanionAbs with UserTypeSeq[${entityName}CompanionAbs] {
       |    lazy val selfType = element[${entityName}CompanionAbs]
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
          |      case (xs: ${syn.name}[a] @unchecked, LambdaResultHasViews(f, iso: Iso[b, c])) =>
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
      if (e.isContainer1) {
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

      val companionMethods = getCompanionMethods.opt { case (constrs, methods) =>
        constrs.rep(md => externalConstructor(md), "\n    ") +
        methods.rep(md => externalMethod(md), "\n    ")
      }

      val concreteClassesString = module.concreteSClasses.map(getSClassExp)

      s"""
       |// Exp -----------------------------------
       |trait ${module.name}Exp extends ${module.name}Dsl ${config.stagedContextTrait.opt(t => "with " + t)} {
       |  ${selfTypeString("Exp")}
       |  lazy val $entityName: Rep[${entityName}CompanionAbs] = new ${entityName}CompanionAbs with UserTypeDef[${entityName}CompanionAbs] {
       |    lazy val selfType = element[${entityName}CompanionAbs]
       |    override def mirror(t: Transformer) = this
       |    $companionMethods
       |  }
       |
       |${if (td.isContainer1) familyView(td) else ""}
       |
       |${baseTypeElem("Exp")}
       |
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
          (m.explicitArgs.collect { case SMethodArg(_,_, name, STpeFunc(_, _), _, _, _) => name} match {
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
              //println(s"    WARNING: Cannot generate matcher for method `${e.name}.${m.name}`: $reason")
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
                    e.tpeArgs.length + 1
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
