/**
 * User: Alexander Slesarenko
 * Date: 11/16/13
 */
package scalan.meta

object Extensions {
  implicit class IterableExtensions[A](val it: Iterable[A]) extends AnyVal
  {
    def opt(show: Iterable[A] => String, default: String = ""): String = it match {
      case Nil => default
      case xs => show(xs)
    }

    def rep(show: A => String, sep: String = ", "): String = it.map(show).mkString(sep)

    def enumTypes(show: Int => String) = (1 to it.size).map(show)
  }

  def all[A](a: A): String = a.toString

  implicit class OptionExtensions[A](val opt: Option[A]) extends AnyVal {
    def opt(show: A => String, default: String = ""): String = opt match {
      case None => default
      case Some(a) => show(a)
    }
  }
  implicit class BooleanExtensions[A](val opt: Boolean) extends AnyVal {
    def opt(show: => String, default: => String = ""): String = if(opt) show else default
  }
}

trait ScalanCodegen extends ScalanAst with ScalanParsers { ctx: EntityManagement =>

  class EntityFileGenerator(module: SEntityModuleDef) {
    import Extensions._

    val typeSyn = module.typeSyn

    def getEntityTemplateData(e: STraitDef) = {
      val tyArgs = e.tpeArgs.map(_.name)
      (e.name,
        e.tpeArgs,
        tyArgs.rep(t => t),
        tyArgs.rep(t =>s"$t:Elem")
        )
    }

    def getSClassTemplateData(c: SClassDef) = {
      val tyArgs = c.tpeArgs.map(_.name)
      (c.name,
      tyArgs.rep(t => t),
      tyArgs.rep(t =>s"$t:Elem"),
      c.args.map(a => a.name),
      c.args.map(a => s"${a.name}: ${a.tpe}"),
      c.args.map(a => a.tpe match {
        case STraitCall("Rep", List(t)) => t
        case _ => sys.error(s"Invalid field $a. Fields of concrete classes should be of type Rep[T] for some T.")
      }),
      c.ancestors.head
      )
    }

    def dataType(ts: List[STpeExpr]): String = ts match {
      case Nil => "Unit"
      case t :: Nil => t.toString
      case t :: ts => s"($t, ${dataType(ts)})"
    }
    
    def pairify(fs: List[String]): String = fs match {
      case Nil => "()"
      case f :: Nil => f
      case f :: fs => s"Pair($f, ${pairify(fs)})"
    }
    
    def zeroSExpr(t: STpeExpr): String = t match {
      case STpeInt => 0.toString
      case STpeBoolean => false.toString
      case STpeFloat => 0f.toString
      case STpeString => "\"\""
      case STraitCall(name, args) => s"element[$t].${if (isLite) "defaultRepValue" else "zero.value"}"
      case STpeTuple(items) => pairify(items.map(zeroSExpr))
      case t => throw new IllegalArgumentException(s"Can't generate zero value for $t")
    }
    
    lazy val isLite = config.isLite
    
    lazy val (entityName, tyArgs, types, typesWithElems) = getEntityTemplateData(module.entityOps)
    
    def getTraitAbs = {
      val companionName = s"${entityName}Companion"
      val proxy =
        s"""
        |  // single proxy for each type family
        |  implicit def proxy$entityName[$typesWithElems](p: ${typeSyn.name}[$types]): $entityName[$types] = {
        |    proxyOps[$entityName[$types]](p)
        |  }
        |""".stripMargin

      val familyElem = s"""  trait ${entityName}Elem[From,To] extends ViewElem[From, To]""".stripMargin
      val defaultElem = s"""
        |  // implicit def default${entityName}Elem[$typesWithElems]: Elem[$entityName[$types]] = ???
        |""".stripMargin
      val companionElem = s"""
        |  trait ${companionName}Elem extends CompanionElem[${companionName}Abs]
        |  implicit lazy val ${companionName}Elem: ${companionName}Elem = new ${companionName}Elem {
        |    lazy val tag = typeTag[${companionName}Abs]
        |    lazy val defaultRep = defaultVal($entityName)
        |  }
        |
        |  trait ${companionName}Abs extends ${companionName}
        |  def $entityName: Rep[${companionName}Abs]
        |  implicit def defaultOf$entityName[$typesWithElems]: Default[Rep[$entityName[$types]]] = $entityName.defaultOf[$types]
        |  implicit def proxy$companionName(p: Rep[${companionName}]): ${companionName} = {
        |    proxyOps[${companionName}](p, Some(true))
        |  }
        |""".stripMargin
        
      val isoZero = if (isLite) "defaultRepTo" else "zero"
      val defaultVal = if (isLite) "defaultVal" else "Common.zero"

      val defs = for { c <- module.concreteSClasses } yield {
        val (className, types, typesWithElems, fields, fieldsWithType, fieldTypes, traitWithTypes) = getSClassTemplateData(c)
        val implicitArgs = c.implicitArgs.opt(args => s"implicit ${args.rep(a => s"${a.name}: ${a.tpe}")}")
        val useImplicits = c.implicitArgs.opt(args => args.map(_.name).rep(all))
        s"""
        |  // elem for concrete class
        |  trait ${className}Elem[$types] extends ${entityName}Elem[${className}Data[$types], $className[$types]]
        |
        |  // state representation type
        |  type ${className}Data[$types] = ${dataType(fieldTypes)}
        |
        |  // 3) Iso for concrete class
        |  abstract class ${className}Iso[$types]($implicitArgs)
        |    extends Iso[${className}Data[$types], $className[$types]] {
        |    override def from(p: Rep[$className[$types]]) =
        |      unmk${className}(p) match {
        |        case Some((${fields.rep(all)})) => ${pairify(fields)}
        |        case None => !!!
        |      }
        |    override def to(p: Rep[${dataType(fieldTypes)}]) = {
        |      val ${pairify(fields)} = p
        |      $className(${fields.rep(all)})
        |    }
        |    lazy val tag = {
        |${c.tpeArgs.rep(a => s"      implicit val tag${a.name} = element[${a.name}].tag", "\n")}
        |      typeTag[$className[$types]]
        |    }
        |    lazy val ${isoZero} = $defaultVal[Rep[$className[$types]]]($className(${fieldTypes.rep(zeroSExpr(_))}))
        |  }
        |  // 4) constructor and deconstructor
        |${if (isLite)
        s"  trait ${className}CompanionAbs extends ${className}Companion {"
        else
        s"  object ${className} extends ${className}Companion {"}
        |${(fields.length != 1).opt(s"""
        |    def apply[$types](p: Rep[${className}Data[$types]])($implicitArgs): Rep[$className[$types]] =
        |      iso$className($useImplicits).to(p)""".stripMargin)
        }${(!isLite).opt(s"""
        |    def apply[$types](p: $traitWithTypes)($implicitArgs): Rep[$className[$types]] =
        |      mk$className(${fields.rep(f => s"p.$f")})
        |""".stripMargin)}
        |    def apply[$types]
        |          (${fieldsWithType.rep(all)})($implicitArgs): Rep[$className[$types]] =
        |      mk$className(${fields.rep(all)})
        |    def unapply[$typesWithElems](p: Rep[$className[$types]]) = unmk$className(p)
        |  }
        |${isLite.opt(s"""
        |  def $className: Rep[${className}CompanionAbs]
        |  implicit def proxy${className}Companion(p: Rep[${className}CompanionAbs]): ${className}CompanionAbs = {
        |    proxyOps[${className}CompanionAbs](p, Some(true))
        |  }
        |
        |  trait ${className}CompanionElem extends CompanionElem[${className}CompanionAbs]
        |  implicit lazy val ${className}CompanionElem: ${className}CompanionElem = new ${className}CompanionElem {
        |    lazy val tag = typeTag[${className}CompanionAbs]
        |    lazy val defaultRep = defaultVal($className)
        |  }""".stripMargin)}
        |
        |  implicit def proxy$className[$typesWithElems](p: Rep[$className[$types]]): ${className}[$types] = {
        |    proxyOps[${className}[$types]](p)
        |  }
        |
        |  implicit class Extended$className[$types](p: Rep[$className[$types]])($implicitArgs) {
        |    def toData: Rep[${className}Data[$types]] = iso$className($useImplicits).from(p)
        |  }
        |
        |  // 5) implicit resolution of Iso
        |  implicit def iso$className[$types]($implicitArgs): Iso[${className}Data[$types], $className[$types]]
        |
        |  // 6) smart constructor and deconstructor
        |  def mk$className[$types](${fieldsWithType.rep(all)})($implicitArgs): Rep[$className[$types]]
        |  def unmk$className[$typesWithElems](p: Rep[$className[$types]]): Option[(${fieldTypes.rep(t => s"Rep[$t]")})]
        |""".stripMargin
      }

      s"""
       |trait ${module.name}Abs extends ${module.name}
       |{ ${module.selfType.opt(t => s"self: ${t.components.rep(all, " with ")} =>")}
       |$proxy
       |$familyElem
       |${if (config.isLite) companionElem else ""}
       |${defs.mkString("\n")}
       |}
       |""".stripMargin
    }

    def getSClassSeq(c: SClassDef) = {
      val (className, types, typesWithElems, fields, fieldsWithType, _, traitWithTypes) = getSClassTemplateData(c)
      val implicitArgs = c.implicitArgs.opt(args => s"implicit ${args.rep(a => s"${a.name}: ${a.tpe}")}")
      val userTypeDefs =
        s"""
         |  case class Seq$className[$types]
         |      (${fieldsWithType.rep(f => s"override val $f")})
         |      (implicit ${c.implicitArgs.rep(a => s"override val ${a.name}: ${a.tpe}")})
         |    extends $className[$types](${fields.rep(all)})${c.selfType.opt(t => s" with ${t.components.rep(all, " with ")}")}${isLite.opt(s" with UserTypeSeq[$traitWithTypes, $className[$types]]")} {
         |${isLite.opt(s"    lazy val selfType = element[${className}[$types]].asInstanceOf[Elem[$traitWithTypes]]")}
         |  }
         |${isLite.opt(s"""
         |  lazy val $className = new ${className}CompanionAbs with UserTypeSeq[${className}CompanionAbs, ${className}CompanionAbs] {
         |    lazy val selfType = element[${className}CompanionAbs]
         |  }
         |""".stripMargin)}
         |""".stripMargin
      val isoDefs =
        s"""
         |  implicit def iso$className[$types]($implicitArgs):Iso[${className}Data[$types], $className[$types]] =
         |    new ${className}Iso[$types] { i =>
         |      // should use i as iso reference
         |      lazy val eTo =
         |        new SeqViewElem[${className}Data[$types], $className[$types]]()(i) with ${className}Elem[$types]
         |    }
         |""".stripMargin

      val constrDefs =
        s"""
         |  def mk$className[$types]
         |      (${fieldsWithType.rep(all)})($implicitArgs) =
         |      new Seq$className[$types](${fields.rep(all)})${c.selfType.opt(t => s" with ${t.components.rep(all, " with ")}")}
         |  def unmk$className[$typesWithElems](p: Rep[$className[$types]]) =
         |    Some((${fields.rep(f => s"p.$f")}))
         |""".stripMargin

      s"""$userTypeDefs\n$isoDefs\n$constrDefs"""
    }

    def getSClassExp(c: SClassDef) = {
      val (className, types, typesWithElems, fields, fieldsWithType, fieldTypes, traitWithTypes) = getSClassTemplateData(c)
      val isLite = config.isLite
      val implicitArgs = c.implicitArgs.opt(args => s"implicit ${args.rep(a => s"${a.name}: ${a.tpe}")}")
      val userTypeNodeDefs =
        s"""
         |  case class Exp$className[$types]
         |      (${fieldsWithType.rep(f => s"override val $f")})
         |      (implicit ${c.implicitArgs.rep(a => s"override val ${a.name}: ${a.tpe}")})
         |    extends $className[$types](${fields.rep(all)})${c.selfType.opt(t => s" with ${t.components.rep(all, " with ")}")} with ${if (isLite) s"UserTypeDef[$traitWithTypes, $className[$types]]" else s"UserTypeDef[$className[$types]]"} {
         |    ${if (isLite) s"lazy val selfType = element[$className[$types]].asInstanceOf[Elem[$traitWithTypes]]" else s"lazy val elem = element[$className[$types]]"}
         |    override def mirror(t: Transformer) = Exp$className[$types](${fields.rep(f => s"t($f)")})
         |  }
         |${isLite.opt(s"""
         |  lazy val $className: Rep[${className}CompanionAbs] = new ${className}CompanionAbs with UserTypeDef[${className}CompanionAbs, ${className}CompanionAbs] {
         |    lazy val selfType = element[${className}CompanionAbs]
         |    override def mirror(t: Transformer) = this
         |  }
         |""".stripMargin)}
         |""".stripMargin

      val constrDefs =
        s"""
         |  def mk$className[$types]
         |    (${fieldsWithType.rep(all)})($implicitArgs) =
         |    new Exp$className[$types](${fields.rep(all)})${c.selfType.opt(t => s" with ${t.components.rep(all, " with ")}")}
         |  def unmk$className[$typesWithElems](p: Rep[$className[$types]]) =
         |    Some((${fields.rep(f => s"p.$f")}))
         |""".stripMargin

      val isoDefs =
        s"""
         |  implicit def iso$className[$types]($implicitArgs):Iso[${className}Data[$types], $className[$types]] =
         |    new ${className}Iso[$types] { i =>
         |      // should use i as iso reference
         |      lazy val eTo =
         |        new StagedViewElem[${className}Data[$types], $className[$types]]()(i) with ${className}Elem[$types]
         |    }
         |""".stripMargin

      s"""$userTypeNodeDefs\n$constrDefs\n$isoDefs"""
    }

    def getTraitSeq = {
      val e = module.entityOps
      val (entityName, _, _, _) = getEntityTemplateData(e)
      val defs = for { c <- module.concreteSClasses } yield getSClassSeq(c)

      s"""
       |trait ${module.name}Seq extends ${module.name}Abs { self: ScalanSeq${module.selfType.opt(t => s" with ${t.components.rep(all, " with ")}")} =>
       |${isLite.opt(s"""
       |  lazy val $entityName: Rep[${entityName}CompanionAbs] = new ${entityName}CompanionAbs with UserTypeSeq[${entityName}CompanionAbs, ${entityName}CompanionAbs] {
       |    lazy val selfType = element[${entityName}CompanionAbs]
       |  }""".stripMargin)}
       |${defs.mkString("\n")}
       |}
       |""".stripMargin
    }

    def getTraitExp = {
      val e = module.entityOps
      val (entityName, _, _, _) = getEntityTemplateData(e)
      val defs = for { c <- module.concreteSClasses } yield getSClassExp(c)

      s"""
       |trait ${module.name}Exp extends ${module.name}Abs with ${config.proxyTrait} with ${config.stagedViewsTrait} { self: ScalanStaged${module.selfType.opt(t => s" with ${t.components.rep(all, " with ")}")} =>
       |${isLite.opt(s"""
       |  lazy val $entityName: Rep[${entityName}CompanionAbs] = new ${entityName}CompanionAbs with UserTypeDef[${entityName}CompanionAbs, ${entityName}CompanionAbs] {
       |    lazy val selfType = element[${entityName}CompanionAbs]
       |    override def mirror(t: Transformer) = this
       |  }""".stripMargin)}
       |${defs.mkString("\n")}
       |}
       |""".stripMargin
    }

    def getFileHeader = {
      s"""
      |package ${module.packageName}
      |package impl
      |
      |${(module.imports ++ config.extraImports.map(SImportStat(_))).rep(i => s"import ${i.name}", "\n")}
      |""".stripMargin
    }

    def getImplFile: String = {
      val topLevel = List(
        getFileHeader,
        getTraitAbs,
        getTraitSeq,
        getTraitExp
      )
      topLevel.mkString("\n")
    }
  }
}
