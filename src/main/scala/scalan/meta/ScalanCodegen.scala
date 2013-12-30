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

  class EntityFileGenerator(module: EntityModuleDef) {
    import Extensions._

    val typeSyn = module.typeSyn

    def getEntityTemplateData(e: TraitDef) = {
      val tyArgs = e.tpeArgs.map(_.name)
      (e.name,
        e.tpeArgs,
        tyArgs.rep(t => t),
        tyArgs.rep(t =>s"$t:Elem")
        )
    }

    def getClassTemplateData(c: ClassDef) = {
      val tyArgs = c.tpeArgs.map(_.name)
      (c.name,
      tyArgs.rep(t => t),
      tyArgs.rep(t =>s"$t:Elem"),
      c.args.map(a => a.name),
      c.args.map(a => s"${a.name}: ${a.tpe}"),
      c.args.map(a => a.tpe match {
        case TraitCall("Rep", List(t)) => t
        case t => t
        //case _ => sys.error(s"Invalid field $a. Fields of concrete classes should be of type Rep[T] for some T.")
      })
      )
    }

    def dataType(t: TpeExpr): String = t match {
      case module.EntityRepType(t) => dataType(t)
      case _ => t.toString
    }
    def dataTypes(ts: List[TpeExpr]): List[String] = ts match {
      case Nil => Nil
      case t :: Nil => List(dataType(t))
      case t :: ts => dataType(t) :: dataTypes(ts)
    }
    
    def pairify(fs: List[String]): String = fs match {
      case Nil => "()"
      case f :: Nil => f
      case f :: fs => s"Pair($f, ${pairify(fs)})"
    }

    def tuplify(fs: List[String]): String = fs match {
      case Nil => "()"
      case f :: Nil => f
      case f :: fs => s"($f, ${tuplify(fs)})"
    }

    def zeroExpr(t: TpeExpr): String = t match {
      case TpeInt => 0.toString
      case TpeBoolean => false.toString
      case TpeFloat => 0f.toString
      case TpeString => "\"\""
      case TraitCall(name, args) => s"element[${dataType(t)}].defaultOf.value"
      case TpeTuple(items) => pairify(items.map(zeroExpr))
      case _ => ???
    }
    
    def getTraitAbs = {
      val e = module.entityOps
      val (entityName, tyArgs, entityTyArgs, typesWithElems) = getEntityTemplateData(e)
      val proxy =
        s"""
        |  // single proxy for each type family
        |  implicit def proxy$entityName[$typesWithElems](p: ${typeSyn.name}[$entityTyArgs]): $entityName[$entityTyArgs] = {
        |${tyArgs.rep(a => s"    implicit val m${a.name} = element[${a.name}].manifest;", "\n")}
        |    proxyOps[$entityName[$entityTyArgs], $entityName[$entityTyArgs]](p)
        |  }
        |""".stripMargin

      val familyElem =
        s"""
        |  trait ${entityName}Elem[From,To] extends ViewElem[From, To]
        |  implicit def default${entityName}Element[$typesWithElems]: Elem[$entityName[$entityTyArgs]] = ???
        |""".stripMargin

      val defs = for { c <- module.concreteClasses } yield {
        val (className, types, typesWithElems, fields, fieldsWithType, fieldTypes) = getClassTemplateData(c)
        def implicitArgs(prefix: String = "") = c.implicitArgs.opt(args => s"(implicit ${args.rep(a => s"$prefix ${a.name}: ${a.tpe}")})")
        val useImplicits = c.implicitArgs.opt(args => s"(${args.map(_.name).rep(all)})")
        s"""
        |  //------------------------------- $className ------------------------------------
        |  // elem for concrete class
        |  trait ${className}Elem[$types] extends ${entityName}Elem[${className}Data[$types], $className[$types]]
        |
        |  // state representation type
        |  type ${className}Data[$types] = ${tuplify(dataTypes(fieldTypes))}
        |
        |  // 3) companion object with Iso, constructor and deconstructor
        |  object $className extends ${className}Companion {
        |    abstract class Iso[$types]${implicitArgs()}
        |           extends IsoBase[${className}Data[$types], $className[$types]] {
        |      override def from = { case $className(${fields.rep(all)}) => ${pairify(fields)} }
        |      override def to = (p: Rep[${tuplify(dataTypes(fieldTypes))}]) => {
        |        val ${pairify(fields)} = p
        |        $className(${fields.rep(all)})
        |      }
        |      def manifest = { 
        |${c.tpeArgs.rep(a => s"        implicit val m${a.name} = element[${a.name}].manifest", "\n")}
        |        Predef.manifest[$className[$types]] 
        |      }
        |      def defaultOf = Common.defaultVal[Rep[$className[$types]]]($className(${fieldTypes.rep(t => zeroExpr(t))}))
        |    }
        |
        |    def apply[$types](p: Rep[${className}Data[$types]])${implicitArgs()}: Rep[$className[$types]]
        |        = iso$className$useImplicits.to(p)
        |    //def apply[$types](p: $entityName[$entityTyArgs])${implicitArgs()}: Rep[$className[$types]]
        |    //    = mk$className(${fields.rep(f => s"p.$f")})
        |    def apply[$types]
        |          (${fieldsWithType.rep(all)})\n          ${implicitArgs()}: Rep[$className[$types]]
        |        = mk$className(${fields.rep(all)})
        |    def unapply[$typesWithElems](p: Rep[$className[$types]]) = unmk$className(p)
        |  }
        |
        |  implicit def proxy$className[$types](p: Rep[$className[$types]])${implicitArgs()}: ${className}Ops[$types] = {
        |${c.tpeArgs.rep(a => s"    implicit val m${a.name} = element[${a.name}].manifest;", "\n")}
        |    proxyOps[${className}Ops[$types], ${className}Ops[$types]](p)
        |  }
        |
        |  implicit def extend$className[$types](p: Rep[$className[$types]])${implicitArgs()} = new {
        |    def toData: Rep[${className}Data[$types]] = iso$className$useImplicits.from(p)
        |  }
        |
        |  // 4) implicit resolution of Iso
        |  implicit def iso$className[$types]${implicitArgs()}: Iso[${className}Data[$types], $className[$types]]
        |
        |  // 5) smart constructor and deconstructor
        |  def mk$className[$types](${fieldsWithType.rep(all)})${implicitArgs()}: Rep[$className[$types]]
        |  def unmk$className[$types](p: Rep[$className[$types]])${implicitArgs()}: Option[(${dataTypes(fieldTypes).rep(t => s"Rep[$t]")})]
        |""".stripMargin
      }

      s"""
       |trait ${module.name}Abs extends ${module.name}
       |{ ${module.selfType.opt(t => s"self: ${t.components.rep(all, " with ")} =>")}
       |$proxy
       |$familyElem
       |${defs.mkString("\n")}
       |}
       |""".stripMargin
    }

    def getClassSeq(c: ClassDef) = {
      val e = module.entityOps
      val entityTypes = e.tpeArgs.map(_.name).rep(t => t)
      val (className, types, typesWithElems, fields, fieldsWithType, _) = getClassTemplateData(c)
      val isLite = !config.emitSourceContext
      def implicitArgs(prefix: String = "") = c.implicitArgs.opt(args => s"(implicit ${args.rep(a => s"$prefix ${a.name}: ${a.tpe}")})")
      val userTypeDefs =
        s"""
         |  case class Seq$className[$types]
         |      (${fieldsWithType.rep(f => s"override val $f")})\n      ${implicitArgs("override val")}
         |      extends $className[$types](${fields.rep(all)}) ${c.selfType.opt(t => s"with ${t.components.rep(all, " with ")}")} {
         |    def elem = element[$className[$types]].asInstanceOf[Elem[${e.name}[$entityTypes]]]
         |  }
         |""".stripMargin
      val isoDefs =
        s"""
         |  implicit def iso$className[$types]${implicitArgs()}: Iso[${className}Data[$types], $className[$types]]
         |    = new $className.Iso[$types] with SeqIso[${className}Data[$types], $className[$types]] { i =>
         |        // should use i as iso reference
         |        override lazy val e${config.isoNames._2} = new SeqViewElem[${className}Data[$types], $className[$types]]${(!isLite).opt("()(i)")}
         |                                    with ${className}Elem[$types] ${isLite.opt("{ val iso = i }")}
         |      }
         |""".stripMargin

      val constrDefs =
        s"""
         |  def mk$className[$types]
         |      (${fieldsWithType.rep(all)})\n      ${implicitArgs()}
         |      = new Seq$className[$types](${fields.rep(all)})
         |  def unmk$className[$types](p: Rep[$className[$types]])\n      ${implicitArgs()}
         |    = Some((${fields.rep(f => s"p.$f")}))
         |""".stripMargin

      s"""$userTypeDefs\n$isoDefs\n$constrDefs"""
    }

    def getClassExp(c: ClassDef) = {
      val e = module.entityOps
      val entityTypes = e.tpeArgs.map(_.name).rep(t => t)
      val (className, types, typesWithElems, fields, fieldsWithType, _) = getClassTemplateData(c)
      val isLite = !config.emitSourceContext
      def implicitArgs(prefix: String = "") = c.implicitArgs.opt(args => s"(implicit ${args.rep(a => s"$prefix ${a.name}: ${a.tpe}")})")
      val userTypeNodeDefs =
        s"""
         |  case class Exp$className[$types]
         |      (${fieldsWithType.rep(f => s"override val $f")})
         |      ${implicitArgs("override val")}
         |    extends $className[$types](${fields.rep(all)}) ${c.selfType.opt(t => s"with ${t.components.rep(all, " with ")}")}
         |       with UserTypeDef[${e.name}[$entityTypes],$className[$types]] {
         |    lazy val objType = element[$className[$types]]
         |    def elem = objType.asInstanceOf[Elem[${e.name}[$entityTypes]]]
         |    override def mirror(t: Transformer)${config.emitSourceContext.opt("(implicit ctx: SourceContext)")}: Rep[_] = Exp$className[$types](${fields.rep(f => s"t($f)")})
         |  }
         |  addUserType(manifest[Exp$className[Any]])
         |""".stripMargin

      val constrDefs =
        s"""
         |  def mk$className[$types]
         |      (${fieldsWithType.rep(all)})\n      ${implicitArgs()}
         |      = new Exp$className[$types](${fields.rep(all)})
         |  def unmk$className[$types]
         |      (p: Rep[$className[$types]])\n      ${implicitArgs()}
         |    = Some((${fields.rep(f => s"p.$f")}))
         |""".stripMargin

      val isoDefs =
        s"""
         |  implicit def iso$className[$types]${implicitArgs()}: Iso[${className}Data[$types], $className[$types]]
         |    = new $className.Iso[$types] with StagedIso[${className}Data[$types], $className[$types]] { i =>
         |        // should use i as iso reference
         |        override lazy val e${config.isoNames._2} = new StagedViewElem[${className}Data[$types], $className[$types]]${(!isLite).opt("()(i)")}
         |                                    with ${className}Elem[$types] ${isLite.opt("{ val iso = i }")}
         |      }
         |""".stripMargin

      s"""$userTypeNodeDefs\n$constrDefs\n$isoDefs"""
    }

    def getTraitSeq = {
      val e = module.entityOps
      val defs = for { c <- module.concreteClasses } yield getClassSeq(c)

      s"""
       |trait ${module.name}Seq extends ${module.name}Abs
       |{ self: ScalanSeq ${module.selfType.opt(t => s"with ${t.components.rep(all, " with ")}")} =>
       |${defs.mkString("\n")}
       |}
       |""".stripMargin
    }

    def getTraitExp = {
      val e = module.entityOps
      val defs = for { c <- module.concreteClasses } yield getClassExp(c)

      s"""
       |trait ${module.name}Exp extends ${module.name}Abs with ProxyExp with ${config.stagedViewsTrait}
       |{ self: ScalanStaged ${module.selfType.opt(t => s"with ${t.components.rep(all, " with ")}")} =>
       |${defs.mkString("\n")}
       |}
       |""".stripMargin
    }

    def getFileHeader = {
      s"""
      |package ${module.packageName}
      |
      |${module.imports.rep(i => s"import ${i.names.rep(all, ".")}", "\n")}
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



