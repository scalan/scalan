/**
 * User: Alexander Slesarenko
 * Date: 11/16/13
 */
package makro

import scalan.codegen.emit.Formatter
import scalan.codegen.emit.ScalaCodeEmitter

object Extensions {
  implicit class IterableExtensions[A](val it: Iterable[A]) extends AnyVal
  {
    def rep(show: A => String, sep: String = ", "): String = it.map(show).mkString(sep)

    def enumTypes(show: Int => String) = (1 to it.size).map(show)
  }

  def all[A](a: A): String = a.toString
}

trait ScalanCodegen extends ScalanAst with ScalanParsers {


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
        case _ => sys.error(s"Invalid field $a. Fields of concrete classes should be of type Rep[T] for some T.")
      })
      )
    }

    def dataType(ts: List[TpeExpr]): String = ts match {
      case Nil => "Unit"
      case t :: Nil => t.toString
      case t :: ts => s"($t, ${dataType(ts)})"
    }
    
    def pairify(fs: List[String]): String = fs match {
      case Nil => "()"
      case f :: Nil => f
      case f :: fs => s"Pair($f, ${pairify(fs)})"
    }
    
    def zeroExpr(t: TpeExpr): String = t match {
      case TpeInt => 0.toString
      case TpeBoolean => false.toString
      case TpeFloat => 0f.toString
      case TpeString => ""
      case TraitCall(name, args) => s"element[$t].zero.zero"
      case TpeTuple(items) => pairify(items.map(zeroExpr))
      case _ => ???
    }
    
    def getTraitAbs = {
      val (entityName, tyArgs, types, typesWithElems) = getEntityTemplateData(module.entityOps)
      val proxy =
        s"""
        |  // single proxy for each type family
        |  implicit def proxy$entityName[$typesWithElems](p: ${typeSyn.name}[$types]): $entityName[$types] = {
        |${tyArgs.rep(a => s"    implicit val m${a.name} = element[${a.name}].manifest;", "\n")}
        |    proxyOps[$entityName[$types], $entityName[$types]](p)
        |  }
        |""".stripMargin

      val familyElem = s"""  trait ${entityName}Elem[From,To] extends ViewElem[From, To]""".stripMargin

      val defs = for { c <- module.concreteClasses } yield {
        val (className, types, typesWithElems, fields, fieldsWithType, fieldTypes) = getClassTemplateData(c)
        s"""
        |  // elem for concrete class
        |  trait ${className}Elem[$types] extends ${entityName}Elem[${className}Data[$types], $className[$types]]
        |
        |  // state representation type
        |  type ${className}Data[$types] = ${dataType(fieldTypes)}
        |
        |  // 3) companion object with Iso, constructor and deconstructor
        |  object $className {
        |    abstract class Iso[$typesWithElems] extends IsoBase[${className}Data[$types], $className[$types]] {
        |      override def fromStaged = { case $className(${fields.rep(all)}) => ${pairify(fields)} }
        |      override def toStaged = (p: Rep[${dataType(fieldTypes)}]) => {
        |        val ${pairify(fields)} = p
        |        $className(${fields.rep(all)})
        |      }
        |      def manifest = { 
        |${c.tpeArgs.rep(a => s"        implicit val m${a.name} = element[${a.name}].manifest", "\n")}
        |        Predef.manifest[$className[$types]] 
        |      }
        |      def zero = Common.zero[Rep[$className[$types]]]($className(${fieldTypes.rep(t => zeroExpr(t))}))
        |    }
        |
        |    def apply[$typesWithElems](${fieldsWithType.rep(all)}): Rep[$className[$types]]  = mk$className(${fields.rep(all)})
        |    def unapply[$typesWithElems](p: Rep[$className[$types]]) = unmk$className(p)
        |  }
        |
        |  // 4) implicit resolution of Iso
        |  implicit def iso$className[$typesWithElems]: Iso[${className}Data[$types], $className[$types]]
        |
        |  // 5) smart constructor and deconstructor
        |  def mk$className[$typesWithElems](${fieldsWithType.rep(all)}): Rep[$className[$types]]
        |  def unmk$className[$typesWithElems](p: Rep[$className[$types]]): Option[(${fieldTypes.rep(t => s"Rep[$t]")})]
        |""".stripMargin
      }

      s"""
       |trait ${module.name}Dsl extends ${module.name} {
       |$proxy
       |$familyElem
       |${defs.mkString("\n")}
       |}
       |""".stripMargin
    }

    def getClassSeq(c: ClassDef) = {
      val (className, types, typesWithElems, fields, fieldsWithType, _) = getClassTemplateData(c)
      val isoDefs =
        s"""
         |  implicit def iso$className[$typesWithElems]:Iso[${className}Data[$types], $className[$types]]
         |    = new $className.Iso[$types] with SeqIso[${className}Data[$types], $className[$types]] { i =>
         |        // should use i as iso reference
         |        override lazy val eTo = new SeqViewElem[${className}Data[$types], $className[$types]]
         |                                    with ${className}Elem[$types] { val iso = i }
         |      }
         |""".stripMargin

      val constrDefs =
        s"""
         |  def mk$className[$typesWithElems](${fieldsWithType.rep(all)})
         |    = new $className[$types](${fields.rep(all)})
         |  def unmk$className[$typesWithElems](p: Rep[$className[$types]])
         |    = Some((${fields.rep(f => s"p.$f")}))
         |""".stripMargin

      s"""$isoDefs\n$constrDefs"""
    }

    def getClassExp(c: ClassDef) = {
      val (className, types, typesWithElems, fields, fieldsWithType, _) = getClassTemplateData(c)
      val isoDefs =
        s"""
         |  implicit def iso$className[$typesWithElems]:Iso[${className}Data[$types], $className[$types]]
         |    = new $className.Iso[$types] with StagedIso[${className}Data[$types], $className[$types]] { i =>
         |        // should use i as iso reference
         |        override lazy val eTo = new StagedViewElem[${className}Data[$types], $className[$types]]
         |                                    with ${className}Elem[$types] { val iso = i }
         |      }
         |""".stripMargin

      val constrDefs =
        s"""
         |  def mk$className[$typesWithElems](${fieldsWithType.rep(all)})
         |    = new Exp$className[$types](${fields.rep(all)})
         |  def unmk$className[$typesWithElems](p: Rep[$className[$types]])
         |    = Some((${fields.rep(f => s"p.$f")}))
         |""".stripMargin

      val userTypeNodeDefs =
        s"""
         |  case class Exp$className[$types]
         |      (${fieldsWithType.rep(f => s"override val $f")})
         |      (implicit ${c.implicitArgs.rep(a => s"override val ${a.name}: ${a.tpe}")})
         |    extends $className[$types](${fields.rep(all)}) with UserType[$className[$types]] {
         |    def elem = element[$className[$types]]
         |    override def mirror(t: Transformer): Rep[_] = Exp$className[$types](${fields.rep(f => s"t($f)")})
         |  }
         |  addUserType(manifest[Exp$className[Any]])
         |""".stripMargin

      s"""$userTypeNodeDefs\n$constrDefs\n$isoDefs"""
    }

    def getTraitSeq = {
      val defs = for { c <- module.concreteClasses } yield getClassSeq(c)

      s"""
       |trait ${module.name}Seq extends ${module.name}Dsl { self: ScalanSeq =>
       |${defs.mkString("\n")}
       |}
       |""".stripMargin
    }

    def getTraitExp = {
      val defs = for { c <- module.concreteClasses } yield getClassExp(c)

      s"""
       |trait ${module.name}Exp extends ${module.name}Dsl with ProxyExp with ViewsExp { self: ScalanStaged =>
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

    def getDslFile: String = {
      val topLevel = List(
        getFileHeader,
        getTraitAbs
      )
      topLevel.mkString("\n")
    }

    def getImplFile: String = {
      val topLevel = List(
        getFileHeader,
        getTraitSeq,
        getTraitExp
      )
      topLevel.mkString("\n")
    }
  }
}



