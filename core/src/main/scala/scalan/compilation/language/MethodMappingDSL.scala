package scalan.compilation.language

import scala.collection.mutable

trait LanguageId

object SCALA extends LanguageId
object CPP extends LanguageId

trait Implicit[T] { this: T =>
  implicit val v: T = this
}

// E below is short for Entity, to avoid conflict with Type, Method, etc.
case class EPackage(name: String) extends Implicit[EPackage]

case class EModule(name: String)(implicit val ePackage: EPackage) extends Implicit[EModule]

case class EType(name: String)(implicit val module: EModule = null, val ePackage: EPackage = null) extends Implicit[EType] {
  override def equals(obj: Any): Boolean = obj match {
    case m: EType => m.name == name && m.module == module && m.ePackage == ePackage
    case _ => false
  }
}

case class EMethod(name: String, overloadIdOpt: Option[String])(implicit val eType: EType) {
  override def equals(obj: Any): Boolean = obj match {
    case m: EMethod => m.name == name && m.overloadIdOpt == overloadIdOpt && m.eType == eType
    case _ => false
  }
}

object EMethod {
  def apply(name: String)(implicit eType: EType): EMethod = new EMethod(name, None)
  def apply(name: String, overloadId: String)(implicit eType: EType): EMethod = new EMethod(name, Some(overloadId))
}

trait MethodMappingDSL { self =>
  // be explicit to avoid errors if scala.reflect.runtime.universe.Symbol will be imported later
  implicit def symbolToString(s: scala.Symbol): String = s.name

  val mappingDSLs: mutable.HashMap[LanguageId, mutable.ArrayBuffer[Mapping]] = new mutable.HashMap()

  abstract class Mapping(languageId: LanguageId) {
    type Func
    trait TypeT {
      def name: String
    }

    mappingDSLs.get(languageId) match {
      case Some(conf) => conf += this
      case _ => mappingDSLs += languageId -> mutable.ArrayBuffer(this)
    }

    def get(className: String, method: String): Option[Func] = {
      methodMap.get((className, method))
    }

    val libPaths: Set[String]

    val functionMap: Map[EMethod, Func]

    val classMap: Map[Class[_], TypeT] = Map.empty[Class[_], TypeT]

    // To simplify config usage, data are transformed to Backend representation. Direct link to LanguageConf is never used
    lazy val methodMap: Map[(String, String), Func] = functionMap.map { case (m, f) =>
      val prefix = m.eType.module match {
        case null =>
          m.eType.ePackage match {
            case null => ""
            case p => p.name + "."
          }
        case family => family.ePackage.name + "." + family.name + "$"
      }
      ((prefix + m.eType.name, m.name), f)
    }
  }

  trait MappingTags {
    val mapping: Mapping

    trait Lib {
      libs += this
    }

    val libs: mutable.HashSet[Lib] = new mutable.HashSet()
  }

  trait CppMappingDSL extends MappingTags {

    case class CppLib(hfile: String, libfile: String) extends Lib with Implicit[CppLib]

    case class CppType(name: String)

    case class CppArg(ty: CppType, name: String)

    case class CppFunc(name: String, args: CppArg*)(implicit val lib: CppLib)

    abstract class CppMapping extends Mapping(CPP) {
      type Func = CppFunc

      lazy val libPaths: Set[String] = Set.empty[String]
    }
  }

  trait ScalaMappingDSL extends MappingTags {

    case class ScalaLib(jar: String = "", pack: String = "") extends Lib with Implicit[ScalaLib]

    case class EmbeddedObject(name: String) extends Lib with Implicit[EmbeddedObject]

    case class ScalaType(name: String)

    case class ScalaArg(ty: ScalaType, name: String)

    case class ScalaFunc(name: String, args: ScalaArg*)(val wrapper: Boolean = false)(implicit val lib: Lib)

    case class ScalaClass(name: String)

    abstract class ScalaMapping extends Mapping(SCALA) {
      type Func = ScalaFunc
      type TypeT = ScalaClass

      lazy val libPaths: Set[String] = libs.collect {
        case l: ScalaLib => l.jar
      }.filter(_.nonEmpty).toSet
    }
  }
}

trait CoreMethodMappingDSL extends MethodMappingDSL {

  trait CoreMapping extends MappingTags

  new ScalaMappingDSL with CoreMapping {

    val mapping = new ScalaMapping {
      val functionMap = Map.empty[EMethod, Func]
    }
  }
}