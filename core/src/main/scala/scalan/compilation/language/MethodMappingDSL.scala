package scalan.compilation.language

import scala.collection.mutable
import scala.language.postfixOps
import scala.reflect.runtime.universe.typeOf

object LanguageId extends Enumeration {
  type LANGUAGE_ID = Value
  val SCALA, CPP = Value

  implicit def defaultLanguage: LANGUAGE_ID = SCALA
}

trait MethodMappingDSL {
  // be explicit to avoid errors if scala.reflect.runtime.universe.Symbol will be imported later
  implicit def symbolToString(s: scala.Symbol): String = s.name

  import LanguageId._

  val mappingDSLs: mutable.HashMap[LANGUAGE_ID, mutable.ArrayBuffer[MappingTags#Mapping[_]]] = new mutable.HashMap()

  def methodReplaceConf(implicit languageId: LANGUAGE_ID): mutable.ArrayBuffer[MappingTags#Mapping[_]] = mappingDSLs(languageId)

  trait Implicit[T] {
    this: T =>
    implicit val v: T = this
  }

  trait MappingTags extends Implicit[MappingTags] {
    import scala.reflect.runtime.universe.Type

    val tyInt = typeOf[Int]
    val tyString = typeOf[String]
    val tyArray = typeOf[Array[_]]

    case class CaseClassObject(aType: Type) extends Fn with Implicit[CaseClassObject]

    val mapping: Mapping[_]

    trait Fn {
      fns += this
    }

    case class Library(packageName: String = null, dependencies: Array[String] = Array.empty[String]) {
      implicit val v = this
      libs += this
    }

    val fns: mutable.HashSet[Fn] = new mutable.HashSet()
    
    val libs: mutable.HashSet[Library] = new mutable.HashSet()

    case class Pack(pack: String)(implicit val lib: Library) extends Implicit[Pack]

    case class Family(familyName: String)(implicit val pack: Pack = null) extends Implicit[Family]

    trait Ty {
      def name: String
    }

    case class BaseTy(name: String) extends Ty

    case class TyArg(name: String)

    trait DomainType extends Ty

    case class ClassType(name: String, tyArgs: TyArg*)(implicit val family: Family = null, val pack: Pack = null) extends DomainType with Implicit[ClassType]{
      override def equals(obj: scala.Any): Boolean = obj.isInstanceOf[ClassType] && {
        val m = obj.asInstanceOf[ClassType]
        m.name == name && m.tyArgs == tyArgs && m.family == family && m.pack == pack
      }
    }

    case class MethodArg(name: Type)

    case class Method(name: String, tyRes: Type, args: MethodArg*)(implicit val theType: ClassType){
      override def equals(obj: scala.Any): Boolean = obj.isInstanceOf[Method] && {
       val m = obj.asInstanceOf[Method]
        m.name == name && m.tyRes == tyRes && m.args == args && m.theType == theType
      }
    }

    trait Fun {
      def name: String
      def lib: Fn
    }

    abstract class Mapping[TC <: MappingTags](languageId: LANGUAGE_ID)(implicit val l: TC) {
      type Func <: Fun

      (mappingDSLs.get(languageId), this) match {
        case (Some(conf: mutable.ArrayBuffer[_]), current) => conf += current
        case _ => mappingDSLs +=  languageId -> mutable.ArrayBuffer(this)
      }

      def get(classPath: String, method: String): Option[Func] = {
        methodMap.getOrElse((classPath, method), None)
      }

      lazy val fns = l.fns
      lazy val dependencies = l.libs.flatMap(_.dependencies)
      
      val libPaths: Set[String]

      val functionMap: Map[Method, Func]

      val classMap: Map[Class[_], Func] = Map.empty[Class[_], Func]

      // To simplify config usage, data are transformed to Backend representation. Direct link to LanguageConf is never used
      lazy val methodMap: Map[(String, String), Option[Func]] = functionMap.map { case (m, f) =>
        (((m.theType.family match {
          case f: Family => f.pack.pack + "." + f.familyName + "$"
          case _ =>
            m.theType.pack match {
              case p: Pack => p.pack + "."
              case _ => ""
            }
        }) + m.theType.name, m.name), Some(f))
      }
    }
  }

  trait CppMappingDSL extends MappingTags {

    case class CppLib(hfile: String, libfile: String) extends Fn with Implicit[CppLib]

    case class CppType(name: String)

    case class CppArg(ty: CppType, name: String)

    case class CppFunc(name: String, args: CppArg*)(implicit val lib: CppLib) extends Fun

    abstract class CppMapping extends Mapping(CPP) {
      type Func = CppFunc

      lazy val libPaths: Set[String] = Set.empty[String]
    }
  }

  trait ScalaMappingDSL extends MappingTags {

    case class ScalaLib(jar: String = "", pack: String = "") extends Fn with Implicit[ScalaLib]

    case class EmbeddedObject(name: String) extends Fn with Implicit[EmbeddedObject]

    case class ScalaType(name: String)

    case class ScalaArg(ty: ScalaType, name: String)

    case class ScalaFunc(name: String, args: ScalaArg*)(val wrapper: Boolean = false)(implicit val lib: Fn) extends Fun

    abstract class ScalaMapping extends Mapping(SCALA) {
      type Func = ScalaFunc

      lazy val libPaths: Set[String] = fns filter(_.isInstanceOf[ScalaLib]) map (_.asInstanceOf[ScalaLib].jar) filter (!_.isEmpty) to
    }
  }
}

trait CoreMethodMappingDSL extends MethodMappingDSL {

  trait CoreMapping extends MappingTags

  new ScalaMappingDSL with CoreMapping {

    val mapping = new ScalaMapping {
      val functionMap = Map.empty[Method, Func]
    }
  }
}