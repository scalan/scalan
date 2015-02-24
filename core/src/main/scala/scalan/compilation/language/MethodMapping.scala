package scalan.compilation.language

import scala.collection.mutable
import scala.language.postfixOps

object LanguageId extends Enumeration {
  type LANGUAGE = Value
  val SCALA, CPP = Value

  implicit def defaultLanguage: LANGUAGE = SCALA
}

trait MethodMapping {

  import LanguageId._

  val backends: mutable.HashMap[LANGUAGE, LanguageConf#Backend[_]] = new mutable.HashMap()

  def methodReplaceConf(implicit language: LANGUAGE): LanguageConf#Backend[_] = backends(language)

  trait Implicit[T] {
    this: T =>
    implicit val v: T = this
  }

  trait LanguageConf extends Implicit[LanguageConf] {

    import scala.reflect.runtime.universe.typeOf
    import scala.reflect.runtime.universe.Type

    val tyInt = typeOf[Int]
    val tyString = typeOf[String]
    val tyArray = typeOf[Array[_]]

    val backend: Backend[_]

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

    case class Family(familyName: Symbol)(implicit val pack: Pack = null) extends Implicit[Family]

    trait Ty {
      def name: Symbol
    }

    case class BaseTy(name: Symbol) extends Ty

    case class TyArg(name: Symbol)

    trait DomainType extends Ty

    case class ClassType(name: Symbol, synonym: Symbol, tyArgs: TyArg*)(implicit val family: Family = null, val pack: Pack = null) extends DomainType with Implicit[ClassType]

    case class MethodArg(name: Type)

    case class Method(name: Symbol, tyRes: Type, args: MethodArg*)(implicit val theType: ClassType)

    trait Fun {
      val lib: Fn = null
    }

    abstract class Backend[TC <: LanguageConf](language: LANGUAGE)(implicit val l: TC) {
      backends += language -> this
      type Func <: Fun

      def get(classPath: String, method: String): Option[Func] = {
        methodMap.getOrElse((classPath, method), None)
      }

      lazy val fns = l.fns
      lazy val dependencies = l.libs.flatMap(_.dependencies)
      
      val libPaths: Set[String]

      val functionMap: Map[Method, Func]

      // To simplify config usage, data are transformed to Backend representation. Direct link to LanguageConf is never used
      lazy val methodMap: Map[(String, String), Option[Func]] = functionMap.map { case (m, f) =>
        (((m.theType.family match {
          case f: Family => f.pack.pack + "." + f.familyName.name + "$"
          case _ =>
            m.theType.pack match {
              case p: Pack => p.pack + "."
              case _ => ""
            }
        }) + m.theType.name.name, m.name.name), Some(f))
      } toMap
    }

  }

  trait CppLanguage extends LanguageConf {

    case class CppLib(hfile: String, libfile: String) extends Fn with Implicit[CppLib]

    case class CppType(name: String)

    case class CppArg(ty: CppType, name: String)

    case class CppFunc(funcName: String, args: CppArg*)(implicit override val lib: CppLib) extends Fun

    abstract class CppBackend extends Backend(CPP) {
      type Func = CppFunc

      lazy val libPaths: Set[String] = Set.empty[String]
    }

  }

  trait ScalaLanguage extends LanguageConf {

    case class ScalaLib(jar: String, pack: String) extends Fn with Implicit[ScalaLib]

    case class EmbeddedObject(name: String) extends Fn with Implicit[EmbeddedObject]

    case class ScalaType(name: Symbol)

    case class ScalaArg(ty: ScalaType, name: Symbol)

    case class ScalaFunc(funcName: Symbol, args: ScalaArg*)(implicit override val lib: Fn) extends Fun

    abstract class ScalaBackend extends Backend(SCALA) {
      type Func = ScalaFunc

      lazy val libPaths: Set[String] = fns filter(_.isInstanceOf[ScalaLib]) map (_.asInstanceOf[ScalaLib].jar) filter (!_.isEmpty) to
    }
  }
}

trait CoreMethodMapping extends MethodMapping {

  trait CoreConf extends LanguageConf

  new ScalaLanguage with CoreConf {

    val backend = new ScalaBackend {
      val functionMap = Map.empty[Method, Func]
    }
  }
}


