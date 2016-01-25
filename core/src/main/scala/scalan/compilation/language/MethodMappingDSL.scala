package scalan.compilation.language

import java.io.File
import java.lang.reflect.Method
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scalan.util.ReflectionUtil

trait LanguageId

// TODO support "static" methods in Scala/non-member functions in C++, LMS methods

// TODO Should this be unified with ScalaMapping/CxxMapping?
object SCALA extends LanguageId
object CXX extends LanguageId

class Mapping[LibraryT, TypeT <: TypeRep[MethodT], MethodT <: MethodRep](library: LibraryT, types: Map[String, (TypeT, Map[(String, Option[String]), MethodT])]) {
  def getMethod(method: Method): Option[(LibraryT, TypeT, MethodT)] =
    getMethod(method.getDeclaringClass.getName, method.getName, ReflectionUtil.overloadId(method))
  def getMethod(className: String, methodName: String, overloadId: Option[String]): Option[(LibraryT, TypeT, MethodT)] = {
    val typeMapping = types.get(className)
    typeMapping.flatMap(pair => pair._2.get((methodName, overloadId)).map(methodT => (library, pair._1, methodT)))
  }
  // TODO handle type arguments
  def getType(m: Manifest[_]): Option[(LibraryT, TypeT)] = types.get(m.runtimeClass.getName).map(pair => (library, pair._1))
}

trait MappingBuilder[LibraryT, TypeT <: TypeRep[MethodT], MethodT <: MethodRep] {
  def language: LanguageId
  type TypeBuilderT <: TypeBuilder[LibraryT, TypeT, MethodT, TypeBuilderT]

  def moduleName: String
  def library: LibraryT
  // If we make this a nested class along with everything else, TypeRep and MethodRep can't be params of MethodCallBridge
  def types(types: TypeT*)(implicit outer: MethodMappingDSL) = {
    val typeMap = types.map { sType =>
      val fullClassName = moduleName + "$" + sType.scalanName
      val methodMap = sType.methods.map { sMethod =>
        ((sMethod.scalanName, sMethod.overloadId), sMethod)
      }.toMap
      (fullClassName, (sType, methodMap))
    }.toMap
    val mapping = new Mapping[LibraryT, TypeT, MethodT](library, typeMap)
    outer.addMapping(language, mapping)
  }
}

trait TypeRep[MethodT <: MethodRep] {
  def scalanName: String
  def mappedName: String
  def methods: Seq[MethodT]
}

trait MethodRep {
  def scalanName: String
  def mappedName: String
  def overloadId: Option[String]
}

object MappingBuilder {
  // should have some other way to give default?
  def symbolOrder(originalArgs: Seq[Symbol], reorderedArgsOpt: Option[Seq[Adjusted[Symbol]]], name: String, useOriginalAsDefault: Boolean) = reorderedArgsOpt match {
    case Some(reorderedArgs) =>
      reorderedArgs.map(_.map { a =>
        val index = originalArgs.indexOf(a)
        if (index != -1)
          index
        else
          throw new NoSuchElementException(s"$a not found in argument list $name(${originalArgs.mkString(", ")})")
      })
    case None => if (useOriginalAsDefault) originalArgs.indices.map(Adjusted.apply(_)) else Nil
  }
}

import MappingBuilder.symbolOrder

trait TypeBuilder[LibraryT, TypeT, MethodT, TypeBuilderT] {
  type MethodBuilderT <: MethodBuilder[LibraryT, TypeT, MethodT, MethodBuilderT]

  def to(mappedName: String): TypeBuilderT
  def methods(methodBuilders: MethodBuilderT*): TypeT
}

trait MethodBuilder[LibraryT, TypeT, MethodT, MethodBuilderT] {
  def to(mappedName: String): MethodBuilderT
  def apply(): MethodT
}

trait Adjustment {
  def apply[A](x: A) = Adjusted(x, Some(this))
}

/** Converts a function to an Ordering object */
object MkOrdering extends Adjustment

/** Converts a function A => B to (&A, &B) => void (for C++) */
object VoidInOut extends Adjustment

/** Converts a enum to its index */
object EnumIndex extends Adjustment

// maybe adjustments: Seq[Adjustment] instead?
case class Adjusted[+A](value: A, adjustment: Option[Adjustment]) {
  def adjust(adjustment: Adjustment) = copy(adjustment = Some(adjustment))
  def map[B](f: A => B) = copy(value = f(value))
}

object Adjusted {
  implicit def apply[A](x: A): Adjusted[A] = Adjusted(x, None)
}

object ScalaMapping {
  def MapModuleScala(moduleName: String): ScalaMappingBuilder = new ScalaMappingBuilder(moduleName, None, Nil)
  def MapModuleScala[A](implicit tag: ClassTag[A]): ScalaMappingBuilder = MapModuleScala(tag.runtimeClass.getName)

  case class ScalaLibrary(packageName: Option[String], jars: Seq[File]) // Add SBT dependency support?

  case class ScalaType(scalanName: String, mappedName: String, methods: Seq[ScalaMethod]) extends TypeRep[ScalaMethod] // TODO constructor arguments

  case class ScalaMethod(scalanName: String, overloadId: Option[String], mappedName: String, isStatic: Boolean, typeArgOrder: Seq[Adjusted[Int]], argOrder: Seq[Adjusted[Int]], implicitArgOrder: Seq[Adjusted[Int]]) extends MethodRep

  case class ScalaMappingBuilder(moduleName: String, packageName: Option[String], jars: Seq[File]) extends MappingBuilder[ScalaLibrary, ScalaType, ScalaMethod] {
    def language = SCALA
    def packageName(name: String): ScalaMappingBuilder = copy(packageName = Some(name))
    def jars(files: File*) = copy(jars = this.jars ++ files)
    def library = ScalaLibrary(packageName, jars)

    type TypeBuilderT = MapTypeScala
  }

  case class MapTypeScala(scalanName: String, fieldSyms: Seq[Symbol], mappedName: String) extends TypeBuilder[ScalaLibrary, ScalaType, ScalaMethod, MapTypeScala] {
    def to(mappedName: String) = copy(mappedName = mappedName)
    def methods(methodBuilders: MethodBuilderT*) =
      ScalaType(scalanName, mappedName, methodBuilders.map(_.apply()))

    type MethodBuilderT = MapMethodScala
  }

  object MapTypeScala {
    def apply(scalanName: String, fieldSyms: Symbol*): MapTypeScala = new MapTypeScala(scalanName, fieldSyms, scalanName)
  }

  case class MapMethodScala(scalanName: String, overloadId: Option[String], scalanArgs: Seq[Symbol], mappedName: String, typeArgs: Option[Seq[Adjusted[Symbol]]], args: Option[Seq[Adjusted[Symbol]]], implicitArgs: Option[Seq[Adjusted[Symbol]]], isStatic: Boolean) extends MethodBuilder[ScalaLibrary, ScalaType, ScalaMethod, MapMethodScala] {
    def to(mappedName: String) = copy(mappedName = mappedName)
    def typeArgs(typeArgs: Adjusted[Symbol]*): MapMethodScala = copy(typeArgs = Some(typeArgs))
    def args(args: Adjusted[Symbol]*): MapMethodScala = copy(args = Some(args))
    def implicitArgs(implicitArgs: Adjusted[Symbol]*): MapMethodScala = copy(implicitArgs = Some(implicitArgs))
    def static: MapMethodScala = copy(isStatic = true)

    def apply() = {
      val typeArgOrder = symbolOrder(scalanArgs, typeArgs, scalanName, false)

      val argOrder = symbolOrder(scalanArgs, args, scalanName, true)

      val implicitArgOrder = symbolOrder(scalanArgs, implicitArgs, scalanName, false)

      ScalaMethod(scalanName, overloadId, mappedName, isStatic, typeArgOrder, argOrder, implicitArgOrder)
    }
  }

  object MapMethodScala {
    def apply(scalanName: String, argSyms: Symbol*): MapMethodScala = new MapMethodScala(scalanName, None, argSyms, scalanName, None, None, None, false)
    def apply(scalanName: String, overloadId: String, argSyms: Symbol*): MapMethodScala = new MapMethodScala(scalanName, Some(overloadId), argSyms, scalanName, None, None, None, false)
  }
}

object CxxMapping {
  def MapModuleCxx(moduleName: String): CxxMappingBuilder = new CxxMappingBuilder(moduleName, None, None)
  def MapModuleCxx[A](implicit tag: ClassTag[A]): CxxMappingBuilder = MapModuleCxx(tag.runtimeClass.getName)

  // Distinguish <> and "" headers in the future? For now, always use "", since it works for both.
  case class CxxLibrary(headerName: Option[String], namespace: Option[String])

  case class CxxType(scalanName: String, mappedName: String, templateArgOrder: Seq[Adjusted[Int]], methods: Seq[CxxMethod]) extends TypeRep[CxxMethod]

  case class CxxMethod(scalanName: String, overloadId: Option[String], mappedName: String, templateArgOrder: Seq[Adjusted[Int]], argOrder: Seq[Adjusted[Int]]) extends MethodRep

  case class CxxMappingBuilder(moduleName: String, headerName: Option[String], namespace: Option[String]) extends MappingBuilder[CxxLibrary, CxxType, CxxMethod] {
    def language = CXX
    def withHeader(name: String): CxxMappingBuilder = copy(headerName = Some(name))
    def withNamespace(namespace: String): CxxMappingBuilder = copy(namespace = Some(namespace))
    def library = CxxLibrary(headerName, namespace)

    type TypeBuilderT = MapTypeCxx
  }

  case class MapTypeCxx(scalanName: String, fieldSyms: Seq[Symbol], mappedName: String, templateArgs: Option[Seq[Adjusted[Symbol]]]) extends TypeBuilder[CxxLibrary, CxxType, CxxMethod, MapTypeCxx] {
    def to(mappedName: String) = copy(mappedName = mappedName)
    def templateArgs(templateArgs: Adjusted[Symbol]*): MapTypeCxx = copy(templateArgs = Some(templateArgs))
    def methods(methodBuilders: MethodBuilderT*) = {
      val templateArgOrder = symbolOrder(fieldSyms, templateArgs, scalanName, true)
      CxxType(scalanName, mappedName, templateArgOrder, methodBuilders.map(_.apply()))
    }

    type MethodBuilderT = MapMethodCxx
  }

  object MapTypeCxx {
    def apply(scalanName: String, fieldSyms: Symbol*): MapTypeCxx = new MapTypeCxx(scalanName, fieldSyms, scalanName, None)
  }

  case class MapMethodCxx(scalanName: String, overloadId: Option[String], scalanArgs: Seq[Symbol], mappedName: String, templateArgs: Option[Seq[Adjusted[Symbol]]], args: Option[Seq[Adjusted[Symbol]]], implicitArgs: Option[Seq[Adjusted[Symbol]]]) extends MethodBuilder[CxxLibrary, CxxType, CxxMethod, MapMethodCxx] {
    def to(mappedName: String) = copy(mappedName = mappedName)
    def templateArgs(templateArgs: Adjusted[Symbol]*): MapMethodCxx = copy(templateArgs = Some(templateArgs))
    def args(args: Adjusted[Symbol]*): MapMethodCxx = copy(args = Some(args))
    def implicitArgs(implicitArgs: Adjusted[Symbol]*): MapMethodCxx = copy(implicitArgs = Some(implicitArgs))

    def apply() = {
      val templateArgOrder = symbolOrder(scalanArgs, templateArgs, scalanName, false)

      val argOrder = symbolOrder(scalanArgs, args, scalanName, true)

      CxxMethod(scalanName, overloadId, mappedName, templateArgOrder, argOrder)
    }
  }

  object MapMethodCxx {
    def apply(scalanName: String, argSyms: Symbol*): MapMethodCxx = new MapMethodCxx(scalanName, None, argSyms, scalanName, None, None, None)
    def apply(scalanName: String, overloadId: String, argSyms: Symbol*): MapMethodCxx = new MapMethodCxx(scalanName, Some(overloadId), argSyms, scalanName, None, None, None)
  }
}

trait MethodMappingDSL {
  implicit def mm: MethodMappingDSL = this

  type AMapping = Mapping[LibraryT, TypeT, MethodT] forSome {
    type LibraryT
    type TypeT <: TypeRep[MethodT]
    type MethodT <: MethodRep
  }

  val mappingDSLs: mutable.Map[LanguageId, ArrayBuffer[AMapping]] = mutable.Map.empty
  def addMapping(language: LanguageId, mapping: AMapping) =
    mappingDSLs.getOrElseUpdate(language, ArrayBuffer.empty) += mapping

//  abstract class Mapping(languageId: LanguageId) {
//    type Func
//    trait TypeT {
//      def name: String
//    }
//
//    mappingDSLs.get(languageId) match {
//      case Some(conf) => conf += this
//      case _ => mappingDSLs += languageId -> mutable.ArrayBuffer(this)
//    }
//
//    def get(className: String, method: String): Option[Func] = {
//      methodMap.get((className, method))
//    }
//
//    val libPaths: Set[String]
//
//    val functionMap: Map[EMethod, Func]
//
//    val classMap: Map[Class[_], TypeT] = Map.empty[Class[_], TypeT]
//
//    // To simplify config usage, data are transformed to Backend representation. Direct link to LanguageConf is never used
//    lazy val methodMap: Map[(String, String), Func] = functionMap.map { case (m, f) =>
//      val prefix = m.eType.module match {
//        case null =>
//          m.eType.ePackage match {
//            case null => ""
//            case p => p.name + "."
//          }
//        case family => family.ePackage.name + "." + family.name + "$"
//      }
//      ((prefix + m.eType.name, m.name), f)
//    }
//  }
//
//  trait MappingTags {
//    val mapping: Mapping
//
//    trait Lib {
//      libs += this
//    }
//
//    val libs: mutable.HashSet[Lib] = new mutable.HashSet()
//  }
//
//  trait CppMappingDSL extends MappingTags {
//
//    case class CppLib(hfile: String, libfile: String) extends Lib with Implicit[CppLib]
//
//    case class CppType(name: String)
//
//    case class CppArg(ty: CppType, name: String)
//
//    case class CppFunc(name: String, args: CppArg*)(implicit val lib: CppLib)
//
//    abstract class CppMapping extends Mapping(CPP) {
//      type Func = CppFunc
//
//      lazy val libPaths: Set[String] = Set.empty[String]
//    }
//  }
//
//  trait ScalaMappingDSL extends MappingTags {
//
//    case class ScalaLib(jar: String = "", pack: String = "") extends Lib with Implicit[ScalaLib]
//
//    case class EmbeddedObject(name: String) extends Lib with Implicit[EmbeddedObject]
//
//    case class ScalaType(name: String)
//
//    case class ScalaArg(ty: ScalaType, name: String)
//
//    case class ScalaFunc(name: String, args: ScalaArg*)(val wrapper: Boolean = false)(implicit val lib: Lib)
//
//    case class ScalaClass(name: String)
//
//    abstract class ScalaMapping extends Mapping(SCALA) {
//      type Func = ScalaFunc
//      type TypeT = ScalaClass
//
//      lazy val libPaths: Set[String] = libs.collect {
//        case l: ScalaLib => l.jar
//      }.filter(_.nonEmpty).toSet
//    }
//  }
}
