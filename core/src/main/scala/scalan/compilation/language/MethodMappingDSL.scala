package scalan.compilation.language

import java.io.File
import java.lang.reflect.Method
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scalan.util.ReflectionUtil

trait Adjustment {
  def apply[A](x: A) = Adjusted(x, Some(this))
}

/** Converts a function to an Ordering object */
object MkOrdering extends Adjustment

/** Converts a function A => B to (&A, &B) => void (for C++) */
object VoidInOut extends Adjustment

/** Converts a enum to its index */
object EnumIndex extends Adjustment

// TODO currently adjustments are simply ignored in MethodCallBridge
// maybe adjustments: Seq[Adjustment] instead?
case class Adjusted[+A](value: A, adjustment: Option[Adjustment]) {
  def adjust(adjustment: Adjustment) = copy(adjustment = Some(adjustment))
  def map[B](f: A => B) = copy(value = f(value))
}

object Adjusted {
  implicit def apply[A](x: A): Adjusted[A] = Adjusted(x, None)
}

trait LanguageId

// TODO Should this be unified with ScalaMapping/CxxMapping?
case object SCALA extends LanguageId
case object CXX extends LanguageId

class Mapping[LibraryT, TypeT <: TypeRep[MethodT], MethodT <: MethodRep](library: LibraryT, types: Map[String, (TypeT, Map[(String, Option[String]), MethodT])]) {
  private def supertypes(clazz: Class[_]): Iterable[Class[_]] = {
    val superclass = clazz.getSuperclass
    val immediateSupertypes =
      (if (superclass != null) Iterable(superclass) else Iterable.empty[Class[_]]) ++ clazz.getInterfaces
    immediateSupertypes ++ immediateSupertypes.flatMap(supertypes)
  }
  private def supertypesIncluding(clazz: Class[_]) = Iterable(clazz) ++ supertypes(clazz)

  def getMethod(method: Method): Option[(LibraryT, TypeT, MethodT)] = {
    val overloadId = ReflectionUtil.overloadId(method)
    val methodName = method.getName

    supertypesIncluding(method.getDeclaringClass).
      map(clazz => getMethod(clazz.getName, methodName, overloadId)).collectFirst {
        case Some(x) => x
      }
  }
  def getMethod(className: String, methodName: String, overloadId: Option[String]): Option[(LibraryT, TypeT, MethodT)] = {
    val typeMapping = types.get(className)
    typeMapping.flatMap(pair => pair._2.get((methodName, overloadId)).map(methodT => (library, pair._1, methodT)))
  }
  // TODO handle type arguments
  def getType(m: Manifest[_]): Option[(LibraryT, TypeT)] = {
    supertypesIncluding(m.runtimeClass).map(clazz => getType(clazz.getName)).collectFirst {
      case Some(x) => x
    }
  }
  def getType(className: String) = types.get(className).map(pair => (library, pair._1))
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
  def isStatic: Boolean
  def isInternal: Boolean
  def receiverIndex: Adjusted[Int]
  def argOrder: Seq[Adjusted[Int]]
}

object MappingBuilder {
  def symbolIndex(originalArgs: Seq[Symbol], symbol: Adjusted[Symbol], name: String) = symbol.map {
    case 'this => -1
    case a =>
      originalArgs.indexOf(a) match {
        case -1 =>
          throw new NoSuchElementException(s"$a not found in argument list $name(${originalArgs.mkString(", ")})")
        case index => index
      }
  }

  def symbolOrder(originalArgs: Seq[Symbol], reorderedArgs: Seq[Adjusted[Symbol]], name: String) = {
    reorderedArgs.map(argSym => symbolIndex(originalArgs, argSym, name))
  }
}

import MappingBuilder.{symbolIndex, symbolOrder}

trait TypeBuilder[LibraryT, TypeT, MethodT, TypeBuilderT] {
  type MethodBuilderT <: MethodBuilder[LibraryT, TypeT, MethodT, MethodBuilderT]

  def to(mappedName: String): TypeBuilderT
  def methods(methodBuilders: MethodBuilderT*): TypeT
}

trait MethodBuilder[LibraryT, TypeT, MethodT, MethodBuilderT] {
  def to(mappedName: String): MethodBuilderT
  def apply(): MethodT
}

object ScalaMapping {
  def MapModuleScala(moduleName: String): ScalaMappingBuilder = new ScalaMappingBuilder(moduleName, None, Nil)
  def MapModuleScala[A](implicit tag: ClassTag[A]): ScalaMappingBuilder = MapModuleScala(tag.runtimeClass.getName)

  case class ScalaLibrary(packageName: Option[String], jars: Seq[File]) // Add SBT dependency support?

  case class ScalaType(scalanName: String, mappedName: String, methods: Seq[ScalaMethod]) extends TypeRep[ScalaMethod] // TODO constructor arguments

  case class ScalaMethod(scalanName: String, overloadId: Option[String], mappedName: String, isStatic: Boolean, isInternal: Boolean, receiverIndex: Adjusted[Int], typeArgOrder: Seq[Adjusted[Int]], argOrder: Seq[Adjusted[Int]], implicitArgOrder: Seq[Adjusted[Int]]) extends MethodRep

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
    def apply(scalanName: String, fieldSyms: Symbol*): MapTypeScala =
      new MapTypeScala(scalanName, fieldSyms, scalanName)
    def apply[A](fieldSyms: Symbol*)(implicit tag: ClassTag[A]): MapTypeScala =
      apply(tag.runtimeClass.getSimpleName, fieldSyms: _*)
  }

  case class MapMethodScala(scalanName: String, overloadId: Option[String], scalanArgs: Seq[Symbol], mappedName: String, isStatic: Boolean, isInternal: Boolean, receiverSym: Adjusted[Symbol], typeArgs: Seq[Adjusted[Symbol]], args: Seq[Adjusted[Symbol]], implicitArgs: Seq[Adjusted[Symbol]]) extends MethodBuilder[ScalaLibrary, ScalaType, ScalaMethod, MapMethodScala] {
    def to(mappedName: String) = copy(mappedName = mappedName)
    def internal(mappedName: String) =
      copy(isInternal = true, mappedName = mappedName, args = Adjusted('this) +: this.args)
    def onCompanion = copy(isStatic = true)
    def onArg(receiver: Adjusted[Symbol]) =
      copy(receiverSym = receiver, args = this.args.filter(_.value != receiver.value))
    def typeArgs(typeArgs: Adjusted[Symbol]*): MapMethodScala = copy(typeArgs = typeArgs)
    def args(args: Adjusted[Symbol]*): MapMethodScala = copy(args = args)
    def implicitArgs(implicitArgs: Adjusted[Symbol]*): MapMethodScala = copy(implicitArgs = implicitArgs)

    def apply() = {
      val receiverIndex = symbolIndex(scalanArgs, receiverSym, scalanName)
      val typeArgOrder = symbolOrder(scalanArgs, typeArgs, scalanName)
      val argOrder = symbolOrder(scalanArgs, args, scalanName)
      val implicitArgOrder = symbolOrder(scalanArgs, implicitArgs, scalanName)

      ScalaMethod(scalanName, overloadId, mappedName, isStatic, isInternal, receiverIndex, typeArgOrder, argOrder, implicitArgOrder)
    }
  }

  object MapMethodScala {
    def apply(scalanName: String, argSyms: Symbol*): MapMethodScala = new MapMethodScala(scalanName, None, argSyms, scalanName, false, false, 'this, Nil, argSyms.map(Adjusted(_)), Nil)
    def apply(scalanName: String, overloadId: String, argSyms: Symbol*): MapMethodScala = new MapMethodScala(scalanName, Some(overloadId), argSyms, scalanName, false, false, 'this, Nil, argSyms.map(Adjusted(_)), Nil)
  }
}

object CxxMapping {
  def MapModuleCxx(moduleName: String): CxxMappingBuilder = new CxxMappingBuilder(moduleName, None, None)
  def MapModuleCxx[A](implicit tag: ClassTag[A]): CxxMappingBuilder = MapModuleCxx(tag.runtimeClass.getName)

  // Distinguish <> and "" headers in the future? For now, always use "", since it works for both.
  case class CxxLibrary(headerName: Option[String], namespace: Option[String])

  case class CxxType(scalanName: String, mappedName: String, templateArgOrder: Seq[Adjusted[Int]], methods: Seq[CxxMethod]) extends TypeRep[CxxMethod]

  case class CxxMethod(scalanName: String, overloadId: Option[String], mappedName: String, isStatic: Boolean, isInternal: Boolean, receiverIndex: Adjusted[Int], templateArgOrder: Seq[Adjusted[Int]], argOrder: Seq[Adjusted[Int]]) extends MethodRep

  case class CxxMappingBuilder(moduleName: String, headerName: Option[String], namespace: Option[String]) extends MappingBuilder[CxxLibrary, CxxType, CxxMethod] {
    def language = CXX
    def withHeader(name: String): CxxMappingBuilder = copy(headerName = Some(name))
    def withNamespace(namespace: String): CxxMappingBuilder = copy(namespace = Some(namespace))
    def library = CxxLibrary(headerName, namespace)

    type TypeBuilderT = MapTypeCxx
  }

  case class MapTypeCxx(scalanName: String, fieldSyms: Seq[Symbol], mappedName: String, templateArgs: Seq[Adjusted[Symbol]]) extends TypeBuilder[CxxLibrary, CxxType, CxxMethod, MapTypeCxx] {
    def to(mappedName: String) = copy(mappedName = mappedName)
    def methods(methodBuilders: MethodBuilderT*) =
      CxxType(scalanName, mappedName, Nil /* TODO use templateArgs */, methodBuilders.map(_.apply()))

    type MethodBuilderT = MapMethodCxx
  }

  object MapTypeCxx {
    def apply(scalanName: String, fieldSyms: Symbol*): MapTypeCxx =
      new MapTypeCxx(scalanName, fieldSyms, scalanName, Nil)
    def apply[A](fieldSyms: Symbol*)(implicit tag: ClassTag[A]): MapTypeCxx =
      apply(tag.runtimeClass.getSimpleName, fieldSyms: _*)
  }

  case class MapMethodCxx(scalanName: String, overloadId: Option[String], scalanArgs: Seq[Symbol], mappedName: String, isStatic: Boolean, isInternal: Boolean, receiverSym: Adjusted[Symbol], templateArgs: Seq[Adjusted[Symbol]], args: Seq[Adjusted[Symbol]]) extends MethodBuilder[CxxLibrary, CxxType, CxxMethod, MapMethodCxx] {
    def to(mappedName: String) = copy(mappedName = mappedName)
    def static = copy(isStatic = true)
    def internal(mappedName: String) =
      copy(isInternal = true, mappedName = mappedName, args = Adjusted('this) +: this.args)
    def templateArgs(templateArgs: Adjusted[Symbol]*): MapMethodCxx = copy(templateArgs = templateArgs)
    def args(args: Adjusted[Symbol]*): MapMethodCxx = copy(args = args)

    def apply() = {
      val receiverIndex = symbolIndex(scalanArgs, receiverSym, scalanName)
      val templateArgOrder = symbolOrder(scalanArgs, templateArgs, scalanName)
      val argOrder = symbolOrder(scalanArgs, args, scalanName)

      CxxMethod(scalanName, overloadId, mappedName, isStatic, isInternal, receiverIndex, templateArgOrder, argOrder)
    }
  }

  object MapMethodCxx {
    def apply(scalanName: String, argSyms: Symbol*): MapMethodCxx =
      new MapMethodCxx(scalanName, None, argSyms, scalanName, false, false, 'this, Nil, argSyms.map(Adjusted(_)))
    def apply(scalanName: String, overloadId: String, argSyms: Symbol*): MapMethodCxx =
      new MapMethodCxx(scalanName, Some(overloadId), argSyms, scalanName, false, false, 'this, Nil, argSyms.map(Adjusted(_)))
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
}
