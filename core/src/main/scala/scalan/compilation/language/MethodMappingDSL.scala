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

trait LanguageMapping {
  type LibraryT
  type TypeT <: TypeRep
  type MethodT <: MethodRep
  type ModuleBuilderT <: ModuleMappingBuilder
  type TypeBuilderT <: TypeBuilder
  type MethodBuilderT <: MethodBuilder

  trait TypeRep {
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

  protected def symbolIndex(originalArgs: Seq[Symbol], symbol: Adjusted[Symbol], name: String) = symbol.map {
    case 'this => -1
    case a =>
      originalArgs.indexOf(a) match {
        case -1 =>
          throw new NoSuchElementException(s"$a not found in argument list $name(${originalArgs.mkString(", ")})")
        case index => index
      }
  }

  protected def symbolOrder(originalArgs: Seq[Symbol], reorderedArgs: Seq[Adjusted[Symbol]], name: String) = {
    reorderedArgs.map(argSym => symbolIndex(originalArgs, argSym, name))
  }

  class ModuleMapping(library: LibraryT, types: Map[String, (TypeT, Map[(String, Option[String]), MethodT])]) {
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

  trait ModuleMappingBuilder {
    def moduleName: String
    def library: LibraryT
    def types(types: TypeT*)(implicit outer: MethodMappingDSL) = {
      val typeMap = types.map { sType =>
        val fullClassName = moduleName + "$" + sType.scalanName
        val methodMap = sType.methods.map { sMethod =>
          ((sMethod.scalanName, sMethod.overloadId), sMethod)
        }.toMap
        (fullClassName, (sType, methodMap))
      }.toMap
      val mapping = new ModuleMapping(library, typeMap)
      outer.addMapping(LanguageMapping.this)(mapping)
    }
  }

  trait TypeBuilder {
    def to(mappedName: String): TypeBuilderT
    def methods(methodBuilders: MethodBuilderT*): TypeT
  }

  trait MethodBuilder {
    def to(mappedName: String): MethodBuilderT
    def apply(): MethodT
  }

  def mapModule(moduleName: String): ModuleBuilderT
  def mapModule[A](implicit tag: ClassTag[A]): ModuleBuilderT = mapModule(tag.runtimeClass.getName)

  def mapType(scalanName: String, fieldSyms: Symbol*): TypeBuilderT
  def mapType[A](fieldSyms: Symbol*)(implicit tag: ClassTag[A]): TypeBuilderT =
    mapType(tag.runtimeClass.getSimpleName, fieldSyms: _*)

  def mapMethod(scalanName: String, overloadIdOpt: Option[String], argSyms: Symbol*): MethodBuilderT
  def mapMethod(scalanName: String, argSyms: Symbol*): MethodBuilderT =
    mapMethod(scalanName, None, argSyms: _*)
  def mapMethod(scalanName: String, overloadId: String, argSyms: Symbol*): MethodBuilderT =
    mapMethod(scalanName, Some(overloadId), argSyms: _*)
}

case object Scala extends LanguageMapping {
  type LibraryT = ScalaLibrary
  type TypeT = ScalaType
  type MethodT = ScalaMethod
  type ModuleBuilderT = ScalaModuleMappingBuilder
  type TypeBuilderT = MapTypeScala
  type MethodBuilderT = MapMethodScala

  def mapModule(moduleName: String) = new ScalaModuleMappingBuilder(moduleName, None, Nil)
  def mapType(scalanName: String, fieldSyms: Symbol*) = new MapTypeScala(scalanName, fieldSyms, scalanName)
  def mapMethod(scalanName: String, overloadIdOpt: Option[String], argSyms: Symbol*) =
    new MapMethodScala(scalanName, overloadIdOpt, argSyms, scalanName, false, false, 'this, Nil, argSyms.map(Adjusted(_)), Nil)

  case class ScalaLibrary(packageName: Option[String], jars: Seq[File]) // Add SBT dependency support?

  case class ScalaType(scalanName: String, mappedName: String, methods: Seq[ScalaMethod]) extends TypeRep // TODO constructor arguments

  case class ScalaMethod(scalanName: String, overloadId: Option[String], mappedName: String, isStatic: Boolean, isInternal: Boolean, receiverIndex: Adjusted[Int], typeArgOrder: Seq[Adjusted[Int]], argOrder: Seq[Adjusted[Int]], implicitArgOrder: Seq[Adjusted[Int]]) extends MethodRep

  case class ScalaModuleMappingBuilder(moduleName: String, packageName: Option[String], jars: Seq[File]) extends ModuleMappingBuilder {
    def packageName(name: String): ScalaModuleMappingBuilder = copy(packageName = Some(name))
    def jars(files: File*) = copy(jars = this.jars ++ files)
    def library = ScalaLibrary(packageName, jars)
  }

  case class MapTypeScala(scalanName: String, fieldSyms: Seq[Symbol], mappedName: String) extends TypeBuilder {
    def to(mappedName: String) = copy(mappedName = mappedName)
    def methods(methodBuilders: MethodBuilderT*) =
      ScalaType(scalanName, mappedName, methodBuilders.map(_.apply()))
  }

  case class MapMethodScala(scalanName: String, overloadId: Option[String], scalanArgs: Seq[Symbol], mappedName: String, isStatic: Boolean, isInternal: Boolean, receiverSym: Adjusted[Symbol], typeArgs: Seq[Adjusted[Symbol]], args: Seq[Adjusted[Symbol]], implicitArgs: Seq[Adjusted[Symbol]]) extends MethodBuilder {
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
}

case object Cxx extends LanguageMapping {
  type LibraryT = CxxLibrary
  type TypeT = CxxType
  type MethodT = CxxMethod
  type ModuleBuilderT = CxxModuleMappingBuilder
  type TypeBuilderT = MapTypeCxx
  type MethodBuilderT = MapMethodCxx

  def mapModule(moduleName: String) = new CxxModuleMappingBuilder(moduleName, None, None)
  def mapType(scalanName: String, fieldSyms: Symbol*) =
    new MapTypeCxx(scalanName, fieldSyms, scalanName, Nil)
  def mapMethod(scalanName: String, overloadIdOpt: Option[String], argSyms: Symbol*) =
    new MapMethodCxx(scalanName, overloadIdOpt, argSyms, scalanName, false, false, 'this, Nil, argSyms.map(Adjusted(_)))

  // Distinguish <> and "" headers in the future? For now, always use "", since it works for both.
  case class CxxLibrary(headerName: Option[String], namespace: Option[String])

  case class CxxType(scalanName: String, mappedName: String, templateArgOrder: Seq[Adjusted[Int]], methods: Seq[CxxMethod]) extends TypeRep

  case class CxxMethod(scalanName: String, overloadId: Option[String], mappedName: String, isStatic: Boolean, isInternal: Boolean, receiverIndex: Adjusted[Int], templateArgOrder: Seq[Adjusted[Int]], argOrder: Seq[Adjusted[Int]]) extends MethodRep

  case class CxxModuleMappingBuilder(moduleName: String, headerName: Option[String], namespace: Option[String]) extends ModuleMappingBuilder {
    def withHeader(name: String): CxxModuleMappingBuilder = copy(headerName = Some(name))
    def withNamespace(namespace: String): CxxModuleMappingBuilder = copy(namespace = Some(namespace))
    def library = CxxLibrary(headerName, namespace)
  }

  case class MapTypeCxx(scalanName: String, fieldSyms: Seq[Symbol], mappedName: String, templateArgs: Seq[Adjusted[Symbol]]) extends TypeBuilder {
    def to(mappedName: String) = copy(mappedName = mappedName)
    def methods(methodBuilders: MethodBuilderT*) =
      CxxType(scalanName, mappedName, Nil /* TODO use templateArgs */, methodBuilders.map(_.apply()))
  }

  case class MapMethodCxx(scalanName: String, overloadId: Option[String], scalanArgs: Seq[Symbol], mappedName: String, isStatic: Boolean, isInternal: Boolean, receiverSym: Adjusted[Symbol], templateArgs: Seq[Adjusted[Symbol]], args: Seq[Adjusted[Symbol]]) extends MethodBuilder {
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
}

trait MethodMappingDSL {
  implicit def mm: MethodMappingDSL = this

  private val moduleMappings: mutable.Map[LanguageMapping, ArrayBuffer[LanguageMapping#ModuleMapping]] = mutable.Map.empty
  def addMapping(language: LanguageMapping)(mapping: language.ModuleMapping) =
    moduleMappings.getOrElseUpdate(language, ArrayBuffer.empty) += mapping
  def mappingsFor(language: LanguageMapping) =
    moduleMappings.getOrElse(language, Nil).asInstanceOf[Seq[language.ModuleMapping]]
}
