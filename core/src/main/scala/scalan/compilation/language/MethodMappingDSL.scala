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
  type Library
  // avoid conflict with scala.reflect.runtime.universe.Type
  type TypeT <: AbstractType
  // avoid conflict with java.lang.reflect.Method
  type MethodT <: AbstractMethod
  type ModuleBuilder <: AbstractModuleBuilder
  type TypeBuilder <: AbstractTypeBuilder
  type MethodBuilder <: AbstractMethodBuilder

  trait AbstractType {
    def scalanName: String
    def mappedName: String
    def methods: Seq[MethodT]
  }

  trait AbstractMethod {
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

  class ModuleMapping(library: Library, types: Map[String, (TypeT, Map[(String, Option[String]), MethodT])]) {
    private def supertypes(clazz: Class[_]): Iterable[Class[_]] = {
      val superclass = clazz.getSuperclass
      val immediateSupertypes =
        (if (superclass != null) Iterable(superclass) else Iterable.empty[Class[_]]) ++ clazz.getInterfaces
      immediateSupertypes ++ immediateSupertypes.flatMap(supertypes)
    }
    private def supertypesIncluding(clazz: Class[_]) = Iterable(clazz) ++ supertypes(clazz)

    def getMethod(method: Method): Option[MethodMapping] = {
      val overloadId = ReflectionUtil.overloadId(method)
      val methodName = method.getName

      supertypesIncluding(method.getDeclaringClass).
        map(clazz => getMethod(clazz.getName, methodName, overloadId)).collectFirst {
          case Some(x) => x
        }
    }
    def getMethod(className: String, methodName: String, overloadId: Option[String]): Option[MethodMapping] = {
      val typeMapping = types.get(className)
      typeMapping.flatMap {
        pair => pair._2.get((methodName, overloadId)).map { methodT => MethodMapping(library, pair._1, methodT) }
      }
    }
    // TODO handle type arguments
    def getType(m: Manifest[_]): Option[TypeMapping] = {
      supertypesIncluding(m.runtimeClass).map(clazz => getType(clazz.getName)).collectFirst {
        case Some(x) => x
      }
    }
    def getType(className: String): Option[TypeMapping] =
      types.get(className).map(pair => TypeMapping(library, pair._1))
  }

  case class TypeMapping(library: Library, tpe: TypeT)

  case class MethodMapping(library: Library, tpe: TypeT, method: MethodT)

  trait AbstractModuleBuilder {
    def moduleName: String
    def library: Library
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

  trait AbstractTypeBuilder {
    def to(mappedName: String): TypeBuilder
    def methods(methodBuilders: MethodBuilder*): TypeT
  }

  trait AbstractMethodBuilder {
    def to(mappedName: String): MethodBuilder
    def apply(): MethodT
  }

  def mapModule(moduleName: String): ModuleBuilder
  def mapModule[A](implicit tag: ClassTag[A]): ModuleBuilder = mapModule(tag.runtimeClass.getName)

  def mapType(scalanName: String, fieldSyms: Symbol*): TypeBuilder
  def mapType[A](fieldSyms: Symbol*)(implicit tag: ClassTag[A]): TypeBuilder =
    mapType(tag.runtimeClass.getSimpleName, fieldSyms: _*)

  def mapMethod(scalanName: String, overloadIdOpt: Option[String], argSyms: Symbol*): MethodBuilder
  def mapMethod(scalanName: String, argSyms: Symbol*): MethodBuilder =
    mapMethod(scalanName, None, argSyms: _*)
  def mapMethod(scalanName: String, overloadId: String, argSyms: Symbol*): MethodBuilder =
    mapMethod(scalanName, Some(overloadId), argSyms: _*)
}

case object Scala extends LanguageMapping {
  def mapModule(moduleName: String) = new ModuleBuilder(moduleName, None, Nil)
  def mapType(scalanName: String, fieldSyms: Symbol*) =
    new TypeBuilder(scalanName, fieldSyms, scalanName)
  def mapMethod(scalanName: String, overloadIdOpt: Option[String], argSyms: Symbol*) =
    new MethodBuilder(scalanName, overloadIdOpt, argSyms, scalanName, false, false, 'this, Nil, argSyms.map(Adjusted(_)), Nil)

  case class Library(packageName: Option[String], jars: Seq[File]) // Add SBT dependency support?

  case class TypeT(scalanName: String, mappedName: String, methods: Seq[MethodT]) extends AbstractType // TODO constructor arguments

  case class MethodT(scalanName: String, overloadId: Option[String], mappedName: String, isStatic: Boolean, isInternal: Boolean, receiverIndex: Adjusted[Int], typeArgOrder: Seq[Adjusted[Int]], argOrder: Seq[Adjusted[Int]], implicitArgOrder: Seq[Adjusted[Int]]) extends AbstractMethod

  case class ModuleBuilder(moduleName: String, packageName: Option[String], jars: Seq[File]) extends AbstractModuleBuilder {
    def packageName(name: String): ModuleBuilder = copy(packageName = Some(name))
    def jars(files: File*) = copy(jars = this.jars ++ files)
    def library = Library(packageName, jars)
  }

  case class TypeBuilder(scalanName: String, fieldSyms: Seq[Symbol], mappedName: String) extends AbstractTypeBuilder {
    def to(mappedName: String) = copy(mappedName = mappedName)
    def methods(methodBuilders: MethodBuilder*) =
      TypeT(scalanName, mappedName, methodBuilders.map(_.apply()))
  }

  case class MethodBuilder(scalanName: String, overloadId: Option[String], scalanArgs: Seq[Symbol], mappedName: String, isStatic: Boolean, isInternal: Boolean, receiverSym: Adjusted[Symbol], typeArgs: Seq[Adjusted[Symbol]], args: Seq[Adjusted[Symbol]], implicitArgs: Seq[Adjusted[Symbol]]) extends AbstractMethodBuilder {
    def to(mappedName: String) = copy(mappedName = mappedName)
    def internal(mappedName: String) =
      copy(isInternal = true, mappedName = mappedName, args = Adjusted('this) +: this.args)
    def onCompanion = copy(isStatic = true)
    def onArg(receiver: Adjusted[Symbol]) =
      copy(receiverSym = receiver, args = this.args.filter(_.value != receiver.value))
    def typeArgs(typeArgs: Adjusted[Symbol]*): MethodBuilder = copy(typeArgs = typeArgs)
    def args(args: Adjusted[Symbol]*): MethodBuilder = copy(args = args)
    def implicitArgs(implicitArgs: Adjusted[Symbol]*): MethodBuilder = copy(implicitArgs = implicitArgs)

    def apply() = {
      val receiverIndex = symbolIndex(scalanArgs, receiverSym, scalanName)
      val typeArgOrder = symbolOrder(scalanArgs, typeArgs, scalanName)
      val argOrder = symbolOrder(scalanArgs, args, scalanName)
      val implicitArgOrder = symbolOrder(scalanArgs, implicitArgs, scalanName)

      MethodT(scalanName, overloadId, mappedName, isStatic, isInternal, receiverIndex, typeArgOrder, argOrder, implicitArgOrder)
    }
  }
}

case object Cxx extends LanguageMapping {
  def mapModule(moduleName: String) = new ModuleBuilder(moduleName, None, None)
  def mapType(scalanName: String, fieldSyms: Symbol*) =
    new TypeBuilder(scalanName, fieldSyms, scalanName, Nil)
  def mapMethod(scalanName: String, overloadIdOpt: Option[String], argSyms: Symbol*) =
    new MethodBuilder(scalanName, overloadIdOpt, argSyms, scalanName, false, false, 'this, Nil, argSyms.map(Adjusted(_)))

  // Distinguish <> and "" headers in the future? For now, always use "", since it works for both.
  case class Library(headerName: Option[String], namespace: Option[String])

  case class TypeT(scalanName: String, mappedName: String, templateArgOrder: Seq[Adjusted[Int]], methods: Seq[MethodT]) extends AbstractType

  case class MethodT(scalanName: String, overloadId: Option[String], mappedName: String, isStatic: Boolean, isInternal: Boolean, receiverIndex: Adjusted[Int], templateArgOrder: Seq[Adjusted[Int]], argOrder: Seq[Adjusted[Int]]) extends AbstractMethod

  case class ModuleBuilder(moduleName: String, headerName: Option[String], namespace: Option[String]) extends AbstractModuleBuilder {
    def withHeader(name: String): ModuleBuilder = copy(headerName = Some(name))
    def withNamespace(namespace: String): ModuleBuilder = copy(namespace = Some(namespace))
    def library = Library(headerName, namespace)
  }

  case class TypeBuilder(scalanName: String, fieldSyms: Seq[Symbol], mappedName: String, templateArgs: Seq[Adjusted[Symbol]]) extends AbstractTypeBuilder {
    def to(mappedName: String) = copy(mappedName = mappedName)
    def methods(methodBuilders: MethodBuilder*) =
      TypeT(scalanName, mappedName, Nil /* TODO use templateArgs */, methodBuilders.map(_.apply()))
  }

  case class MethodBuilder(scalanName: String, overloadId: Option[String], scalanArgs: Seq[Symbol], mappedName: String, isStatic: Boolean, isInternal: Boolean, receiverSym: Adjusted[Symbol], templateArgs: Seq[Adjusted[Symbol]], args: Seq[Adjusted[Symbol]]) extends AbstractMethodBuilder {
    def to(mappedName: String) = copy(mappedName = mappedName)
    def static = copy(isStatic = true)
    def internal(mappedName: String) =
      copy(isInternal = true, mappedName = mappedName, args = Adjusted('this) +: this.args)
    def templateArgs(templateArgs: Adjusted[Symbol]*): MethodBuilder = copy(templateArgs = templateArgs)
    def args(args: Adjusted[Symbol]*): MethodBuilder = copy(args = args)

    def apply() = {
      val receiverIndex = symbolIndex(scalanArgs, receiverSym, scalanName)
      val templateArgOrder = symbolOrder(scalanArgs, templateArgs, scalanName)
      val argOrder = symbolOrder(scalanArgs, args, scalanName)

      MethodT(scalanName, overloadId, mappedName, isStatic, isInternal, receiverIndex, templateArgOrder, argOrder)
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
