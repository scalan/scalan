package scalan.util

import java.lang.reflect.{Method, AnnotatedElement}

import scala.reflect.{classTag, ClassTag}
import scala.reflect.runtime.universe._
import scalan.OverloadId

sealed trait ParamMirror {
  def bind(newReceiver: Any): ParamMirror
  def get: Any
  def name: String
}
private[util] case class FMirror(fieldMirror: FieldMirror) extends ParamMirror {
  def bind(newReceiver: Any) = FMirror(fieldMirror.bind(newReceiver))
  def get = fieldMirror.get
  def name = fieldMirror.symbol.name.toString
}
private[util] case class GMirror(methodMirror: MethodMirror) extends ParamMirror {
  def bind(newReceiver: Any) = GMirror(methodMirror.bind(newReceiver))
  def get = methodMirror.apply()
  def name = methodMirror.symbol.name.toString
}

object ReflectionUtil {
  def typeSymbol[A: TypeTag] = typeOf[A].typeSymbol

  def annotation[T: TypeTag](symbol: Symbol) = symbol.annotations.find {
    _.tree.tpe =:= typeOf[T]
  }

  def jAnnotation[A <: java.lang.annotation.Annotation : ClassTag](element: AnnotatedElement) =
    Option(element.getAnnotation(classTag[A].runtimeClass.asInstanceOf[Class[A]]))

  def overloadId(method: Method) = jAnnotation[OverloadId](method).map(_.value)

  def methodToJava(sym: MethodSymbol): Method = ???

  def classToSymbol(clazz: Class[_]) =
    runtimeMirror(clazz.getClassLoader).classSymbol(clazz)

  private def constructorParams(clazzSym: ClassSymbol) = {
    val constructor = clazzSym.primaryConstructor.asMethod
    if (constructor == NoSymbol) {
      throw new ScalaReflectionException(s"Primary constructor for class ${clazzSym.name.toString} not found")
    }
    constructor.paramLists.flatten
  }

  def paramMirrors(instance: Any): List[ParamMirror] = {
    val clazz = instance.getClass
    val javaMirror = runtimeMirror(clazz.getClassLoader)
    val instanceMirror = javaMirror.reflect(instance)
    val clazzSym = javaMirror.classSymbol(clazz)

    val ctorParams = constructorParams(clazzSym)
    val tpe = clazzSym.toType

    ctorParams.map { sym =>
      val overloads = tpe.members.filter(_.name == sym.name).toList
      val methods = overloads.collect {
        case methodSym: MethodSymbol if methodSym.paramLists.isEmpty =>
          methodSym
        case fieldSym: TermSymbol if fieldSym.getter != NoSymbol =>
          fieldSym.getter.asMethod
      }.toSet
      methods.size match {
        case 0 =>
          try {
            // overloads should only include fields, because methods is empty
            // If this fails (overloads is empty or Java field isn't found),
            // produce a message saying how to fix this
            overloads match {
              case List(field) =>
                FMirror(instanceMirror.reflectField(field.asTerm))
            }
          } catch {
            case _: Throwable =>
              val tpe = sym.typeSignature
              val typeArgs = tpe.typeArgs
              val errorMessage = if (sym.name.toString.startsWith("evidence$") && typeArgs.length == 1) {
                val typeArg = typeArgs.head.typeSymbol.name.toString
                val bound = tpe.typeConstructor.typeSymbol.name.toString
                val paramName = bound match {
                  case "Elem" => s"e$typeArg"
                  case "Cont" => s"c$typeArg"
                  case _ => StringUtil.lowerCaseFirst(bound) + typeArg
                }
                val decl = s"val $paramName: $bound[$typeArg]"
                s"""Declaration of $clazz appears to use a context bound `$typeArg: $bound`.
                   |It must be changed to an `implicit val` parameter: if the class has no implicit parameters,
                   |add a new parameter list `(implicit $decl)`. If it already has an implicit parameter list, add `$decl` to it.
                   |See also http://docs.scala-lang.org/tutorials/FAQ/context-and-view-bounds.html#what-is-a-context-bound
                   |
                   |If there is no such bound and ${sym.name} is a parameter name, declare it as a `val`.""".stripMargin
              } else {
                s"Declaration of class $clazz has a non-`val` parameter ${sym.name}. Declare it as a `val`."
              }
              throw new ScalaReflectionException(errorMessage)
          }
        case 1 =>
          GMirror(instanceMirror.reflectMethod(methods.head))
        case n =>
          throw new ScalaReflectionException(s"$n parameterless methods called ${sym.name} found in class $clazz. This should never happen.")
      }
    }
  }

  /** Returns the superclass for an anonymous class produced by mixing in traits; the argument otherwise. */
  def namedSuperclass(clazz: Class[_]) = {
    if (clazz.getSimpleName.contains("$anon$")) {
      val superclass = clazz.getSuperclass
      if (superclass == classOf[Object]) {
        // clazz is composed of traits only, return the first one
        clazz.getInterfaces.head
      } else
        superclass
    } else
      clazz
  }

  // Implemented in internal/Symbols.scala, but not exposed
  /** True if the symbol represents an anonymous class */
  def isAnonymousClass(symbol: Symbol) = symbol.isClass && symbol.name.toString.contains("$anon")

  /** A string describing the argument which allows to distinguish between overloads and overrides, unlike MethodSymbol.toString */
  def showMethod(m: MethodSymbol) = {
    val typeParams = m.typeParams match {
      case Nil => ""
      case typeParams => typeParams.map(_.name).mkString("[", ", ", "]")
    }
    val params =
      m.paramLists.map(_.map(sym => s"${sym.name}: ${sym.typeSignature}").mkString("(", ", ", ")")).mkString("")
    s"${m.owner.name}.${m.name}$typeParams$params"
  }

  def typeTagToClass(tag: WeakTypeTag[_]): Class[_] = tag.mirror.runtimeClass(tag.tpe)
}
