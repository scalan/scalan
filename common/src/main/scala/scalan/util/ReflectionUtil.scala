package scalan.util

import scala.reflect.{ReflectionUtil0, runtime}
import scala.reflect.runtime.universe._

object ReflectionUtil {
  def typeSymbol[A: TypeTag] = typeOf[A].typeSymbol

  def annotation[T: TypeTag](symbol: Symbol) = symbol.annotations.find {
    _.tree.tpe =:= typeOf[T]
  }

  def methodToJava(sym: MethodSymbol) = ReflectionUtil0.methodToJava(sym)

  def primaryConstructor(tpe: Type) = {
    val constructorSymbol = tpe.decl(termNames.CONSTRUCTOR)
    constructorSymbol match {
      case ctorTermSymbol: TermSymbol =>
        if (ctorTermSymbol.isOverloaded) {
          val constructors = ctorTermSymbol.alternatives
          constructors.collectFirst {
            case c: MethodSymbol if c.isPrimaryConstructor => c
          }
        } else {
          Some(ctorTermSymbol.asMethod)
        }
      case NoSymbol => None
    }
  }

  def classToSymbol(clazz: Class[_]) =
    runtimeMirror(clazz.getClassLoader).classSymbol(clazz)

  def simplifyType(tpe: Type) = {
    val tpe1 = tpe match {
      case NullaryMethodType(returnTpe) => returnTpe
      case _ => tpe
    }
    tpe1.dealias
  }

  def paramFieldMirrors(clazz: Class[_], instanceMirror: InstanceMirror, knownSupertypeFieldSyms: TermSymbol*) = {
    val tpe = classToSymbol(clazz).toType
    val constructor = primaryConstructor(tpe).getOrElse {
      throw new ScalaReflectionException(s"Primary constructor for class $clazz not found")
    }
    val ctorParams = constructor.paramLists.flatten
    val knownSupertypeFieldsWithTypes = knownSupertypeFieldSyms.map(f => f -> simplifyType(f.typeSignatureIn(tpe)))
    ctorParams.map { sym =>
      val fieldSym = tpe.decl(sym.name).asTerm
      val fieldType = ReflectionUtil.simplifyType(fieldSym.typeSignature)
      // workaround for http://stackoverflow.com/questions/32118877/compiler-doesnt-generate-a-field-for-implicit-val-when-an-implicit-val-with-the
      // this would be handled by below try-catch as well, but is common
      // enough to handle specially
      val fieldSym1 = knownSupertypeFieldsWithTypes.collectFirst {
        case (f, t) if t == fieldType => f
      }.getOrElse(fieldSym)

      try {
        instanceMirror.reflectField(fieldSym1)
      } catch {
        case e: Exception =>
          // this is represented by a real field in a supertype, find it
          val realFieldSym = tpe.members.collectFirst {
            case f: TermSymbol if f.isGetter && ReflectionUtil.simplifyType(f.typeSignatureIn(tpe)) == fieldType => f.accessed.asTerm
          }.getOrElse {
            throw new ScalaReflectionException(s"Failed to find the field corresponding to ${tpe}.${sym.name}")
          }
          instanceMirror.reflectField(realFieldSym)
      }
    }
  }
}
