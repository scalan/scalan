package scalan.util

import scala.reflect.{ReflectionUtil0, runtime}
import scala.reflect.runtime.universe._

object ReflectionUtil {
  def typeSymbol[A: TypeTag] = typeOf[A].typeSymbol

  def annotation[T: TypeTag](symbol: Symbol) = symbol.annotations.find {
    _.tree.tpe =:= typeOf[T]
  }

  def methodToJava(sym: MethodSymbol) = ReflectionUtil0.methodToJava(sym)

  def classToSymbol(clazz: Class[_]) =
    runtimeMirror(clazz.getClassLoader).classSymbol(clazz)

  def simplifyType(tpe: Type) = {
    val tpe1 = tpe match {
      case NullaryMethodType(returnTpe) => returnTpe
      case _ => tpe
    }
    tpe1.dealias
  }

  private def isFieldOrGetter(s: TermSymbol) = s.isVal || s.isGetter

  def paramFieldMirrors(clazz: Class[_], instanceMirror: InstanceMirror, knownSupertypeFieldSyms: TermSymbol*) = {
    val clazzSym = classToSymbol(clazz)
    val constructor = clazzSym.primaryConstructor.asMethod
    if (constructor == NoSymbol) {
      throw new ScalaReflectionException(s"Primary constructor for class $clazz not found")
    }
    val ctorParams = constructor.paramLists.flatten
    val tpe = clazzSym.toType
    val knownSupertypeFieldsWithTypes = knownSupertypeFieldSyms.map(f => f -> simplifyType(f.typeSignatureIn(tpe)))
    ctorParams.map { sym =>
      val fieldSym = tpe.decl(sym.name).asTerm
      val fieldType = simplifyType(fieldSym.typeSignature)
      // workaround for http://stackoverflow.com/questions/32118877/compiler-doesnt-generate-a-field-for-implicit-val-when-an-implicit-val-with-the
      // this would be handled by below try-catch as well, but is common
      // enough to handle specially
      val fieldSym1 = (fieldSym :: fieldSym.overrides).collectFirst {
        case f: TermSymbol if isFieldOrGetter(f) => f
      }.orElse {
        knownSupertypeFieldsWithTypes.collectFirst {
          case (f, t) if t == fieldType => f
        }
      }.getOrElse(fieldSym)

      try {
        instanceMirror.reflectField(fieldSym1)
      } catch {
        case e: Exception =>
          // this is represented by a real field in a supertype, find it
          val superTypeFieldSyms = tpe.members.flatMap {
            case f: TermSymbol if isFieldOrGetter(f) && simplifyType(f.typeSignatureIn(tpe)) =:= fieldType =>
              (f :: f.overrides).map { case f: TermSymbol => if (isFieldOrGetter(f)) f else NoSymbol }
            case _ => Nil
          }

          if (superTypeFieldSyms.isEmpty) {
            throw new ScalaReflectionException(s"Failed to find the field corresponding to ${tpe}.${sym.name}")
          } else {
            val fieldMirrorsIterator = superTypeFieldSyms.iterator.flatMap { fieldSym =>
              try {
                List(instanceMirror.reflectField(fieldSym.asTerm))
              } catch {
                case e: Exception => Nil
              }
            }
            if (fieldMirrorsIterator.hasNext) {
              fieldMirrorsIterator.next()
            } else {
              throw new ScalaReflectionException(s"No fields in supertypes corresponding to ${tpe}.${sym.name} correspond to Java fields")
            }
          }

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
      m.paramLists.map(_.map(sym => s"${sym.name}: ${sym.typeSignature}").mkString("(", ", ", ")"))
    s"${m.owner.name}.${m.name}$typeParams$params"
  }
}
