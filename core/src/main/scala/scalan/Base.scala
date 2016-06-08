package scalan

import java.util.{Arrays, Objects, Properties}
import java.io.FileReader

import com.typesafe.scalalogging.slf4j.LazyLogging

import scala.annotation.unchecked.uncheckedVariance

trait Base extends LazyLogging { self: Scalan =>
  type |[+A, +B] = Either[A, B]
  type Rep[+A]
  type IntRep = Rep[Int]
  type BoolRep = Rep[Boolean]
  type UnitRep = Rep[Unit]
  type NothingRep = Rep[Nothing]
  type ByteRep = Rep[Byte]
  type ShortRep = Rep[Short]
  type CharRep = Rep[Char]
  type LongRep = Rep[Long]
  type FloatRep = Rep[Float]
  type DoubleRep = Rep[Double]
  type :=>[-A, +B] = PartialFunction[A, B]
  type RFunc[-A,+B] = Rep[Function1[A,B]]

  protected def stagingExceptionMessage(message: String, syms: Seq[Rep[_]]): String

  // Consider if extra data should be Seq[Any] instead (change name in this case)
  class StagingException(message: String, cause: Throwable, val syms: Seq[Rep[_]]) extends
  RuntimeException(stagingExceptionMessage(message, syms), cause) {
    def this(message: String, syms: Seq[Rep[_]]) = this(message, null, syms)
  }

  class NotImplementedStagingException(message: String, syms: Seq[Rep[_]]) extends StagingException(message, null, syms)

  def ??? : Nothing = ???("Missing or incomplete implementation")
  def ???(value: Any, syms: Rep[_]*): Nothing = throw new NotImplementedStagingException(value.toString, syms)

  def !!! : Nothing = !!!("should not be called")
  def !!!(msg: String, syms: Rep[_]*): Nothing = throw new StagingException(msg, syms)
  def !!!(msg: String, e: Throwable, syms: Rep[_]*): Nothing = throw new StagingException(msg, e, syms)

  implicit class RepForSomeExtension(x: Rep[_]) {
    def asRep[T]: Rep[T] = x.asInstanceOf[Rep[T]]
  }
  implicit class RepExtension[A](x: Rep[A]) {
    def asValue: A = valueFromRep(x)
  }

  def toRep[A](x: A)(implicit eA: Elem[A]): Rep[A] = !!!(s"Don't know how to create Rep for $x with element $eA")
  implicit def liftToRep[A:Elem](x: A): Rep[A] = toRep(x)

  def valueFromRep[A](x: Rep[A]): A

  trait Def[+T] extends Product {
    def selfType: Elem[T @uncheckedVariance]
    lazy val self: Rep[T] = reifyObject(this)

    override def equals(other: Any) = other match {
      // check that nodes correspond to same operation, have the same type, and the same arguments
      // alternative would be to include Elem fields into case class
      case other: Base#Def[_] =>
        getClass == other.getClass && productArity == other.productArity && {
          val len = productArity
          var i = 0
          var result = true
          while (result && i < len) {
            result = Objects.deepEquals(productElement(i), other.productElement(i))
            i += 1
          }
          result
        } && selfType.name == other.selfType.name
      case _ => false
    }

    override lazy val hashCode = {
      val len = productArity
      var i = 0
      var result = 1
      while (i < len) {
        val element = productElement(i)
        val elementHashCode = element match {
          case null => 0
          case arr: Array[Object] => Arrays.deepHashCode(arr)
          case arr: Array[Int] => Arrays.hashCode(arr)
          case arr: Array[Long] => Arrays.hashCode(arr)
          case arr: Array[Float] => Arrays.hashCode(arr)
          case arr: Array[Double] => Arrays.hashCode(arr)
          case arr: Array[Boolean] => Arrays.hashCode(arr)
          case arr: Array[Byte] => Arrays.hashCode(arr)
          case arr: Array[Short] => Arrays.hashCode(arr)
          case arr: Array[Char] => Arrays.hashCode(arr)
          case _ => element.hashCode
        }
        result = 41 * result + elementHashCode
        i += 1
      }
      result
    }

    override def toString = {
      val sb = new StringBuilder
      sb.append(productPrefix)
      sb.append("(")
      val iterator = productIterator
      if (iterator.hasNext) {
        append(sb, iterator.next)
      }
      while (iterator.hasNext) {
        sb.append(", ")
        append(sb, iterator.next)
      }
      sb.append(")")
      sb.toString
    }

    private final def append(sb: StringBuilder, x: Any): Unit = {
      x match {
        case arr: Array[_] =>
          sb.append("Array(")
          if (arr.length > 0) {
            append(sb, arr(0))
            var i = 1
            while (i < arr.length) {
              sb.append(", ")
              append(sb, arr(i))
              i += 1
            }
          }
          sb.append(")")
        case s: String =>
          sb.append("\"")
          sb.append(s)
          sb.append("\"")
        case _ => sb.append(x)
      }
    }
  }

  object Def {
    def unapply[T](e: Rep[T]): Option[Def[T]] = def_unapply(e)
  }
  object && {
    def unapply[T](x: T): Option[(T,T)] = Some((x, x))
  }

  def def_unapply[T](e: Rep[T]): Option[Def[T]]

  abstract class CompanionDef[T] extends Def[T] {
    override def productArity = 0
    override def productElement(n: Int) = !!!(s"productElement($n) called, but productArity = 0", self)
    override def canEqual(other: Any) = other.isInstanceOf[CompanionDef[_]]
  }

  // this is a bit hackish. Better would be to make Elem part of Rep in sequential context
  implicit class RepDef[T <: Def[_]](x: Rep[T]) {
    def selfType1: Elem[T] = repDef_getElem(x)
  }
  def repDef_getElem[T <: Def[_]](x: Rep[T]): Elem[T]
  def rep_getElem[T](x: Rep[T]): Elem[T]

  def reifyObject[A](d: Def[A]): Rep[A]

  val cacheElems = true
  val cachePairs = true
}

object Base {
  val config = {
    val prop = new Properties
    try {
      val propertiesFileName = System.getProperty("scalan.properties.file", "scalan.properties")
      val propertiesStreams = util.FileUtil.getResources(propertiesFileName)
      try {
        propertiesStreams.foreach(prop.load)

        val pathToAdd = prop.getProperty("runtime.target")
        if(pathToAdd != null) {
          val fullPathToAdd = pathToAdd.charAt(0) match {
            case '/' => pathToAdd
            case _ => System.getProperty("user.dir") + "/" + pathToAdd
          }

          // create the dir if it not exists, and check access to one.
          val touchFile = scalan.util.FileUtil.file(fullPathToAdd, "touch.tmp")
          touchFile.getParentFile.mkdirs()
          touchFile.createNewFile()
          touchFile.delete()

          val newPath = fullPathToAdd + java.io.File.pathSeparator + System.getProperty("java.library.path")
                        //System.getProperty("java.library.path") + java.io.File.pathSeparator + fullPathToAdd
          System.setProperty( "java.library.path", newPath)

          // hack from http://nicklothian.com/blog/2008/11/19/modify-javalibrarypath-at-runtime/ for reload java.library.path
          val fieldSysPath = classOf[ClassLoader].getDeclaredField( "sys_paths" );
          fieldSysPath.setAccessible( true );
          fieldSysPath.set( null, null );

          prop.setProperty( "java.library.path", newPath)
          prop.setProperty("runtime.target", fullPathToAdd)

        }

      } finally {
        propertiesStreams.foreach(_.close())
      }
    } catch {
      case e: Throwable => print("WARNING: config not readed: " + e.toString+"\n")
    }

    // propSum filled from system, then from user properties for allow user properties to override system properties
    // separate value prop important for correct replace "java.library.path"
    val propSum = new Properties
    propSum.putAll(System.getProperties)
    propSum.putAll(prop)
    propSum
  }
}
