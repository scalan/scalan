package scalan.compilation.lms.scalac

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

object LmsManifestUtil {

  def toManifest(o: Any): Manifest[_] = {
    classToManifest(o.getClass)
  }

  def classToManifest(c: Class[_]): Manifest[_] = {
    val runtimeMirror = universe.runtimeMirror(c.getClassLoader)
    val aType = runtimeMirror.classSymbol(c).toType
    toManifest(aType, runtimeMirror)
  }

  def tagToManifest[T](t: WeakTypeTag[T]): Manifest[_] = toManifest(t.tpe, t.mirror)

  private def toManifest[T](t: scala.reflect.runtime.universe.Type, m: scala.reflect.runtime.universe.Mirror): Manifest[_] = {
    val args = t match {
      case api: TypeRefApi => api.args
      case _ => List.empty[Type]
    }

    simpleType.applyOrElse[Type, Manifest[_]](t, {
      case _ =>
        try {
          val c = m.runtimeClass(t)
          args.length match {
            case 0 => Manifest.classType(c)
            case 1 => Manifest.classType(c, toManifest(args(0), m))
            case n => Manifest.classType(c, toManifest(args(0), m), args.drop(1).map(toManifest(_, m)): _*)
          }
        } catch {
          case _: NoClassDefFoundError => LmsType.wildCard
        }
    })
  }

  private def simpleType: PartialFunction[scala.reflect.runtime.universe.Type, Manifest[_]] = {
    // Doesn't work for some reason, produces int instead of Int
    //    implicit val typeTag = eA.tag
    //    implicit val classTag = eA.classTag
    //    manifest[T]
    case t if t =:= typeOf[Unit] => Manifest.Unit
    case t if t =:= typeOf[Boolean] => Manifest.Boolean
    case t if t =:= typeOf[Byte] => Manifest.Byte
    case t if t =:= typeOf[Short] => Manifest.Short
    case t if t =:= typeOf[Int] => Manifest.Int
    case t if t =:= typeOf[Char] => Manifest.Char
    case t if t =:= typeOf[Long] => Manifest.Long
    case t if t =:= typeOf[Float] => Manifest.Float
    case t if t =:= typeOf[Double] => Manifest.Double
    case t if t =:= typeOf[String] => manifest[String]
  }

}
