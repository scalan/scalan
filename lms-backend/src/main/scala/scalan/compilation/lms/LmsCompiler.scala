package scalan
package compilation
package lms

trait LmsCompiler extends Compiler { self: ScalanCtxExp =>

  def makeBridge[A, B]: LmsBridge[A, B]

  def createManifest[T]: PartialFunction[Elem[T], Manifest[_]] = {
    // Doesn't work for some reason, produces int instead of Int
    //    implicit val typeTag = eA.tag
    //    implicit val classTag = eA.classTag
    //    manifest[T]
    case UnitElement => Manifest.Unit
    case BoolElement => Manifest.Boolean
    case ByteElement => Manifest.Byte
    case ShortElement => Manifest.Short
    case IntElement => Manifest.Int
    case CharElement => Manifest.Char
    case LongElement => Manifest.Long
    case FloatElement => Manifest.Float
    case DoubleElement => Manifest.Double
    case StringElement => manifest[String]
    case PairElem(eFst, eSnd) =>
      Manifest.classType(classOf[(_, _)], createManifest(eFst), createManifest(eSnd))
    case SumElem(eLeft, eRight) =>
      Manifest.classType(classOf[Either[_, _]], createManifest(eLeft), createManifest(eRight))
    case el: FuncElem[_, _] =>
      Manifest.classType(classOf[_ => _], createManifest(el.eDom), createManifest(el.eRange))
    case el: ArrayElem[_] =>
      Manifest.arrayType(createManifest(el.eItem))
    case el: ArrayBufferElem[_] =>
      Manifest.classType(classOf[scala.collection.mutable.ArrayBuilder[_]], createManifest(el.eItem))
    case el: ListElem[_] ⇒
      Manifest.classType(classOf[List[_]], createManifest(el.eItem))
    case el: MMapElem[_,_] =>
      Manifest.classType(classOf[java.util.HashMap[_,_]], createManifest(el.eKey), createManifest(el.eValue))
    case el: BaseElemEx[_, _] => {
      val c = el.tag.mirror.runtimeClass(el.tag.tpe)
      import scala.reflect.runtime.universe._
      val targs = el.tag.tpe match { case TypeRef(_, _, args) => args }
      targs.length match {
        case 0 => Manifest.classType(c)
        case 1 => Manifest.classType(c, LmsType.wildCard)
        case n => Manifest.classType(c, LmsType.wildCard, scala.List.fill(n - 1)(LmsType.wildCard): _*)
      }
    }
    case el => ???(s"Don't know how to create manifest for $el")
  }
}
