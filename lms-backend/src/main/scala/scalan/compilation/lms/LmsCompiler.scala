package scalan
package compilation
package lms

import scalan.community.ScalanCommunityExp

trait LmsCompiler extends Compiler { self: ScalanCommunityExp =>

  case class Config(extraCompilerOptions: Seq[String])

  implicit val defaultConfig = Config(Seq.empty)

  def makeBridge[A, B]: LmsBridge[A, B]

  def graphPasses(config: Config) = Seq(AllUnpackEnabler, AllInvokeEnabler)

  def createManifest[T]: PartialFunction[Elem[T],Manifest[_]] = {
    // Doesn't work for some reason, produces int instead of Int
    //    implicit val typeTag = eA.tag
    //    implicit val classTag = eA.classTag
    //    manifest[T]
    case UnitElement => Manifest.Unit
    case BoolElement => Manifest.Boolean
    case ByteElement => Manifest.Byte
    case ShortElement => Manifest.Short
    case IntElement => Manifest.Int
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
    case el => ???(s"Don't know how to create manifest for $el")
  }
}
