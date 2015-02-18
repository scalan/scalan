package scalan.compilation.lms.scalac

object LmsType {

  class WildCard

  val wildCard : Manifest[WildCard] = Manifest.classType(classOf[WildCard])
}
