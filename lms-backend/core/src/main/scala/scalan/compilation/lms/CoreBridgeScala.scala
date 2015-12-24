package scalan.compilation.lms

import scalan.compilation.language.SCALA
import scalan.compilation.language.ScalaMapping.{ScalaMethod, ScalaType, ScalaLibrary}

trait CoreBridgeScala extends CoreBridge with ObjectOrientedBridge[ScalaLibrary, ScalaType, ScalaMethod] {
  val languageId = SCALA

  override def staticReceiverString(library: ScalaLibrary, tpe: ScalaType): String =
    library.packageName.fold("")(_ + ".") + tpe.mappedName
}
