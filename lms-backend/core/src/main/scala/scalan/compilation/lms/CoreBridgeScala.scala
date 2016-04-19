package scalan.compilation.lms

import scalan.compilation.language.Scala
import scalan.compilation.language.Scala.{ScalaMethod, ScalaType, ScalaLibrary}

trait CoreBridgeScala extends CoreBridge with ObjectOrientedBridge[ScalaLibrary, ScalaType, ScalaMethod] {
  val language = Scala

  override def staticReceiverString(library: ScalaLibrary, tpe: ScalaType): String =
    library.packageName.fold("")(_ + ".") + tpe.mappedName
}
