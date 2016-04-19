package scalan.compilation.lms

import scalan.compilation.language.Scala

trait CoreBridgeScala extends CoreBridge with ObjectOrientedBridge {
  val language = Scala

  override def staticReceiverString(library: Scala.Library, tpe: Scala.TypeT): String =
    library.packageName.fold("")(_ + ".") + tpe.mappedName
}
