package scalan.compilation.lms

import scalan.compilation.language.Scala

trait CoreBridgeScala extends CoreBridge with ObjectOrientedBridge {
  val language = Scala

  override def staticReceiverString(typeMapping: language.TypeMapping): String =
    typeMapping.library.packageName.fold("")(_ + ".") + typeMapping.tpe.mappedName
}
