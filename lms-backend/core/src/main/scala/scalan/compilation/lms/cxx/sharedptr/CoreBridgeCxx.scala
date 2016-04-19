package scalan.compilation.lms.cxx.sharedptr

import scalan.compilation.language.Cxx
import scalan.compilation.lms.{ObjectOrientedBridge, CoreBridge}

trait CoreBridgeCxx extends CoreBridge with ObjectOrientedBridge {
  val language = Cxx

  override def staticReceiverString(typeMapping: language.TypeMapping): String =
    typeMapping.library.namespace.fold("")(_ + "::") + typeMapping.tpe.mappedName
}
