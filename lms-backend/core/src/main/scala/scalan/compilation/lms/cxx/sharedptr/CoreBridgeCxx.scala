package scalan.compilation.lms.cxx.sharedptr

import scalan.compilation.language.Cxx
import scalan.compilation.lms.{ObjectOrientedBridge, CoreBridge}

trait CoreBridgeCxx extends CoreBridge with ObjectOrientedBridge {
  val language = Cxx

  override def staticReceiverString(library: Cxx.Library, tpe: Cxx.TypeT): String =
    library.namespace.fold("")(_ + "::") + tpe.mappedName
}
