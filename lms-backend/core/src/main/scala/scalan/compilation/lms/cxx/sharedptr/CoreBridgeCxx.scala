package scalan.compilation.lms.cxx.sharedptr

import scalan.compilation.language.Cxx
import scalan.compilation.language.Cxx.{CxxMethod, CxxType, CxxLibrary}
import scalan.compilation.lms.{ObjectOrientedBridge, CoreBridge}

trait CoreBridgeCxx extends CoreBridge with ObjectOrientedBridge[CxxLibrary, CxxType, CxxMethod] {
  val language = Cxx

  override def staticReceiverString(library: CxxLibrary, tpe: CxxType): String =
    library.namespace.fold("")(_ + "::") + tpe.mappedName
}
