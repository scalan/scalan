package scalan.compilation.lms.cxx.sharedptr

import scalan.compilation.language.CXX
import scalan.compilation.language.CxxMapping.{CxxMethod, CxxType, CxxLibrary}
import scalan.compilation.lms.{ObjectOrientedBridge, CoreBridge}

trait CoreBridgeCxx extends CoreBridge with ObjectOrientedBridge[CxxLibrary, CxxType, CxxMethod] {
  val languageId = CXX

  override def staticReceiverString(library: CxxLibrary, tpe: CxxType): String =
    library.namespace.fold("")(_ + "::") + tpe.mappedName
}
