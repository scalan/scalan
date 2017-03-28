package scalan

import scalan.meta.ScalanAst.SModuleDef
import scalan.util.Serialization

abstract class ModuleInfo {
  val dump: String
  lazy val moduleDef = Serialization.load[SModuleDef](dump)
}
