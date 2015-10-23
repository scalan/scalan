package scalan

import scalan.meta.ScalanAst.SEntityModuleDef
import scalan.util.Serialization

abstract class ModuleInfo {
  val dump: String
  lazy val moduleDef = Serialization.load[SEntityModuleDef](dump)
}
