package scalan.meta

import scalan.meta.ScalanAst.{STraitCall, SClassArg, SValDef, STpeArgs, STpeExpr, STpeFunc, SExpr, STpeArg, STpeEmpty, SModuleDef, SClassDef, STpeTuple, SMethodArg, STpeSingleton, SMethodDef, STypeApply, STpeSelectFromTT, SClassArgs, SMethodArgs, STpeTypeBounds, STpePrimitive, SBodyItem, STpeAnnotated, STpeExistential}
import PrintExtensions._

trait Emitters {
  def genClass(c: SClassDef): String
}


