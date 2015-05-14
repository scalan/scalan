package scalan.compilation.lms

import scalan.compilation.language.MethodMappingDSL
import scalan.compilation.lms.common.{PointerLmsOps, PointerLmsOpsExp}
import scalan.primitives.AbstractStringsDslExp
import scalan.{ScalanCtxExp, PointerOpsExp}

trait PointerBridge extends CoreBridge { self: ScalanCtxExp with MethodMappingDSL with PointerOpsExp =>

  val lms: CoreLmsBackendBase with PointerLmsOpsExp

  override def createManifest[T]: PartialFunction[Elem[T], Manifest[_]] = {
    case el: ScalarElem[_] =>
      Manifest.classType(classOf[PointerLmsOps#Scalar[_]], createManifest(el.eItem))
    case el: PointerElem[_] =>
      Manifest.classType(classOf[PointerLmsOps#Pointer[_]], createManifest(el.eItem))
    case el =>
      super.createManifest(el)
  }

  override def transformDef[T](m: LmsMirror, g: AstGraph, sym: Exp[T], d: Def[T]) = d match {

    case CreateScalar(source) =>
      createManifest(source.elem) match {
        case mA: Manifest[a_t] =>
          val _source = m.symMirror[a_t](source)
          val exp = lms.createScalar[a_t](_source)(mA)
          m.addSym(sym, exp)
      }

    case _ => super.transformDef(m, g, sym, d)
  }
}
