package scalan.compilation.lms

import scalan.compilation.language.MethodMappingDSL
import scalan.compilation.lms.common.{PointerLmsOps, PointerLmsOpsExp}
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

  override def transformDef[T](m: LmsMirror, g: AstGraph, sym: Exp[T], d: Def[T]) = {
    def itemElem(sourceElem: Elem[_]) = sourceElem match {
      case ScalaArrayElem(eItem) => eItem
      case elem: ScalarElem[_] => elem.eItem
      case elem: PointerElem[_] => elem.eItem
      case _ => !!!(s"cannot get itemElement for $sourceElem")
    }
    def createItemManifest(sourceElem: Elem[_]) = createManifest(itemElem(sourceElem))

    d match {
      case CreateScalar(source) =>
        createManifest(source.elem) match {
          case mA: Manifest[a_t] =>
            val _source = m.symMirror[a_t](source)
            val exp = lms.createScalar[a_t](_source)(mA)
            m.addSym(sym, exp)
        }

      case ScalarPtr(xScalar) =>
        createItemManifest(xScalar.elem) match {
          case mA: Manifest[a_t] =>
            val _xScalar = m.symMirror[lms.Scalar[a_t]](xScalar)
            val exp = lms.scalarPtr[a_t](_xScalar)(mA)
            m.addSym(sym, exp)
        }

      case ArrayPtr(xs) =>
        createItemManifest(xs.elem) match {
          case mA: Manifest[a_t] =>
            val _xs = m.symMirror[Array[a_t]](xs)
            val exp = lms.arrayPtr[a_t](_xs)(mA)
            m.addSym(sym, exp)
        }

      case np@NullPtr() =>
        createItemManifest(np.elem) match {
          case mA: Manifest[a_t] =>
            val exp = lms.nullPtr[a_t](mA)
            m.addSym(sym, exp)
        }

      case _ => super.transformDef(m, g, sym, d)
    }
  }
}
