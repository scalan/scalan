package scalan.compilation.lms

import scalan.{CommunityMethodMappingDSL, ScalanCommunityDslExp}

trait CommunityBridge extends CoreBridge with CommunityMethodMappingDSL {
  override val scalan: ScalanCommunityDslExp
  import scalan._

  val lms: CommunityLmsBackendBase

  override def transformDef[T](m: LmsMirror, g: AstGraph, sym: Exp[T], d: Def[T]) = d match {
    case DotSparse(i1, v1, i2, v2) =>
      v1.elem match {
        case el: ArrayElem[_] =>
          createManifest(el.eItem) match {
            case (mA: Manifest[a]) =>
              val i1_ = m.symMirror[Array[Int]](i1)
              val i2_ = m.symMirror[Array[Int]](i2)
              val v1_ = m.symMirror[Array[a]](v1)
              val v2_ = m.symMirror[Array[a]](v2)
              val exp = lms.array_dotProductSparse[a](i1_, v1_, i2_, v2_)(mA)

              m.addSym(sym, exp)
          }
      }
    case _ => super.transformDef(m, g, sym, d)
  }
}
