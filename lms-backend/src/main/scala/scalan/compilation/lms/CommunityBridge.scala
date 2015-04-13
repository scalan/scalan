package scalan.compilation.lms

import scalan.CommunityMethodMappingDSL
import scalan.ScalanCommunityDslExp

trait CommunityBridge extends CoreBridge { self: ScalanCommunityDslExp with CommunityMethodMappingDSL =>

  val lms: CommunityLmsBackendBase

  abstract override def defTransformer[T](m: LmsMirror, g: AstGraph, e: TableEntry[T]) =
    linalgDefTransformer(m, g, e) orElse super.defTransformer(m, g, e)

  def linalgDefTransformer[T](m: LmsMirror, g: AstGraph, e: TableEntry[T]): DefTransformer = {
    val sym = e.sym
    val tt: DefTransformer = {
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
    }
    tt
  }
}
