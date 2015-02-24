package scalan.compilation.lms

import scalan.CommunityMethodMapping
import scalan.ScalanCommunityDslExp

trait CommunityBridge extends CoreBridge { self: ScalanCommunityDslExp with CommunityMethodMapping =>

  val lms: CommunityLmsBackendBase

  abstract override def defTransformer[T](m: LmsMirror, g: AstGraph, e: TableEntry[T]) =
    linalgDefTransformer(m, g, e) orElse super.defTransformer(m, g, e)

  def linalgDefTransformer[T](m: LmsMirror, g: AstGraph, e: TableEntry[T]): DefTransformer = {
    val (exps, symMirr, funcMirr) = m
    val sym = e.sym
    val tt: DefTransformer = {
      case DotSparse(i1, v1, i2, v2) =>
        v1.elem match {
          case el: ArrayElem[_] =>
            createManifest(el.eItem) match {
              case (mA: Manifest[a]) =>
                val i1_ = symMirr(i1).asInstanceOf[lms.Exp[Array[Int]]]
                val i2_ = symMirr(i2).asInstanceOf[lms.Exp[Array[Int]]]
                val v1_ = symMirr(v1).asInstanceOf[lms.Exp[Array[a]]]
                val v2_ = symMirr(v2).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.array_dotProductSparse[a](i1_, v1_, i2_, v2_)(mA)

                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }
    }
    tt
  }
}
