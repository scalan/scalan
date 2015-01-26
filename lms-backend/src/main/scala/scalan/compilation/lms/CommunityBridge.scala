package scalan.compilation.lms

import scalan.community.ScalanCommunityDslExp

trait CommunityBridge[A, B] extends CoreBridge[A, B] {

  // `LmsCompiler` mixed just to provide `createManifest` function
  val scalan: ScalanCommunityDslExp with LmsCompiler
  val lms: CommunityLmsBackend

  abstract override def defTransformer[T](m: Mirror, g: scalan.AstGraph, e: scalan.TableEntry[T]) =
    linalgDefTransformer(m, g, e) orElse super.defTransformer(m, g, e)

  def linalgDefTransformer[T](m: Mirror, g: scalan.AstGraph, e: scalan.TableEntry[T]): DefTransformer = {
    val (exps, symMirr, funcMirr) = m
    val sym = e.sym
    val tt: DefTransformer = {
      case scalan.DotSparse(i1, v1, i2, v2) =>
        v1.elem match {
          case el: scalan.ArrayElem[_] =>
            scalan.createManifest(el.eItem) match {
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
