package scalan.compilation.lms

import scalan.{CommunityMethodMappingDSL, ScalanCommunityDslExp}

trait CommunityBridge extends CoreBridge { self: ScalanCommunityDslExp with CommunityMethodMappingDSL =>

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

    case ArrayBinarySearch(i, xs, o) =>
      xs.elem match {
        case el: ArrayElem[_] =>
          createManifest(el.eItem) match {
            case (mA: Manifest[a]) =>
              val idxs = m.symMirror[Array[Int]](xs)
              val index = m.symMirror[Int](i)
              val exp = lms.array_binarySearch[a](index, idxs)(mA)
              m.addSym(sym, exp)
          }
      }

    case Reflect(PrintlnE(s), _, _) =>
      val s1 = m.symMirror[String](s)
      val exp = lms.println(s1)
      m.addSym(sym, exp)

    case Reflect(ReadLineE(), _, _) =>
      val exp = lms.readline
      m.addSym(sym, exp)

    case Reify(x, u, es) => m
//    case Reify(x, u, es) =>
//      createManifest(x.elem) match {
//        case (mA: Manifest[a]) =>
//          val x1 = m.symMirror[a](x)
//          val exp = lms.reify(x1, m.summaryMirror(u), es.map(e => m.symMirror(e)))(mA)
//          m.addSym(sym, exp)
//      }
    case _ => super.transformDef(m, g, sym, d)
  }
}
