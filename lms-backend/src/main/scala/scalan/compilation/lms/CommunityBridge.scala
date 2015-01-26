package scalan.compilation.lms

import scalan.community.ScalanCommunityExp

trait CommunityBridge[A, B] extends LmsBridge[A, B] { self: LmsBridge[A, B] =>

  // `LmsCompiler` mixed just to provide `createManifest` function
  val scalan: ScalanCommunityExp with LmsCompiler

  override def defTransformer[T](m: Mirror, g: scalan.AstGraph, e: scalan.TableEntry[T]) =
    communityDefTransformer(m, g, e) orElse super.defTransformer(m, g, e)

  def communityDefTransformer[T](m: Mirror, fromGraph: scalan.AstGraph, tp: scalan.TableEntry[T]): DefTransformer = {
    val (exps, symMirr, funcMirr) = m
    val sym = tp.sym

    val tt: DefTransformer = {
      case lam: scalan.Lambda[a, b] =>
        val f = mirrorLambdaToLmsFunc[a, b](m)(lam)
        (exps, symMirr, funcMirr + ((sym, f)))

      case c @ scalan.Const(_) =>
        val exp = lms.unitD(c.x)
        (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)

      case d @ scalan.Left(l) =>
        import scalan.SumElemExtensions
        (scalan.createManifest(d.selfType.eLeft), scalan.createManifest(d.selfType.eRight)) match {
          case (mA: Manifest[a], mB: Manifest[b]) =>
            val left = symMirr(l).asInstanceOf[lms.Exp[a]]
            val exp = lms.sumLeft[a, b](left)(mA, mB)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case d @ scalan.Right(r) =>
        import scalan.SumElemExtensions
        (scalan.createManifest(d.selfType.eLeft), scalan.createManifest(d.selfType.eRight)) match {
          case (mA: Manifest[a], mB: Manifest[b]) =>
            val right = symMirr(r).asInstanceOf[lms.Exp[b]]
            val exp = lms.sumRight[a, b](right)(mA, mB)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case scalan.Tup(fst, snd) =>
        (scalan.createManifest(fst.elem), scalan.createManifest(snd.elem)) match {
          case (mA: Manifest[a], mB: Manifest[b]) =>
            val first = symMirr(fst).asInstanceOf[lms.Exp[a]]
            val second = symMirr(snd).asInstanceOf[lms.Exp[b]]
            val exp = lms.tuple[a, b](first, second)(mA, mB)
            (exps :+ exp, symMirr + ((sym, exp)), funcMirr)
        }

      case scalan.First(tuple) =>
        tuple.elem match {
          case pe: scalan.PairElem[_, _] =>
            (scalan.createManifest(pe.eFst), scalan.createManifest(pe.eSnd)) match {
              case (mA: Manifest[a], mB: Manifest[b]) =>
                val tup = symMirr(tuple).asInstanceOf[lms.Exp[(a, b)]]
                val exp = lms.first[a, b](tup)(mA, mB)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }

      case scalan.Second(tuple) =>
        tuple.elem match {
          case pe: scalan.PairElem[_, _] =>
            (scalan.createManifest(pe.eFst), scalan.createManifest(pe.eSnd)) match {
              case (mA: Manifest[a], mB: Manifest[b]) =>
                val tup = symMirr(tuple).asInstanceOf[lms.Exp[(a, b)]]
                val exp = lms.second[a, b](tup)(mA, mB)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }

      case scalan.ApplyBinOp(op, arg1, arg2) =>
        scalan.createManifest(arg1.elem) match {
          case (mA: Manifest[a]) =>
            val arg1_ = symMirr(arg1).asInstanceOf[lms.Exp[a]]
            val arg2_ = symMirr(arg2).asInstanceOf[lms.Exp[a]]
            val exp = op.asInstanceOf[scalan.BinOp[a, _]] match {
              case scalan.NumericTimes(n) =>
                lms.opMult(arg1_, arg2_)(n.asInstanceOf[Numeric[a]], mA)
              case scalan.NumericPlus(n) =>
                lms.opPlus(arg1_, arg2_)(n.asInstanceOf[Numeric[a]], mA)
              case scalan.NumericMinus(n) =>
                lms.opMinus(arg1_, arg2_)(n.asInstanceOf[Numeric[a]], mA)
              case scalan.IntegralDivide(n) =>
                lms.opDiv(arg1_, arg2_)(n.asInstanceOf[Numeric[a]], mA)
              case scalan.IntegralMod(n) =>
                if (mA == Manifest.Int)
                  lms.opMod(arg1_.asInstanceOf[lms.Exp[Int]], arg2_.asInstanceOf[lms.Exp[Int]])
                else
                  throw new IllegalStateException(s"LMS only supports mod operation for Int, got $mA instead")
              case scalan.FractionalDivide(n) =>
                lms.opDiv(arg1_, arg2_)(n.asInstanceOf[Numeric[a]], mA)
              case scalan.Equals() =>
                lms.opEq[a](arg1_, arg2_)(mA)
              case scalan.OrderingLT(ord) =>
                lms.LT[a](arg1_, arg2_)(mA, ord.asInstanceOf[Ordering[a]])
              case scalan.OrderingLTEQ(ord) =>
                lms.LTEQ[a](arg1_, arg2_)(mA, ord.asInstanceOf[Ordering[a]])
              case scalan.OrderingGT(ord) =>
                lms.GT[a](arg1_, arg2_)(mA, ord.asInstanceOf[Ordering[a]])
              case scalan.OrderingGTEQ(ord) =>
                lms.GTEQ[a](arg1_, arg2_)(mA, ord.asInstanceOf[Ordering[a]])
              case scalan.OrderingMax(ord) =>
                lms.Max[a](arg1_, arg2_)(mA, ord.asInstanceOf[Ordering[a]])
              case scalan.OrderingMin(ord) =>
                lms.Min[a](arg1_, arg2_)(mA, ord.asInstanceOf[Ordering[a]])
            }
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }

      case scalan.Semicolon(left, right) => {
        (scalan.createManifest(left.elem), scalan.createManifest(right.elem)) match {
          case (mA: Manifest[a], mB: Manifest[b]) => {
            val left_ = symMirr(left).asInstanceOf[lms.Exp[a]]
            val right_ = symMirr(right).asInstanceOf[lms.Exp[b]]
            val exp = lms.block(left_, right_)(mA, mB)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
          }
        }
      }
      case i @ scalan.IfThenElse(cond, iftrue, iffalse) =>
        scalan.createManifest(i.selfType) match {
          case (mA: Manifest[a]) =>
            val cond_ = symMirr(cond).asInstanceOf[lms.Exp[Boolean]]

            fromGraph.branches.ifBranches.get(sym) match {
              case Some(branches) =>
                def thenBody = mirrorBlockToLms(m)(branches.thenBody, iftrue)
                def elseBody = mirrorBlockToLms(m)(branches.elseBody, iffalse)
                val exp = lms.ifThenElse(cond_, thenBody, elseBody)(mA)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              case _ =>
                val then_ = symMirr(iftrue).asInstanceOf[lms.Exp[a]]
                val else_ = symMirr(iffalse).asInstanceOf[lms.Exp[a]]
                val exp = lms.ifThenElse(cond_, () => then_, () => else_)(mA)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }

      case apply @ scalan.ArrayApply(xs, ind) =>
        scalan.createManifest(apply.selfType) match {
          case (mA: Manifest[a]) =>
            val xs_ = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
            val ind_ = symMirr(ind).asInstanceOf[lms.Exp[Int]]
            val exp = lms.arrayGet[a](xs_, ind_)(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }

      case scalan.ArrayApplyMany(xs, idxs) =>
        xs.elem match {
          case el: scalan.ArrayElem[_] =>
            scalan.createManifest(el.eItem) match {
              case (mA: Manifest[a]) =>
                val xs_ = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
                val idxs_ = symMirr(idxs).asInstanceOf[lms.Exp[Array[Int]]]
                val exp = lms.arrayGather[a](xs_, idxs_)(mA)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }

      case scalan.ArrayLength(xs) =>
        scalan.createManifest(xs.elem) match {
          case mA: Manifest[a] =>
            val xs_ = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
            val exp = lms.arrayLength[a](xs_)(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }

      case scalan.ArrayRangeFrom0(n) =>
        val n_ = symMirr(n).asInstanceOf[lms.Exp[Int]]
        val exp = lms.indexRangeD(n_)
        (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)

      case scalan.ArrayZip(arg1, arg2) =>
        (arg1.elem, arg2.elem) match {
          case (el1: scalan.ArrayElem[_], el2: scalan.ArrayElem[_]) =>
            (scalan.createManifest(el1.eItem), scalan.createManifest(el2.eItem)) match {
              case (mA: Manifest[a], mB: Manifest[b]) =>
                val arg1_ = symMirr(arg1).asInstanceOf[lms.Exp[Array[a]]]
                val arg2_ = symMirr(arg2).asInstanceOf[lms.Exp[Array[b]]]
                val exp = lms.opZip[a, b](arg1_, arg2_)(mA, mB)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }

      case map @ scalan.ArrayMap(source, lambdaSym @ scalan.Def(lam: scalan.Lambda[_, _])) =>
        (source.elem, map.selfType) match {
          case (el: scalan.ArrayElem[_], el1: scalan.ArrayElem[_]) =>
            (scalan.createManifest(el.eItem), scalan.createManifest(el1.eItem)) match {
              case (mA: Manifest[a], mB: Manifest[b]) =>
                val f = mirrorLambdaToLmsFunc[a, b](m)(lam.asInstanceOf[scalan.Lambda[a, b]]) //(mA, mB)
              val lmsSource = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.mapArray[a, b](lmsSource, f)(mA, mB)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, f)))
            }
        }

      case filter @ scalan.ArrayFilter(source, lambdaSym @ scalan.Def(lam: scalan.Lambda[_, _])) =>
        filter.selfType match {
          case el: scalan.ArrayElem[_] =>
            scalan.createManifest(el.eItem) match {
              case mA: Manifest[a] =>
                val f = mirrorLambdaToLmsFunc[a, Boolean](m)(lam.asInstanceOf[scalan.Lambda[a, Boolean]]) //(mA, mB)
                val lmsSource = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.filterArray[a](lmsSource, f)(mA)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((lambdaSym, f)))
            }
        }

      /* This is reduce */
      case scalan.ArrayReduce(source, monoid) =>
        (monoid, source.elem) match {
          case (monoid, el: scalan.ArrayElem[_]) if monoid.opName.equals("+") =>
            scalan.createManifest(el.eItem) match {
              case (mA: Manifest[a]) =>
                val lmsSource = symMirr(source).asInstanceOf[lms.Exp[Array[a]]]
                val exp = lms.reduce[a](lmsSource)(mA)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
          case _ => scalan.!!!("ScalanLMSBridge: Unfortunately, only Plus monoid is supported by lms")
        }

      case scalan.ArrayStride(xs, start, length, stride) =>
        xs.elem match {
          case el: scalan.ArrayElem[a] =>
            val mA = scalan.createManifest(el.eItem).asInstanceOf[Manifest[a]]
            val lmsXs = symMirr(xs).asInstanceOf[lms.Exp[Array[a]]]
            val lmsStart = symMirr(start).asInstanceOf[lms.Exp[Int]]
            val lmsLength = symMirr(length).asInstanceOf[lms.Exp[Int]]
            val lmsStride = symMirr(stride).asInstanceOf[lms.Exp[Int]]
            val exp = lms.strideArray(lmsXs, lmsStart, lmsLength, lmsStride)(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }
    }

    tt
  }

}
