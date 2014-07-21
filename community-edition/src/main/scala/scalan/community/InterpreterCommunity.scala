package scalan.community

import scalan.Interpreter

trait InterpreterCommunity extends Interpreter {
  override val seq: ScalanCommunitySeq //with VectorsDslSeq
  override val staged: ScalanCommunityStaged //with VectorsDslExp

  import staged._

  override def mirrorDef(d: Def[_], s: Exp[_], exps: List[seq.Rep[_]], symMirr: SymMirror, funcMirr: FuncMirror): (List[seq.Rep[_]], SymMirror, FuncMirror) = d match {
    case apply@ArrayApply(xs, ind) => {
      apply.eT match {
        case (elem:Elem[a]) =>
          val xs_ = symMirr(xs).asInstanceOf[seq.Rep[Array[a]]]
          val ind_ = symMirr(ind).asInstanceOf[seq.Rep[Int]]
          val exp = xs_(ind_)
          (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
      }
    }
    case ArrayApplyMany(xs, idxs) => {
      createSeqElem(xs.elem) match {
        case (el: seq.ArrayElem[a]) =>
          val xs_ = symMirr(xs).asInstanceOf[seq.Rep[Array[a]]]
          val idxs_ = symMirr(idxs).asInstanceOf[seq.Rep[Array[Int]]]
          val exp = seq.array_applyMany(xs_, idxs_)
          (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
      }
    }
    case ArrayLength(xs) => {
      val xs_ = symMirr(xs).asInstanceOf[seq.Rep[Array[_]]]
      val exp = xs_.length
      (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
    }
    case ArrayRangeFrom0(n) => {
      val n_ = symMirr(n).asInstanceOf[seq.Rep[Int]]
      val exp = seq.array_rangeFrom0(n_)
      (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
    }

    case ArrayZip(arg1, arg2) => {
      (arg1.elem,arg2.elem) match {
        case (el1: ArrayElem[a], el2: ArrayElem[b]) =>
          val arg1_ = symMirr(arg1).asInstanceOf[seq.Rep[Array[a]]]
          val arg2_ = symMirr(arg2).asInstanceOf[seq.Rep[Array[b]]]
          val exp = arg1_ zip arg2_
          (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
      }
    }
    case map@ArrayMap(source, lambdaSym@Def(lam:Lambda[_,_]) ) => {
      (source.elem,map.selfType) match {
        case (el: ArrayElem[a], el1: ArrayElem[b]) =>
          val f : (a => b) = mirrorLambdaToFunc[a,b](lam.asInstanceOf[Lambda[a, b]], symMirr, funcMirr)
          val seqSource = symMirr(source).asInstanceOf[seq.Rep[Array[a]]]

          implicit val eB = createSeqElem(el1.ea);
          val exp = seqSource.map(f).toArray(eB.classTag)
          (exps ++ List(exp), symMirr + ((s,exp)), funcMirr + ((lambdaSym,f)))
      }
    }
    case filter@ArrayFilter(source, lambdaSym@Def(lam:Lambda[_,_]) ) => {
      (filter.selfType) match {
        case (el: ArrayElem[a]) =>
          val f = mirrorLambdaToFunc[a,Boolean](lam.asInstanceOf[Lambda[a, Boolean]], symMirr, funcMirr)
          val seqSource = symMirr(source).asInstanceOf[seq.Rep[Array[a]]]
          val exp = seqSource.filter(f)
          (exps ++ List(exp), symMirr + ((s,exp)), funcMirr + ((lambdaSym,f)))
      }
    }
    /* This is reduce */
    case ArrayReduce(source, monoid) => {
      (monoid.append, source.elem) match {
        case (lambdaSym@Def(lam:Lambda[_,_]), el: ArrayElem[a]) =>
          val seqSource = symMirr(source).asInstanceOf[seq.Rep[Array[a]]]
          val seqZero = symMirr(monoid.zero).asInstanceOf[seq.Rep[a]]
          val seqAppend = mirrorLambdaToFunc[(a,a),a](lam.asInstanceOf[Lambda[(a,a), a]], symMirr, funcMirr)
          implicit val eA = createSeqElem(el.ea);
          val seqMonoid = seq.RepMonoid[a](monoid.opName, seqAppend, seqZero, monoid.isInfix, monoid.isCommutative)
          val exp = seq.array_reduce[a](seqSource)(seqMonoid)
          (exps ++ List(exp), symMirr + ((s, exp)), funcMirr + ((lambdaSym, seqAppend)))
      }
    }
    case ArrayStride(xs, start, length, stride) =>
      createSeqElem(xs.elem) match {
        case el: seq.ArrayElem[a] =>
          //val mA = scalan.createManifest(el.ea).asInstanceOf[Manifest[a]]
          //implicit val eArr = createSeqElem(el);
          //implicit val eA = createSeqElem(el.ea);

          val seqXs = symMirr(xs).asInstanceOf[seq.Rep[Array[a]]]
          val seqStart = symMirr(start).asInstanceOf[seq.Rep[Int]]
          val seqLength = symMirr(length).asInstanceOf[seq.Rep[Int]]
          val seqStride = symMirr(stride).asInstanceOf[seq.Rep[Int]]
          val exp = seq.array_stride[a](seqXs, seqStart, seqLength, seqStride)
          (exps ++ List(exp), symMirr + ((s, exp)), funcMirr)
      }
    /*case ds@DotSparse(i1, v1, i2, v2) => {
      (ds.m.append, createSeqElem(v1.elem) ) match {
        case (lambdaSym@Def(lam:Lambda[_,_]), el: seq.ArrayElem[a]) =>
          //implicit val eArr = createSeqElem(el);
          val i1_ = symMirr(i1).asInstanceOf[seq.Rep[Array[Int]]]
          val i2_ = symMirr(i2).asInstanceOf[seq.Rep[Array[Int]]]
          val v1_ = symMirr(v1).asInstanceOf[seq.Rep[Array[a]]]
          val v2_ = symMirr(v2).asInstanceOf[seq.Rep[Array[a]]]

          implicit val eA = el.ea;

          val seqZero = symMirr(ds.m.zero).asInstanceOf[seq.Rep[a]]
          val seqAppend = mirrorLambdaToFunc[(a,a),a](lam.asInstanceOf[Lambda[(a,a), a]], symMirr, funcMirr)
          val seqMonoid = seq.RepMonoid[a](ds.m.opName, seqAppend, seqZero, ds.m.isInfix, ds.m.isCommutative)


          val exp = seq.dotSparse(i1_,seq.PArray(v1_), i2_, seq.PArray(v2_))(eA, ds.n, seqMonoid)
          (exps ++ List(exp), symMirr + ((s,exp)), funcMirr + ((lambdaSym, seqAppend)))
      }
    }*/

    case _ => super.mirrorDef(d, s, exps, symMirr, funcMirr)
  }

  override def createSeqElem[T](eA: Elem[T]) : seq.Elem[T] = {
    // Doesn't work for some reason, produces int instead of Int
    //    implicit val typeTag = eA.tag
    //    implicit val classTag = eA.classTag
    //    manifest[T]
    eA match {
      case el: ArrayElem[_] => {
        val elem = seq.arrayElement(createSeqElem(el.ea))
        elem.asInstanceOf[seq.Elem[T]]
      }
      case _ => super.createSeqElem(eA)
    }
  }
}
