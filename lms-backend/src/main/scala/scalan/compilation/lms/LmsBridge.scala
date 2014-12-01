package scalan.compilation.lms

import scalan.community.ScalanCommunityExp
import scalan.linalgebra.VectorsDslExp

abstract class LmsBridge[A,B] {
  val scalan: ScalanCommunityExp with LmsCompiler with VectorsDslExp // TODO remove this!

  def outerApply[Ctx <: scalan.Transformer](lFunc: LmsFunction[A,B])(in: lFunc.Exp[A], g: scalan.ProgramGraph[Ctx]): lFunc.Exp[B] = {

    import scala.collection.Map

    val emptySymMirror = Map.empty[scalan.Exp[_], lFunc.Exp[X] forSome {type X}]
    val emptyFuncMirror = Map.empty[scalan.Exp[_], ( lFunc.Exp[X] => lFunc.Exp[Y]) forSome {type X; type Y}]

    val definitions = g.schedule

    type SymMirror = Map[scalan.Exp[_], lFunc.Exp[A] forSome {type A}]

    type FuncMirror = Map[scalan.Exp[_],( lFunc.Exp[A] => lFunc.Exp[B]) forSome { type A; type B } ]

    def mirrorLambdaToLmsFunc[I, R](lam:scalan.Lambda[I,R],
                                    symMirror: SymMirror,
                                    funcMirror: FuncMirror) : (lFunc.Exp[I] => lFunc.Exp[R]) = {
      val lamX = lam.x
      val f = { x: lFunc.Exp[I] =>
        val sched = lam.scheduleSingleLevel
        val (lamExps, _, _) = mirrorDefs(lam, sched, symMirror + ((lamX, x)), funcMirror )
        val res = lamExps.lastOption.getOrElse(x)
        res.asInstanceOf[lFunc.Exp[R]]
      }
      f
    }

    /* Mirror block */
    def mirrorBlockToLms[R](block:scalan.DefBlock,
                            symMirror: SymMirror,
                            funcMirror: FuncMirror,
                            dflt: scalan.Rep[_]) = {
      def f() : lFunc.Exp[R] = {
        val sched = block.scheduleSingleLevel
        val (blockExps, _, _) = mirrorDefs(block, sched, symMirror, funcMirror )
        val res = blockExps.lastOption.getOrElse(symMirror(dflt))
         res.asInstanceOf[lFunc.Exp[R]]
      }
      f
    }


    def mirrorDefs(fromGraph: scalan.AstGraph, defs: Seq[scalan.TableEntry[_]], symMirror: SymMirror, funcMirror: FuncMirror):
    (Seq[lFunc.Exp[_]], SymMirror, FuncMirror) =
    {
      val (lmsExps, finalSymMirr, finalFuncMirr) = defs.foldLeft(List.empty[lFunc.Exp[_]], symMirror, funcMirror) {
        case ((exps, symMirr, funcMirr),tp) => {
          val s = tp.sym
          tp.rhs match {
            case lam:scalan.Lambda[a, b] => {
              val f = mirrorLambdaToLmsFunc[a,b](lam, symMirr, funcMirr)
              (exps, symMirr, funcMirr + ((s, f)) )
            }
            case c@scalan.Const(_) => {
              val exp = lFunc.unitD(c.x)
              (exps ++ List(exp), symMirr + ((s, exp)), funcMirr )
            }
            case scalan.First(tuple) => {
              tuple.elem match {
                case pe: scalan.PairElem[_, _] =>
                  (scalan.createManifest(pe.eFst), scalan.createManifest(pe.eSnd))  match {
                    case (mA:Manifest[a], mB: Manifest[b]) =>
                      val tup = symMirr(tuple).asInstanceOf[lFunc.Exp[(a,b)]]
                      val exp = lFunc.first[a,b](tup)(mA, mB)
                      (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
                  }
              }
            }
            case scalan.Second(tuple) => {
              tuple.elem match {
                case pe: scalan.PairElem[_, _] =>
                  (scalan.createManifest(pe.eFst), scalan.createManifest(pe.eSnd))  match {
                    case (mA:Manifest[a], mB: Manifest[b]) =>
                      val tup = symMirr(tuple).asInstanceOf[lFunc.Exp[(a,b)]]
                      val exp = lFunc.second[a,b](tup)(mA, mB)
                      (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
                  }
              }
            }
            case scalan.ApplyBinOp(op, arg1, arg2) => {
              scalan.createManifest(arg1.elem) match {
                case (mA:Manifest[a]) =>
                  val arg1_ = symMirr(arg1).asInstanceOf[lFunc.Exp[a]]
                  val arg2_ = symMirr(arg2).asInstanceOf[lFunc.Exp[a]]
                  val exp = op.asInstanceOf[scalan.BinOp[a, _]] match {
                    case scalan.NumericTimes(n) =>
                      lFunc.opMult(arg1_, arg2_)(n.asInstanceOf[Numeric[a]], mA)
                    case scalan.NumericPlus(n) =>
                      lFunc.opPlus(arg1_, arg2_)(n.asInstanceOf[Numeric[a]], mA)
                    case scalan.IntegralDivide(n) =>
                      lFunc.opDiv(arg1_, arg2_)(n.asInstanceOf[Numeric[a]], mA)
                    case scalan.IntegralMod(n) =>
                      if (mA == Manifest.Int)
                        lFunc.opMod(arg1_.asInstanceOf[lFunc.Exp[Int]], arg2_.asInstanceOf[lFunc.Exp[Int]])
                      else
                        throw new IllegalStateException(s"LMS only supports mod operation for Int, got $mA instead")
                    case scalan.FractionalDivide(n) =>
                      lFunc.opDiv(arg1_, arg2_)(n.asInstanceOf[Numeric[a]], mA)
                    case scalan.Equals() =>
                      lFunc.opEq[a](arg1_, arg2_)(mA)
                  }
                  (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
              }
            }

            case i@scalan.IfThenElse(cond, iftrue, iffalse) => {
              scalan.createManifest(i.selfType) match {
                case (mA: Manifest[a]) => {
                  val cond_ = symMirr(cond).asInstanceOf[lFunc.Exp[Boolean]]

                  fromGraph.branches.ifBranches.get(s) match {
                    case Some(b) => {
                      def thenBody = mirrorBlockToLms(b.thenBody, symMirr, funcMirr, iftrue)
                      def elseBody = mirrorBlockToLms(b.elseBody, symMirr, funcMirr, iffalse)
                      val exp = lFunc.ifThenElse(cond_, thenBody , elseBody)(mA)
                      (exps ++ List(exp), symMirr + ((s, exp)), funcMirr)
                    }
                    case _ => {
                      val then_ = symMirr(iftrue).asInstanceOf[lFunc.Exp[a]]
                      val else_ = symMirr(iffalse).asInstanceOf[lFunc.Exp[a]]
                      val exp = lFunc.ifThenElse(cond_, then_ , else_)(mA)
                      (exps ++ List(exp), symMirr + ((s, exp)), funcMirr)
                    }
                  }
                }
              }
            }

            case apply@scalan.ArrayApply(xs, ind) => {
              scalan.createManifest(apply.selfType) match {
                case (mA:Manifest[a]) =>
                  val xs_ = symMirr(xs).asInstanceOf[lFunc.Exp[Array[a]]]
                  val ind_ = symMirr(ind).asInstanceOf[lFunc.Exp[Int]]
                  val exp = lFunc.arrayGet[a](xs_, ind_)(mA)
                  (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
              }
            }
            case scalan.ArrayApplyMany(xs, idxs) => {
              (xs.elem) match {
                case (el: scalan.ArrayElem[_]) =>
                  scalan.createManifest(el.eItem) match {
                    case (mA:Manifest[a]) =>
                      val xs_ = symMirr(xs).asInstanceOf[lFunc.Exp[Array[a]]]
                      val idxs_ = symMirr(idxs).asInstanceOf[lFunc.Exp[Array[Int]]]
                      val exp = lFunc.arrayGather[a](xs_, idxs_)(mA)
                      (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
                  }
              }
            }
            case scalan.ArrayLength(xs) => {
              scalan.createManifest(xs.elem) match {
                case (mA:Manifest[a]) =>
                  val xs_ = symMirr(xs).asInstanceOf[lFunc.Exp[Array[a]]]
                  val exp = lFunc.arrayLength[a](xs_)(mA)
                  (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
              }
            }
            case scalan.ArrayRangeFrom0(n) => {
              val n_ = symMirr(n).asInstanceOf[lFunc.Exp[Int]]
              val exp = lFunc.indexRangeD(n_)
              (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
            }
            case scalan.ArrayZip(arg1, arg2) => {
              (arg1.elem,arg2.elem) match {
                case (el1: scalan.ArrayElem[_], el2: scalan.ArrayElem[_]) =>
                  (scalan.createManifest(el1.eItem), scalan.createManifest(el2.eItem)) match {
                    case (mA:Manifest[a], mB:Manifest[b]) =>
                      val arg1_ = symMirr(arg1).asInstanceOf[lFunc.Exp[Array[a]]]
                      val arg2_ = symMirr(arg2).asInstanceOf[lFunc.Exp[Array[b]]]
                      val exp = lFunc.opZip[a,b](arg1_, arg2_)(mA, mB)
                      (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
                  }
              }
            }
            case map@scalan.ArrayMap(source, lambdaSym@scalan.Def(lam:scalan.Lambda[_,_]) ) => {
              (source.elem,map.selfType) match {
                case (el: scalan.ArrayElem[_], el1: scalan.ArrayElem[_]) =>
                  (scalan.createManifest(el.eItem), scalan.createManifest(el1.eItem)) match {
                    case (mA:Manifest[a], mB: Manifest[b]) =>
                      val f = mirrorLambdaToLmsFunc[a,b](lam.asInstanceOf[scalan.Lambda[a, b]], symMirr, funcMirr)//(mA, mB)
                    val lmsSource = symMirr(source).asInstanceOf[lFunc.Exp[Array[a]]]
                      val exp = lFunc.mapArray[a,b](lmsSource, f)(mA, mB)
                      (exps ++ List(exp), symMirr + ((s,exp)), funcMirr + ((lambdaSym,f)))
                  }
              }
            }
            case filter@scalan.ArrayFilter(source, lambdaSym@scalan.Def(lam:scalan.Lambda[_,_]) ) => {
              (filter.selfType) match {
                case (el: scalan.ArrayElem[_]) =>
                  (scalan.createManifest(el.eItem)) match {
                    case (mA:Manifest[a]) =>
                      val f = mirrorLambdaToLmsFunc[a,Boolean](lam.asInstanceOf[scalan.Lambda[a, Boolean]], symMirr, funcMirr)//(mA, mB)
                    val lmsSource = symMirr(source).asInstanceOf[lFunc.Exp[Array[a]]]
                      val exp = lFunc.filterArray[a](lmsSource, f)(mA)
                      (exps ++ List(exp), symMirr + ((s,exp)), funcMirr + ((lambdaSym,f)))
                  }
              }
            }
            /* This is reduce */
            case scalan.ArrayReduce(source, monoid) => {
              (monoid, source.elem) match {
                case (monoid, el: scalan.ArrayElem[_]) if monoid.opName.equals("+") => {
                  scalan.createManifest(el.eItem) match {
                    case (mA: Manifest[a]) =>
                      val lmsSource = symMirr(source).asInstanceOf[lFunc.Exp[Array[a]]]
                      val exp = lFunc.reduce[a](lmsSource)(mA)
                      (exps ++ List(exp), symMirr + ((s, exp)), funcMirr)
                  }
                }
                case _ => scalan.!!!("ScalanLMSBridge: Unfortunately, only Plus monoid is supported by lms")
              }
            }
            case scalan.ArrayStride(xs, start, length, stride) =>
              xs.elem match {
                case el: scalan.ArrayElem[a] =>
                  val mA = scalan.createManifest(el.eItem).asInstanceOf[Manifest[a]]
                  val lmsXs = symMirr(xs).asInstanceOf[lFunc.Exp[Array[a]]]
                  val lmsStart = symMirr(start).asInstanceOf[lFunc.Exp[Int]]
                  val lmsLength = symMirr(length).asInstanceOf[lFunc.Exp[Int]]
                  val lmsStride = symMirr(stride).asInstanceOf[lFunc.Exp[Int]]
                  val exp = lFunc.strideArray(lmsXs, lmsStart, lmsLength, lmsStride)(mA)
                  (exps ++ List(exp), symMirr + ((s, exp)), funcMirr)
              }
            case scalan.DotSparse(i1, v1, i2, v2) => {
              (v1.elem) match {
                case (el: scalan.ArrayElem[_]) =>
                  (scalan.createManifest(el.eItem)) match {
                    case (mA:Manifest[a]) =>
                      val i1_ = symMirr(i1).asInstanceOf[lFunc.Exp[Array[Int]]]
                      val i2_ = symMirr(i2).asInstanceOf[lFunc.Exp[Array[Int]]]
                      val v1_ = symMirr(v1).asInstanceOf[lFunc.Exp[Array[a]]]
                      val v2_ = symMirr(v2).asInstanceOf[lFunc.Exp[Array[a]]]
                      val exp = lFunc.opDotProductSV[a](i1_, v1_, i2_, v2_)(mA)
                      (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
                  }
              }
            }

            case _ => scalan.!!!(s"ScalanLMSBridge: Don't know how to mirror symbol ${s.toStringWithDefinition}")
          }
        }
      }
      (lmsExps, finalSymMirr, finalFuncMirr)
    }

    val (lmsExps, finalSymMirror, finalFuncMirror) = mirrorDefs(g, definitions, emptySymMirror, emptyFuncMirror)
    val res = finalFuncMirror(g.roots.last).asInstanceOf[lFunc.Exp[A] => lFunc.Exp[B]](in)

    res
  }

  class LmsFacade[Ctx <: scalan.Transformer](g: scalan.ProgramGraph[Ctx]) extends LmsFunction[A, B] {
    def apply(in: this.Exp[A]): this.Exp[B] = {
      outerApply(this)(in, g)
    }
  }

  def getFacade[Ctx <: scalan.Transformer](g: scalan.ProgramGraph[Ctx]) = new LmsFacade[Ctx](g)
}
