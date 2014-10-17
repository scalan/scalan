package scalan

import compilation.GraphVizExport
import java.io.File

/**
 * Created with IntelliJ IDEA.
 * User: afilippov
 * Date: 7/3/14
 * Time: 4:07 PM
 * To change this template use File | Settings | File Templates.
 */
trait Interpreter {
  val seq: ScalanSeq //with VectorsDslSeq
  val staged: ScalanExp with GraphVizExport //with VectorsDslExp

  import staged._

  type SymMirror = Map[Exp[_], seq.Rep[A] forSome {type A}]

  type FuncMirror = Map[Exp[_],( seq.Rep[A] => seq.Rep[B]) forSome { type A; type B } ]


  def mirrorLambdaToFunc[I, R](lam:Lambda[I,R],
                               symMirror: SymMirror,
                               funcMirror: FuncMirror) : (seq.Rep[I] => seq.Rep[R]) = {
    val lamX = lam.x
    val f = { x: seq.Rep[I] =>
      val sched = lam.schedule
      val (lamExps, _, _) = mirrorDefs(sched, symMirror + ((lamX, x)), funcMirror)
      val res = if (lamExps.isEmpty) x else (lamExps.last)
      res.asInstanceOf[seq.Rep[R]]
    }
    f
  }

  def mirrorDef(d: Def[_], s: Exp[_], exps: List[seq.Rep[_]], symMirr: SymMirror, funcMirr: FuncMirror): (List[seq.Rep[_]], SymMirror, FuncMirror) = d match {
    case lam:Lambda[a, b] => {
      val f = mirrorLambdaToFunc[a,b](lam, symMirr, funcMirr)
      (exps, symMirr, funcMirr + ((s, f)) )
    }
    case c@Const(_) => {
      val exp = c.x
      (exps ++ List(exp), symMirr + ((s, exp)), funcMirr )
    }
    case First(tuple) => {
      tuple.elem match {
        case pe: PairElem[a, b] =>
          val tup = symMirr(tuple).asInstanceOf[seq.Rep[(a,b)]]
          val exp = tup._1
          (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
      }
    }
    case Second(tuple) => {
      tuple.elem match {
        case pe: PairElem[a, b] =>
          val tup = symMirr(tuple).asInstanceOf[seq.Rep[(a,b)]]
          val exp = tup._2
          (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
      }
    }
    case NumericTimes(arg1, arg2, n) => {
      arg1.elem match {
        case (elem:Elem[a]) =>
          val arg1_ = symMirr(arg1).asInstanceOf[seq.Rep[a]]
          val arg2_ = symMirr(arg2).asInstanceOf[seq.Rep[a]]
          implicit val n1 = n.asInstanceOf[Numeric[a]]
          val exp = seq.numeric_times[a](arg1_,arg2_)(n1, createSeqElem(elem))
          (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
      }
    }
    case NumericPlus(arg1, arg2, n) => {
      arg1.elem match {
        case (elem:Elem[a]) =>
          val arg1_ = symMirr(arg1).asInstanceOf[seq.Rep[a]]
          val arg2_ = symMirr(arg2).asInstanceOf[seq.Rep[a]]
          implicit val n1 = n.asInstanceOf[Numeric[a]]
          val exp = seq.numeric_plus[a](arg1_,arg2_)(n1, createSeqElem(elem))
          (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
      }
    }
    /*
    case NumericDivInt(arg1, arg2) =>
      val arg1_ = symMirr(arg1).asInstanceOf[lFunc.Exp[Int]]
      val arg2_ = symMirr(arg2).asInstanceOf[lFunc.Exp[Int]]
      val exp = lFunc.opDiv(arg1_, arg2_)(implicitly[Numeric[Int]], manifest[Int])
      (exps ++ List(exp), symMirr + ((s,exp)), funcMirr)
    case NumericModInt(arg1, arg2) =>
      val arg1_ = symMirr(arg1).asInstanceOf[lFunc.Exp[Int]]
      val arg2_ = symMirr(arg2).asInstanceOf[lFunc.Exp[Int]]
      val exp = lFunc.opMod(arg1_, arg2_)
      (exps ++ List(exp), symMirr + ((s,exp)), funcMirr)
    case NumericDiv(arg1, arg2, n) => {
      createManifest(arg1.elem) match {
        case (mA:Manifest[a]) =>
          val arg1_ = symMirr(arg1).asInstanceOf[lFunc.Exp[a]]
          val arg2_ = symMirr(arg2).asInstanceOf[lFunc.Exp[a]]
          val n1 = n.asInstanceOf[Numeric[a]]
          val exp = lFunc.opDiv(arg1_, arg2_)(n1, mA)
          (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
      }
    }  */
    case NotEqual(arg1, arg2) => {
      createSeqElem(arg1.elem) match {
        case (elem:seq.Elem[a]) =>
          val arg1_ = symMirr(arg1).asInstanceOf[seq.Rep[a]]
          val arg2_ = symMirr(arg2).asInstanceOf[seq.Rep[a]]
          val exp = arg1_ != arg2_
          (exps ++ List(exp), symMirr + ((s,exp)), funcMirr )
      }
    }

    case apply@ArrayApply(xs, ind) => {
      apply.selfType match {
        case elem: Elem[a] =>
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

          implicit val eB = createSeqElem(el1.eItem);
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
          implicit val eA = createSeqElem(el.eItem)
          val seqMonoid = seq.RepMonoid[a](monoid.opName, seqAppend, seqZero, monoid.isCommutative)
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

    case _ => !!!(s"${getClass.getSimpleName}: Don't know how to interpret symbol ${s.toStringWithDefinition}")
  }

  /* Mirror block */
  def mirrorDefs(defs: Schedule, symMirror: SymMirror, funcMirror: FuncMirror): (Seq[seq.Rep[_]], SymMirror, FuncMirror) =
  {
    val (seqExps, finalSymMirr, finalFuncMirr) = defs.foldLeft(List.empty[seq.Rep[_]], symMirror, funcMirror) {
      case ((exps, symMirr, funcMirr),tp) => mirrorDef(tp.rhs, tp.sym, exps, symMirr, funcMirr)
    }
    (seqExps, finalSymMirr, finalFuncMirr)
  }

  def evaluateGraph[Ctx <: staged.Transformer, A, B](in: seq.Rep[A], g: staged.ProgramGraph[Ctx]): seq.Rep[B] = {
    import scala.collection.Map

    val emptySymMirror = Map.empty[Exp[_], seq.Rep[X] forSome {type X}]
    val emptyFuncMirror = Map.empty[Exp[_], ( seq.Rep[X] => seq.Rep[Y]) forSome {type X; type Y}]

    val definitions = g.schedule

    val (seqExps, finalSymMirror, finalFuncMirror) = mirrorDefs(definitions, emptySymMirror, emptyFuncMirror)
    val res = finalFuncMirror(g.roots.last).asInstanceOf[seq.Rep[A] => seq.Rep[B]](in)

    res
  }

  def run[A,B] (f: Exp[A=>B])
                (dir: String, name: String, input: seq.Rep[A], emitGraphs: Boolean)
                (implicit eA: seq.Elem[A], eB: seq.Elem[B]): seq.Rep[B]  =
  {
    val outDir = new File(dir)
    if (emitGraphs) {
      val dotFile = new File(outDir, name + ".dot")
      emitDepGraph(f, dotFile, false)
    }
    val g0 = new PGraph(List(f))

    evaluateGraph(input, g0)
  }

  def createSeqElem[T](eA: Elem[T]) : seq.Elem[T] = {
    // Doesn't work for some reason, produces int instead of Int
    //    implicit val typeTag = eA.tag
    //    implicit val classTag = eA.classTag
    //    manifest[T]
    val seqElem = eA match {
      case UnitElement => seq.UnitElement
      case BoolElement => seq.BoolElement
      case ByteElement => seq.ByteElement
      case IntElement => seq.IntElement
      case DoubleElement => seq.DoubleElement
      case el: PairElem[_, _] =>
        seq.pairElement(createSeqElem(el.eFst), createSeqElem(el.eSnd))
      case el: FuncElem[_,_] =>
        seq.funcElement(createSeqElem(el.eDom), createSeqElem(el.eRange))
      case el: ArrayElem[_] =>
        seq.arrayElement(createSeqElem(el.eItem))
      case el => ???(s"Don't know how to create seq element for $el")
    }
    seqElem.asInstanceOf[seq.Elem[T]]
  }
}
