package scalan

import codegen.GraphVizExport
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
  val staged: ScalanStaged with GraphVizExport //with VectorsDslExp

  import staged._

  type SymMirror = Map[Exp[_], seq.Rep[A] forSome {type A}]

  type FuncMirror = Map[Exp[_],( seq.Rep[A] => seq.Rep[B]) forSome { type A; type B } ]


  def mirrorLambdaToFunc[I, R](lam:Lambda[I,R],
                               symMirror: SymMirror,
                               funcMirror: FuncMirror) : (seq.Rep[I] => seq.Rep[R]) = {
    val lamX = lam.x
    val f = { x: seq.Rep[I] =>
      val sched = lam.bodySchedule
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

    case _ => !!!(s"${getClass.getSimpleName}: Don't know how to interpret symbol ${s.toStringWithDefinition}")
  }

  /* Mirror block */
  def mirrorDefs(defs: Seq[TableEntry[_]], symMirror: SymMirror, funcMirror: FuncMirror): (Seq[seq.Rep[_]], SymMirror, FuncMirror) =
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
    (emitGraphs) match {
      case true =>
        val dotFile = new File(outDir, name + ".dot")
        import staged._
        emitDepGraph(f, dotFile.getAbsolutePath(), false)
      case _ =>
    }
    val g0 = new PGraph(List(f))

    evaluateGraph(input, g0)
  }

  def createSeqElem[T](eA: Elem[T]) : seq.Elem[T] = {
    // Doesn't work for some reason, produces int instead of Int
    //    implicit val typeTag = eA.tag
    //    implicit val classTag = eA.classTag
    //    manifest[T]
    val elem = eA match {
      case UnitElement => seq.UnitElement
      case IntElement => seq.IntElement
      case DoubleElement => seq.DoubleElement
      case el: PairElem[_, _] =>
        seq.pairElement(createSeqElem(el.eFst), createSeqElem(el.eSnd))
      case el: FuncElem[_,_] =>
        seq.funcElement(createSeqElem(el.eDom), createSeqElem(el.eRange))
      case el => ???(s"Don't know how to create seq element for $el")
    }
    elem.asInstanceOf[seq.Elem[T]]
  }
}
