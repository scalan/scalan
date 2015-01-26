package scalan.compilation.lms

import scalan.ScalanExp

trait LmsBridge[A, B] {

  val scalan: ScalanExp
  val lms: LmsBackend

  type SymMirror = Map[scalan.Exp[_], lms.Exp[A] forSome {type A}]
  type FuncMirror = Map[scalan.Exp[_], (lms.Exp[A] => lms.Exp[B]) forSome {type A; type B}]
  type ExpMirror = Seq[lms.Exp[_]]

  type Mirror = (ExpMirror, SymMirror, FuncMirror)

  type EntryTransformer = PartialFunction[scalan.TableEntry[_], Mirror]
  type DefTransformer = PartialFunction[scalan.Def[_], Mirror]

  def defTransformer[T](m: Mirror, g: scalan.AstGraph, e: scalan.TableEntry[T]): DefTransformer = {
    case x => scalan.!!!(s"ScalanLMSBridge: Don't know how to mirror symbol ${x.self.toStringWithDefinition}")
  }

  def tableTransformer(m: Mirror, g: scalan.AstGraph): EntryTransformer = {
    case e => defTransformer(m, g, e)(e.rhs)
  }

  def mirrorLambdaToLmsFunc[I, R](m: Mirror)(lam: scalan.Lambda[I, R]): (lms.Exp[I] => lms.Exp[R]) = {
    val (expMirror, symMirror, funcMirror) = m
    val lamX = lam.x
    val f = { x: lms.Exp[I] =>
      val sched = lam.scheduleSingleLevel
      val (lamExps, _, _) = mirrorDefs((expMirror, symMirror + ((lamX, x)), funcMirror))(lam, sched)
      val res = lamExps.lastOption.getOrElse(x)
      res.asInstanceOf[lms.Exp[R]]
    }
    f
  }

  /* Mirror block */
  def mirrorBlockToLms[R](m: Mirror)(block: scalan.ThunkDef[_], dflt: scalan.Rep[_]): () => lms.Exp[R] = { () =>
    val (_, symMirror, _) = m
    val sched = block.scheduleSingleLevel
    val (blockExps, _, _) = mirrorDefs(m)(block, sched)
    val res = blockExps.lastOption.getOrElse(symMirror(dflt))
    res.asInstanceOf[lms.Exp[R]]
  }

  def mirrorDefs(m: Mirror)(fromGraph: scalan.AstGraph, defs: Seq[scalan.TableEntry[_]]): Mirror = {
    val (_, symMirror, funcMirror) = m
    val init: Mirror = (List.empty[lms.Exp[_]], symMirror, funcMirror)
    val (lmsExps, finalSymMirr, finalFuncMirr) = defs.foldLeft(init)((m, t) => tableTransformer(m, fromGraph)(t))
    (lmsExps, finalSymMirr, finalFuncMirr)
  }

  class LmsFacade[Ctx <: scalan.Transformer](g: scalan.ProgramGraph[Ctx]) {
    def apply(in: lms.Exp[A]): lms.Exp[B] = {
      val emptyMirror: Mirror = (Seq.empty, Map.empty, Map.empty)
      val (_, _, finalFuncMirror) = mirrorDefs(emptyMirror)(g, g.schedule)
      val res = finalFuncMirror(g.roots.last).asInstanceOf[lms.Exp[A] => lms.Exp[B]](in)

      res
    }
  }

  def getFacade[Ctx <: scalan.Transformer](g: scalan.ProgramGraph[Ctx]) = new LmsFacade[Ctx](g)
}
