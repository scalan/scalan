package scalan.compilation.lms

import scalan.ScalanCtxExp
import scalan.compilation.lms.scalac.LmsManifestUtil
import LmsManifestUtil._

trait LmsBridge { self: ScalanCtxExp =>

  val lms: LmsBackend

  type LmsFunction = (lms.Exp[A] => lms.Exp[B]) forSome {type A; type B}

  class LmsMirror(val lastExp: Option[lms.Exp[_]], symMirror: Map[Exp[_], lms.Exp[_]], funcMirror: Map[Exp[_], LmsFunction]) {
    def addSym(scalanExp: Exp[_], lmsExp: lms.Exp[_]) =
      new LmsMirror(Some(lmsExp), symMirror.updated(scalanExp, lmsExp), funcMirror)
    def addFuncAndSym(scalanExp: Exp[_], lmsFunc: LmsFunction, lmsExp: lms.Exp[_]) =
      new LmsMirror(Some(lmsExp), symMirror.updated(scalanExp, lmsExp), funcMirror.updated(scalanExp, lmsFunc))
    def addFunc(scalanExp: Exp[_], lmsFunc: LmsFunction) =
      new LmsMirror(lastExp, symMirror, funcMirror.updated(scalanExp, lmsFunc))
    def withoutLastExp = new LmsMirror(None, symMirror, funcMirror)

    def symMirror[A](scalanExp: Exp[_]): lms.Exp[A] = symMirror.apply(scalanExp).asInstanceOf[lms.Exp[A]]
    def symMirrorUntyped(scalanExp: Exp[_]): lms.Exp[_] = symMirror.apply(scalanExp)
    def funcMirror[A, B](scalanExp: Exp[_]): lms.Exp[A] => lms.Exp[B] =
      funcMirror.apply(scalanExp).asInstanceOf[lms.Exp[A] => lms.Exp[B]]
  }

  object LmsMirror {
    val empty = new LmsMirror(None, Map.empty, Map.empty)
  }

  type EntryTransformer = PartialFunction[TableEntry[_], LmsMirror]
  type DefTransformer = PartialFunction[Def[_], LmsMirror]

  def defTransformer[T](m: LmsMirror, g: AstGraph, e: TableEntry[T]): DefTransformer = {
    case x => !!!(s"LMSBridge: Don't know how to mirror symbol ${x.self.toStringWithDefinition}")
  }

  def tableTransformer(m: LmsMirror, g: AstGraph): EntryTransformer = {
    case e => defTransformer(m, g, e)(e.rhs)
  }

  def mirrorLambdaToLmsFunc[I, R](m: LmsMirror)(lam: Lambda[I, R]): (lms.Exp[I] => lms.Exp[R]) = {
    val lamX = lam.x
    val f = { x: lms.Exp[I] =>
      val sched = lam.scheduleSingleLevel
      val lastExp = mirrorDefs(m.addSym(lamX, x))(lam, sched).lastExp
      val res = lastExp.getOrElse(x)
      res.asInstanceOf[lms.Exp[R]]
    }
    f
  }

  /* Mirror block */
  def mirrorBlockToLms[R](m: LmsMirror)(block: ThunkDef[_], dflt: Rep[_]): () => lms.Exp[R] = { () =>
    val sched = block.scheduleSingleLevel
    val lastExp = mirrorDefs(m)(block, sched).lastExp
    val res = lastExp.getOrElse(m.symMirrorUntyped(dflt))
    res.asInstanceOf[lms.Exp[R]]
  }

  def mirrorDefs(m: LmsMirror)(fromGraph: AstGraph, defs: Seq[TableEntry[_]]): LmsMirror = {
    val init: LmsMirror = m.withoutLastExp
    val finalMirror = defs.foldLeft(init)((m, t) => tableTransformer(m, fromGraph)(t))
    finalMirror
  }

  // can't just return lmsFunc: lms.Exp[A] => lms.Exp[B], since mirrorDefs needs to be called in LMS context
  def apply[A, B](g: PGraph) = { x: lms.Exp[A] =>
    val finalMirror = mirrorDefs(LmsMirror.empty)(g, g.schedule)
    val lmsFunc = finalMirror.funcMirror[A, B](g.roots.last)
    lmsFunc(x)
  }

  def createManifest[T]: PartialFunction[Elem[T], Manifest[_]] = {
    case el: ArrayBufferElem[_] => Manifest.classType(classOf[scala.collection.mutable.ArrayBuilder[_]], createManifest(el.eItem))
    case PairElem(eFst, eSnd) =>
      Manifest.classType(classOf[(_, _)], createManifest(eFst), createManifest(eSnd))
    case SumElem(eLeft, eRight) =>
      Manifest.classType(classOf[Either[_, _]], createManifest(eLeft), createManifest(eRight))
    case el: FuncElem[_, _] =>
      Manifest.classType(classOf[_ => _], createManifest(el.eDom), createManifest(el.eRange))
    case el: ArrayElem[_] =>
      // see Scala bug https://issues.scala-lang.org/browse/SI-8183 (won't fix)
      val m = el.eItem match {
        case UnitElement => manifest[scala.runtime.BoxedUnit]
        case _ => createManifest(el.eItem)
      }
      Manifest.arrayType(m)
    case el: ListElem[_] =>
      Manifest.classType(classOf[List[_]], createManifest(el.eItem))
    case el: MMapElem[_,_] =>
      Manifest.classType(classOf[java.util.HashMap[_,_]], createManifest(el.eKey), createManifest(el.eValue))
    case el: Element[_] => tagToManifest[T](el.tag)
    case el => ???(s"Don't know how to create manifest for $el")
  }
}
