package scalan.compilation.lms

import scalan.ScalanCtxExp
import scalan.compilation.lms.scalac.LmsManifestUtil
import LmsManifestUtil._

trait LmsBridge { self: ScalanCtxExp =>

  val lms: LmsBackend

  type LmsFunction = (lms.Exp[A] => lms.Exp[B]) forSome {type A; type B}

  class LmsMirror private (
       lastExp: Option[lms.Exp[_]],
       private val symMirror: Map[Exp[_], lms.Exp[_]],
       funcMirror: Map[Exp[_], LmsFunction])
  {
    def addSym(scalanExp: Exp[_], lmsExp: lms.Exp[_]) =
      new LmsMirror(Some(lmsExp), symMirror.updated(scalanExp, lmsExp), funcMirror)
    def addFuncAndSym(scalanExp: Exp[_], lmsFunc: LmsFunction, lmsExp: lms.Exp[_]) =
      new LmsMirror(Some(lmsExp), symMirror.updated(scalanExp, lmsExp), funcMirror.updated(scalanExp, lmsFunc))
    def addFunc(scalanExp: Exp[_], lmsFunc: LmsFunction) =
      new LmsMirror(lastExp, symMirror, funcMirror.updated(scalanExp, lmsFunc))
    private def withoutLastExp = new LmsMirror(None, symMirror, funcMirror)
    private def withLastExp(e: lms.Exp[_]) = new LmsMirror(Some(e), symMirror, funcMirror)
    private def lastExpOrElse(default: => lms.Exp[_]) = lastExp.getOrElse(default)

    def symMirror[A](scalanExp: Exp[_]): lms.Exp[A] = symMirror.apply(scalanExp).asInstanceOf[lms.Exp[A]]
    def symsMirror[A](scalanExps: List[Exp[_]]): List[lms.Sym[A]] =
      scalanExps.map(e => symMirror[A](e).asInstanceOf[lms.Sym[A]])
    def symMirrorUntyped(scalanExp: Exp[_]): lms.Exp[_] = symMirror.apply(scalanExp)
    def funcMirror[A, B](scalanExp: Exp[_]): lms.Exp[A] => lms.Exp[B] =
      funcMirror.apply(scalanExp).asInstanceOf[lms.Exp[A] => lms.Exp[B]]

    def summaryMirror(ss: Summary): lms.Summary =
      new lms.Summary(ss.maySimple, ss.mstSimple, ss.mayGlobal, ss.mstGlobal, ss.resAlloc, ss.control,
                      symsMirror(ss.mayRead), symsMirror(ss.mstRead), symsMirror(ss.mayWrite), symsMirror(ss.mstWrite))

    def mirrorLambda[I, R](lam: Lambda[I, R]): (lms.Exp[I] => lms.Exp[R]) = {
      val lamX = lam.x
      val f = { x: lms.Exp[I] =>
        val sched = lam.filterReifyRoots(lam.scheduleSingleLevel)
        val finalMirror = addSym(lamX, x).mirrorDefs(lam, sched)
        val res = finalMirror.lastExpOrElse(x)
        res.asInstanceOf[lms.Exp[R]]
      }
      f
    }

    def mirrorBlock[R](block: ThunkDef[_], dflt: Rep[_]): () => lms.Exp[R] = { () =>
      val sched = block.filterReifyRoots(block.scheduleSingleLevel)
      val finalMirror = mirrorDefs(block, sched)
      val res = finalMirror.lastExpOrElse(symMirrorUntyped(dflt))
      res.asInstanceOf[lms.Exp[R]]
    }

    def mirrorDefs(fromGraph: AstGraph, defs: Seq[TableEntry[_]]): LmsMirror = {
      val finalMirror = defs.foldLeft(withoutLastExp) { (m, t) =>
        val s = t.sym
        m.symMirror.get(s) match {
          case Some(lmsExp) => m.withLastExp(lmsExp)
          case None => transformDef(m, fromGraph, s, t.rhs)
        }
      }
      finalMirror
    }
  }

  object LmsMirror {
    val empty = new LmsMirror(None, Map.empty, Map.empty)
  }

  def transformDef[T](m: LmsMirror, g: AstGraph, sym: Exp[T], d: Def[T]): LmsMirror = {
    !!!(s"LMSBridge: Don't know how to mirror ${sym.toStringWithDefinition}")
  }

  // can't just return lmsFunc: lms.Exp[A] => lms.Exp[B], since mirrorDefs needs to be called in LMS context
  def apply[A, B](g: PGraph) = { x: lms.Exp[A] =>
    val finalMirror = LmsMirror.empty.mirrorDefs(g, g.schedule)
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
