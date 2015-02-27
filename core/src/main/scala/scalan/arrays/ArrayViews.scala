/**
 * User: Alexander Slesarenko
 * Date: 11/23/13
 */
package scalan.arrays

import scala.reflect.runtime.universe._
import scalan._
import scalan.common.Default
import scalan.staged.BaseExp

trait ArrayViews extends ArrayOps with Views { self: Scalan =>

//  trait ArrayView[A, B] extends PArray[B] {
//    def arr: Option[PA[A]]
//    def iso: Iso[A, B]
//    def arrOrEmpty: PA[A]
//  }
//
//  def mkArrayView[A,B](view: Arr[A])(implicit iso: Iso[A,B]): Arr[B]
//  def unmkArrayView[A,B](view: Arr[B])(implicit iso: Iso[A,B]): Arr[A]
}

trait ArrayViewsSeq extends ArrayViews with ArrayOpsSeq with ViewsSeq { self: ScalanSeq =>

//  case class SeqViewArray[A, B](arr: Option[PA[A]], iso: Iso[A,B])
//    extends ViewArray[A,B] with SeqPArray[B]
//  {
//    override val elem = iso.eB
//
//    def arrOrEmpty: PA[A] = arr match { case Some(arr) => arr case None => iso.eA.empty }
//
//    def length = arr match { case Some(arr) => arr.length case None => 0 }
//    def apply(i: IntRep) = iso.to(arr.get(i))
//    def force = this
//    def toPipe = !!!
//
//    def mapBy[R:Elem](f: Rep[B => R]): PA[R] = {
//      val len = length
//      element[R].tabulate(len)(i => f(iso.to(arr.get(i))))
//    }
//
//    def slice(start: IntRep, len: IntRep) = SeqViewArray(Some(arrOrEmpty.slice(start, len)), iso)
//
//    override def flagCombine(ifFalse: PA[B], flags: PA[Boolean]) = ifFalse.asInstanceOf[SeqViewArray[A,B]] match {
//      case falseA@SeqViewArray(_, iso) => SeqViewArray(Some(arrOrEmpty.flagCombine(falseA.arrOrEmpty, flags)), iso)
//      case _ => sys.error("SeqPairArray expected by was" + ifFalse)
//    }
//
//    // length(this) + length(ifFalse) == length(flags)
//    def flagMerge(ifFalse: PA[B], flags: PA[Boolean]) = ifFalse.matchType {
//      (a: SeqViewArray[A,B]) => SeqViewArray(Some(arrOrEmpty.flagMerge(a.arrOrEmpty, flags)), iso)
//    }
//
//    // length(this) == length(flags) == (length(A) + length(B))
//    def flagSplit  (flags: PA[Boolean]) = {
//      val (at,af) = arrOrEmpty.flagSplit(flags)
//      (SeqViewArray(Some(at),iso), SeqViewArray(Some(af),iso))
//    }
//  }

//  implicit def mkArrayView[A,B](arr: PA[A])(implicit iso: Iso[A,B]): PA[B] = SeqViewArray(Some(arr), iso)
//  implicit def unmkArrayView[A,B](view: PA[B])(implicit iso: Iso[A,B]): PA[A] = view.asInstanceOf[ViewArray[A,B]].arrOrEmpty

}

trait ArrayViewsExp extends ArrayViews with ArrayOpsExp with ViewsExp with BaseExp { self: ScalanExp =>
  case class ViewArray[A, B](source: Arr[A])(implicit innerIso: Iso[A, B]) extends View1[A, B, Array] {
    lazy val iso = arrayIso(innerIso)
    def copy(source: Arr[A]) = ViewArray(source)
    override def toString = s"ViewArray[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewArray[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }

  object UserTypeArray {
    def unapply(s: Exp[_]): Option[Iso[_, _]] = {
      s.elem match {
        case ArrayElem(UnpackableElem(iso)) => Some(iso)
        case _ => None
      }
    }
  }

  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ViewArray[_, _]) =>
      Some((view.source, arrayIso(view.iso)))
    case UserTypeArray(iso: Iso[a, b]) =>
      val newIso = arrayIso(iso)
      val repr = reifyObject(UnpackView(s.asRep[Array[b]])(newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  case class ArrayIso[A:Elem,B:Elem](iso: Iso[A,B]) extends Iso[Array[A], Array[B]] {
    lazy val eTo = element[Array[B]]
    def from(x: Arr[B]) = x.map(iso.from _)
    def to(x: Arr[A]) = x.map(iso.to _)
    lazy val tag = {
      implicit val tB = iso.tag
      weakTypeTag[Array[B]]
    }
    lazy val defaultRepTo = Default.defaultVal(SArray.empty[B])
  }

  def arrayIso[A, B](iso: Iso[A, B]): Iso[Array[A], Array[B]] = {
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    ArrayIso(iso)
  }
  
  val HasViewArrayArg = HasArg(hasViewArrayArg)

  protected def hasViewArrayArg(s: Exp[_]): Boolean = s match {
    case Def(_: ViewArray[_, _]) => true
    case _ => false
  }

  def flatMapUnderlyingArray[A,B,C](view: ViewArray[A,B], f: Rep[B=>Array[C]]): Arr[C] = {
    val iso = view.innerIso
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    implicit val eAC: Elem[Array[C]] = f.elem.eRange
    implicit val eC = eAC.eItem
    view.source.flatMap { x => f(iso.to(x)) }
  }
  def mapUnderlyingArray[A,B,C](view: ViewArray[A,B], f: Rep[B=>C]): Arr[C] = {
    val iso = view.innerIso
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    implicit val eC: Elem[C] = f.elem.eRange
    view.source.map { x => f(iso.to(x)) }
  }

  def reduceUnderlyingArray[A,B](view: ViewArray[A,B], m: RepMonoid[B]): Rep[A] = {
    val iso = view.innerIso
    println(iso)
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    val zeroNew = iso.from(m.zero)
    //val appendNew = { (x: Rep[A],y: Rep[A]) => iso.from(m.append(iso.to(x), iso.to(y))) }
    val newMonoid = new RepMonoid(m.opName, zeroNew, fun { p: Rep[(A, A)] => iso.from(m.append(iso.to(p._1), iso.to(p._2)))}, m.isCommutative)(eA)
    view.source.reduce( newMonoid)
  }

  def mapReduceUnderlyingArray[A,B,K,V](view: ViewArray[A,B], map: Rep[B=>(K,V)], reduce: Rep[((V,V))=>V]): MM[K,V] = {
    val iso = view.innerIso
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    implicit val eK: Elem[K] = map.elem.eRange.eFst
    implicit val eV: Elem[V] = map.elem.eRange.eSnd
    view.source.mapReduceBy[K,V]( fun { x => map(iso.to(x)) }, reduce)
  }

  def foldUnderlyingArray[A,B,S](view: ViewArray[A,B], init:Rep[_], f: Rep[((S,B)) => S]): Rep[S] = {
    val iso = view.innerIso
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    implicit val eS = f.elem.eRange
    view.source.fold[S](init.asRep[S], fun { p => f((p._1, iso.to(p._2))) })
  }

  def filterUnderlyingArray[A, B](view: ViewArray[A, B], f: Rep[B => Boolean]): Arr[B] = {
    val iso = view.innerIso
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    val filtered = view.source.filter { x => f(iso.to(x)) }
    ViewArray(filtered)(iso)
  }
  
  def liftViewArrayFromArgs[T](d: Def[T])/*(implicit eT: Elem[T])*/: Option[Exp[_]] = d match {
    case ArrayApply(Def(view: ViewArray[a, b]), i) =>
      implicit val eA = view.innerIso.eFrom
      implicit val eB = view.innerIso.eTo
      val res = view.innerIso.to(view.source(i))
      Some(res)
  /*
    case ArrayUpdate(Def(view: ViewArray[a, b]), i, Def(UnpackableDef(value, iso2: Iso[c, d]))) if view.innerIso == iso2 =>
      implicit val eA = view.innerIso.eFrom
      implicit val eB = view.innerIso.eTo
      val res = ViewArray(view.source.update(i, value.asRep[a]))(view.innerIso)
      Some(res)
      */
    case ArrayUpdate(Def(view: ViewArray[a, b]), i, HasViews(value, iso2: Iso[c, d])) if view.innerIso == iso2 =>
      implicit val eA = view.innerIso.eFrom
      implicit val eB = view.innerIso.eTo
      val res = ViewArray(view.source.update(i, value.asRep[a]))(view.innerIso)
      Some(res)
    case ArrayFold(Def(view: ViewArray[_,_]), init, f) =>
      Some(foldUnderlyingArray(view, init, f))
    case ArrayApplyMany(Def(view: ViewArray[a, b]), is) =>
      implicit val eA = view.innerIso.eFrom
      implicit val eB = view.innerIso.eTo
      val res = ViewArray(view.source(is))(view.innerIso)
      Some(res)
    case ArrayFlatMap(Def(view: ViewArray[_, _]), f) =>
      Some(flatMapUnderlyingArray(view, f))
    case ArrayMap(Def(view: ViewArray[_, _]), f) =>
      Some(mapUnderlyingArray(view, f))
    case red @ ArrayReduce(Def(view: ViewArray[_, _]), _) =>
      Some(reduceUnderlyingArray(view, red.m))
    case ArrayMapReduce(Def(view: ViewArray[_, _]), map, reduce) =>
      Some(mapReduceUnderlyingArray(view, map, reduce))
    case ArrayFilter(Def(view: ViewArray[_, _]), f) =>
      Some(filterUnderlyingArray(view, f))
    case pa @ ArrayZip(arr1: Arr[a] @unchecked, Def(v1:ViewArray[_,_])) => {
      implicit val eA = arr1.elem.eItem
      val iso2 = identityIso(eA)
      val pIso = pairIso(iso2, v1.innerIso)
      implicit val eAB = pIso.eTo
      val zipped = arr1 zip v1.source
      Some(ViewArray(zipped)(pIso))
    }
    case _ => None
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    case ArrayLength(Def(ViewArray(arr: Arr[a] @unchecked))) =>
      array_length(arr)
    case HasViewArrayArg(_) => liftViewArrayFromArgs(d) match {
      case Some(s) => s
      case _ => {
        //println("Can not clean this view: " +d)
        super.rewriteDef(d)
      }
    }
    case ArrayUpdate(arr, i, HasViews(srcValue, iso: Iso[a,b]))  =>
      val value = srcValue.asRep[a]
      implicit val eA = iso.eFrom
      implicit val eB = iso.eTo
      val arrIso = arrayIso[a,b](iso)
      val srcBuf = arrIso.from(arr.asRep[Array[b]])
      ViewArray(srcBuf.update(i, value))(iso)
    case ArrayMap(xs: Arr[a] @unchecked, f@Def(Lambda(_, _, _, UnpackableExp(_, iso: Iso[c, b])))) =>
      val f1 = f.asRep[a => b]
      val xs1 = xs.asRep[Array[a]]
      implicit val eA = xs1.elem.eItem
      // implicit val eB = iso.eTo
      implicit val eC = iso.eFrom
      // implicit val leA = Lazy(eA)
      val s = xs1.map { x =>
        val tmp = f1(x)
        iso.from(tmp)
        // UnpackView(f1(x))(iso)
      }
      val res = ViewArray(s)(iso)
      // val res = ViewArray(s.values)(iso).nestBy(s.segments)
      res
    case ArrayFlatMap(xs: Arr[a] @unchecked, f@Def(Lambda(_, _, _, UnpackableExp(_, arrIso: ArrayIso[c, b])))) =>
      val f1 = f.asRep[a => Array[b]]
      val xs1 = xs.asRep[Array[a]]
      implicit val eA = xs1.elem.eItem
      implicit val eC = arrIso.iso.eFrom

      val s = xs1.flatMap { x =>
        val tmp = f1(x)
        arrIso.from(tmp)
      }
      val res = ViewArray(s)(arrIso.iso)
      res

      /*
    case ArrayFold(xs: Arr[a], HasViews(initWithoutViews, iso: Iso[b, c]), f) =>
      val xs1 = xs.asRep[Array[a]]
      val init = initWithoutViews.asRep[b]
      val step = f.asRep[((c,a))=>c]
      implicit val eA = xs1.elem.eItem
      implicit val eB = iso.eFrom
      implicit val eC = iso.eTo
      val res = xs1.fold(init, fun {(p: Rep[(b,a)]) => iso.from(step((iso.to(p._1), p._2)))})
      iso.to(res)
      */
    case view1@ViewArray(Def(view2@ViewArray(arr))) =>
      //println(view1)
      val compIso = composeIso(view2.innerIso, view1.innerIso)
      implicit val eAB = compIso.eTo
      ViewArray(arr)(compIso)
    case _ =>
      super.rewriteDef(d)
  }
}