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
  
  case class ViewArray[A, B](source: Arr[A])(iso: Iso1[A, B, Array])
    extends View1[A, B, Array](iso) {
    //def this(source: Arr[A])(iso: Iso[A, B]) = this(source)(ArrayIso(iso))
    //lazy val iso = ArrayIso(innerIso)
    def copy(source: Arr[A]) = ViewArray(source)(iso)
    override def toString = s"ViewArray[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewArray[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }

  object UserTypeArray {
    def unapply(s: Exp[_]): Option[Iso[_, _]] = {
      s.elem match {
        case ScalaArrayElem(UnpackableElem(iso)) => Some(iso)
        case _ => None
      }
    }
  }

  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ViewArray[a, b]) =>
      Some((view.source, view.iso))
    case UserTypeArray(iso: Iso[a, b]) =>
      val newIso = ArrayIso(iso)
      val repr = reifyObject(UnpackView(s.asRep[Array[b]])(newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]


//  def ArrayIso[A, B](iso: Iso[A, B]): Iso[Array[A], Array[B]] =
//    ArrayIso(iso)

  private def flatMapUnderlyingArray[A,B,C](view: ViewArray[A,B], f: Rep[B=>Array[C]]): Arr[C] = {
    val iso = view.innerIso
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    implicit val eAC: Elem[Array[C]] = f.elem.eRange
    implicit val eC = eAC.eItem
    view.source.flatMap { x => f(iso.to(x)) }
  }

  private def mapUnderlyingArray[A,B,C](view: ViewArray[A,B], f: Rep[B=>C]): Arr[C] = {
    val iso = view.innerIso
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    implicit val eC: Elem[C] = f.elem.eRange
    view.source.map { x => f(iso.to(x)) }
  }

  private def reduceUnderlyingArray[A,B](view: ViewArray[A,B], m: RepMonoid[B]): Rep[A] = {
    val iso = view.innerIso
    println(iso)
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    val zeroNew = iso.from(m.zero)
    //val appendNew = { (x: Rep[A],y: Rep[A]) => iso.from(m.append(iso.to(x), iso.to(y))) }
    val newMonoid = new RepMonoid(m.opName, zeroNew, fun { p: Rep[(A, A)] => iso.from(m.append(iso.to(p._1), iso.to(p._2)))}, m.isCommutative)(eA)
    view.source.reduce( newMonoid)
  }

  private def mapReduceUnderlyingArray[A,B,K,V](view: ViewArray[A,B], map: Rep[B=>(K,V)], reduce: Rep[((V,V))=>V]): MM[K,V] = {
    val iso = view.innerIso
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    implicit val eK: Elem[K] = map.elem.eRange.eFst
    implicit val eV: Elem[V] = map.elem.eRange.eSnd
    view.source.mapReduceBy[K,V]( fun { x => map(iso.to(x)) }, reduce)
  }

  private def foldUnderlyingArray[A,B,S](view: ViewArray[A,B], init:Rep[_], f: Rep[((S,B)) => S]): Rep[S] = {
    val iso = view.innerIso
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    implicit val eS = f.elem.eRange
    view.source.fold[S](init.asRep[S], fun { p => f((p._1, iso.to(p._2))) })
  }

  private def filterUnderlyingArray[A, B](view: ViewArray[A, B], f: Rep[B => Boolean]): Arr[B] = {
    val iso = view.innerIso
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    val filtered = view.source.filter { x => f(iso.to(x)) }
    ViewArray(filtered)(ArrayIso(iso))
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    case ArrayLength(Def(ViewArray(arr: Arr[a] @unchecked))) =>
      array_length(arr)
    case ArrayApply(Def(view: ViewArray[a, b]), i) =>
      implicit val eA = view.innerIso.eFrom
      implicit val eB = view.innerIso.eTo
      val res = view.innerIso.to(view.source(i))
      res
    /*
      case ArrayUpdate(Def(view: ViewArray[a, b]), i, Def(UnpackableDef(value, iso2: Iso[c, d]))) if view.innerIso == iso2 =>
        implicit val eA = view.innerIso.eFrom
        implicit val eB = view.innerIso.eTo
        val res = ViewArray(view.source.update(i, value.asRep[a]))(view.innerIso)
        res
        */
    case ArrayUpdate(Def(view: ViewArray[a, b]), i, HasViews(value, iso2: Iso[c, d])) if view.innerIso == iso2 =>
      implicit val eA = view.innerIso.eFrom
      implicit val eB = view.innerIso.eTo
      ViewArray(view.source.update(i, value.asRep[a]))(view.iso)
    case ArrayFold(Def(view: ViewArray[_,_]), init, f) =>
      foldUnderlyingArray(view, init, f)
    case ArrayApplyMany(Def(view: ViewArray[a, b]), is) =>
      implicit val eA = view.innerIso.eFrom
      implicit val eB = view.innerIso.eTo
      ViewArray(view.source(is))(view.iso)
    case ArrayFlatMap(Def(view: ViewArray[_, _]), f) =>
      flatMapUnderlyingArray(view, f)
    case ArrayMap(Def(view: ViewArray[_, _]), f) =>
      mapUnderlyingArray(view, f)
    case red @ ArrayReduce(Def(view: ViewArray[_, _]), _) =>
      reduceUnderlyingArray(view, red.m)
    case ArrayMapReduce(Def(view: ViewArray[_, _]), map, reduce) =>
      mapReduceUnderlyingArray(view, map, reduce)
    case ArrayFilter(Def(view: ViewArray[_, _]), f) =>
      filterUnderlyingArray(view, f)

    case pa @ ArrayZip(Def(v1:ViewArray[a,_]), arr2: Arr[b] @unchecked) =>
      implicit val eA = v1.source.elem.eItem
      implicit val eB = arr2.elem.eItem
      val iso2 = identityIso(eB)
      val pIso = ArrayIso(pairIso(v1.innerIso, iso2))
      implicit val eAB = pIso.eTo
      implicit val eBA = pIso.eFrom
      val zipped = v1.source zip arr2
      ViewArray(zipped)(pIso)

    case pa @ ArrayZip(arr1: Arr[a] @unchecked, Def(v2:ViewArray[_,_])) =>
      implicit val eA = arr1.elem.eItem
      val iso2 = identityIso(eA)
      val pIso = ArrayIso(pairIso(iso2, v2.innerIso))
      implicit val eAB = pIso.eTo
      val zipped = arr1 zip v2.source
      ViewArray(zipped)(pIso)

    // Rule: ???
    case ArrayUpdate(arr, i, HasViews(srcValue, iso: Iso[a,b]))  =>
      val value = srcValue.asRep[a]
      implicit val eA = iso.eFrom
      implicit val eB = iso.eTo
      val arrIso = ArrayIso[a,b](iso)
      val srcBuf = arrIso.from(arr.asRep[Array[b]])
      ViewArray(srcBuf.update(i, value))(arrIso)

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
      val res = ViewArray(s)(ArrayIso(iso))
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
      val res = ViewArray(s)(ArrayIso(arrIso.iso))
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
    case ArrayToList(Def(view: ViewArray[a, b])) =>
      val iso = view.innerIso
      implicit val eA: Elem[a] = iso.eFrom
      ViewList(view.source.toList)(ListIso(iso))
    case view1@ViewArray(Def(view2@ViewArray(arr))) =>
      //println(view1)
      val compIso = composeIso(view1.innerIso, view2.innerIso)
      implicit val eAB = compIso.eTo
      ViewArray(arr)(ArrayIso(compIso))
    case _ =>
      super.rewriteDef(d)
  }
}