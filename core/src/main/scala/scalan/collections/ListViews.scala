/**
 * User: Alexander Slesarenko
 * Date: 11/23/13
 */
package scalan.collections

import scala.reflect.runtime.universe._
import scalan._
import scalan.staged.BaseExp

trait ListViews extends ListOps with Views { self: Scalan =>
}

trait ListViewsStd extends ListViews with ListOpsStd with ViewsDslStd { self: ScalanStd =>
}

trait ListViewsExp extends ListViews with ListOpsExp with ViewsDslExp with BaseExp { self: ScalanExp =>

  case class ViewList[A, B](source: Lst[A], override val innerIso: Iso[A, B]) extends View1[A, B, List](listIso(innerIso)) {
    override def toString = s"ViewList[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewList[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }

  object UserTypeList {
    def unapply(s: Exp[_]): Option[Iso[_, _]] = {
      s.elem match {
        case ListElem(UnpackableElem(iso)) => Some(iso)
        case _ => None
      }
    }
  }

  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ViewList[_, _]) =>
      Some((view.source, view.iso))
    case UserTypeList(iso: Iso[a, b]) =>
      val newIso = listIso(iso)
      val repr = reifyObject(UnpackView(s.asRep[List[b]], newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  override def rewriteDef[T](d: Def[T]) = d match {
    case ListLength(Def(ViewList(arr: Lst[a]@unchecked, _))) =>
      list_length(arr)

    case lm: ListMap[_,c] => (lm.xs, lm.f) match {
      case (xs: Lst[a]@unchecked, f@Def(Lambda(_, _, _, HasViews(_, iso: Iso[b, c])))) => {
        val f1 = f.asRep[a => c]
        implicit val eA = xs.elem.eItem
        implicit val eB = iso.eFrom
        val s = xs.mapBy(f1 >> iso.fromFun)
        val res = ViewList(s, iso)
        res
      }
      case (Def(view: ViewList[a, b]), _) => {
        val iso = view.innerIso
        val ff = lm.f.asRep[b => c]
        implicit val eA = iso.eFrom
        implicit val eC = ff.elem.eRange
        view.source.mapBy(iso.toFun >> ff)
      }
      case _ =>
        super.rewriteDef(d)
    }
    case lm: ListFlatMap[_,c] => (lm.xs, lm.f) match {
      case (xs: Lst[a]@unchecked, f@Def(Lambda(_, _, _, HasViews(_, Def(listIso: ListIso[b, c]))))) => {
        val f1 = f.asRep[a => List[c]]
        val xs1 = xs.asRep[List[a]]
        implicit val eA = xs1.elem.eItem
        implicit val eC = listIso.innerIso.eFrom

        val s = xs1.flatMapBy(f1 >> listIso.fromFun)
        val res = ViewList(s, listIso.innerIso)
        res
      }
      case (Def(view: ViewList[a, b]), _) => {
        val iso = view.innerIso
        val f = lm.f.asRep[b => List[c]]
        implicit val eA = iso.eFrom
        implicit val eC = f.elem.eRange.eItem
        view.source.flatMapBy(iso.toFun >> f)
      }
      case _ =>
        super.rewriteDef(d)
    }
    case ListFilter(Def(view: ViewList[a, b]), f) => {
      val iso = view.innerIso
      implicit val eA = iso.eFrom
      implicit val eB = iso.eTo
      val filtered = view.source.filterBy(iso.toFun >> f)
      ViewList(filtered, iso)
    }
    case view1@ViewList(Def(view2@ViewList(arr, innerIso2)), innerIso1) => {
      val compIso = composeIso(innerIso1, innerIso2)
      implicit val eAB = compIso.eTo
      ViewList(arr, compIso)
    }
    case ListFoldLeft(xs, init, step) => (xs, init, step) match {
      case (Def(view: ViewList[a, b]), init: Rep[s], step) => {
        val iso = view.innerIso
        implicit val eA = iso.eFrom
        implicit val eB = iso.eTo
        implicit val eS = init.elem
        view.source.foldLeft(init)(fun { (p: Rep[(s, a)]) => step.asRep[((s, b)) => s](Pair(p._1, iso.to(p._2)))})
      }
      case (xs: Rep[List[a]] @unchecked, HasViews(initWithoutView, iso: Iso[s1, s2]), f) => {
        val init = initWithoutView.asRep[s1]
        val step = f.asRep[((s2,a))=>s2]
        implicit val eA = xs.elem.eItem
        implicit val eS1 = iso.eFrom
        implicit val eS2 = iso.eTo
        val res = xs.foldLeft(init)(fun {(p: Rep[(s1,a)]) =>
          iso.from(step(Pair(iso.to(p._1), p._2)))
        })
        iso.to(res)
      }
      case _ =>
        super.rewriteDef(d)
    }
    case ListFoldRight(xs, init, step) => (xs, init, step) match {
      case (Def(view: ViewList[a, b]), init: Rep[s], step) => {
        val iso = view.innerIso
        implicit val eA = iso.eFrom
        implicit val eB = iso.eTo
        implicit val eS = init.elem
        view.source.foldRight(init)(fun { (p: Rep[(a, s)]) => step.asRep[((b, s)) => s](Pair(iso.to(p._1), p._2))})
      }
      case (xs: Rep[List[a]] @unchecked, HasViews(initWithoutView, iso: Iso[s1, s2] @unchecked), f) => {
        val init = initWithoutView.asRep[s1]
        val step = f.asRep[((a,s2))=>s2]
        implicit val eA = xs.elem.eItem
        implicit val eS1 = iso.eFrom
        implicit val eS2 = iso.eTo
        val res = xs.foldRight(init)(fun {(p: Rep[(a,s1)]) =>
          iso.from(step(Pair(p._1, iso.to(p._2))))
        })
        iso.to(res)
      }
      case _ =>
        super.rewriteDef(d)
    }
    case ListReplicate(len, HasViews(valueWithoutView, iso: Iso[a, b] @unchecked)) => {
      val v = valueWithoutView.asRep[a]
      implicit val eA = iso.eFrom
      implicit val eB = iso.eTo
      ViewList(SList.replicate(len, v), iso)
    }

    case ListCons(x@HasViews(_, _), HasViews(xs, Def(iso: ListIso[a, b]))) =>
      implicit val eA = iso.innerIso.eFrom
      val xs1 = xs.asRep[List[a]]
      val x1 = iso.innerIso.from(x.asRep[b])
      ViewList(RepListOps(xs1).::(x1), iso.innerIso)

    case ListConcat(HasViews(xs, Def(iso: ListIso[a, b])), ys@HasViews(_, Def(_: ListIso[_, _]))) =>
      implicit val eA = iso.innerIso.eFrom
      val xs1 = xs.asRep[List[a]]
      val ys1 = iso.from(ys.asRep[List[b]])
      ViewList(xs1 ::: ys1, iso.innerIso)

    case _ =>
      super.rewriteDef(d)
  }
}