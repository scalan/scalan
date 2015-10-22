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

trait ListViewsSeq extends ListViews with ListOpsSeq with ViewsSeq { self: ScalanSeq =>
}

trait ListViewsExp extends ListViews with ListOpsExp with ViewsExp with BaseExp { self: ScalanExp =>

  case class ViewList[A, B](source: Lst[A])(iso: Iso1[A, B, List]) extends View1[A, B, List](iso) {
    //lazy val iso = listIso(innerIso)
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
      val repr = defToRep(UnpackView(s.asRep[List[b]])(newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  override def rewriteDef[T](d: Def[T]) = d match {
    case ListLength(Def(ViewList(arr: Lst[a]@unchecked))) =>
      list_length(arr)

    case lm: ListMap[_,c] => (lm.xs, lm.f) match {
      case (xs: Lst[a]@unchecked, f@Def(Lambda(_, _, _, UnpackableExp(_, iso: Iso[b, c])))) => {
        val f1 = f.asRep[a => c]
        implicit val eA = xs.elem.eItem
        implicit val eB = iso.eFrom
        val s = xs.map { x =>
          val tmp = f1(x)
          iso.from(tmp)
        }
        val res = ViewList(s)(listIso(iso))
        res
      }
      case (Def(view: ViewList[a, b]), _) => {
        val iso = view.innerIso
        val ff = lm.f.asRep[b => c]
        implicit val eA = iso.eFrom
        implicit val eB = iso.eTo
        implicit val eC = ff.elem.eRange
        view.source.map { x => ff(iso.to(x))}
      }
      case _ =>
        super.rewriteDef(d)
    }
    case lm: ListFlatMap[_,c] => (lm.xs, lm.f) match {
      case (xs: Lst[a]@unchecked, f@Def(Lambda(_, _, _, UnpackableExp(_, listIso: ListIso[b, c])))) => {
        val f1 = f.asRep[a => List[c]]
        val xs1 = xs.asRep[List[a]]
        implicit val eA = xs1.elem.eItem
        implicit val eC = listIso.iso.eFrom

        val s = xs1.flatMap { x =>
          val tmp = f1(x)
          listIso.from(tmp)
        }
        val res = ViewList(s)(listIso)
        res
      }
      case (Def(view: ViewList[a, b]), _) => {
        val iso = view.innerIso
        val f = lm.f.asRep[b => List[c]]
        implicit val eA = iso.eFrom
        implicit val eB = iso.eTo
        implicit val eAC: Elem[List[c]] = f.elem.eRange
        implicit val eC = eAC.eItem
        view.source.flatMap { x => f(iso.to(x)) }
      }
      case _ =>
        super.rewriteDef(d)
    }
    case ListFilter(Def(view: ViewList[a, b]), f) => {
      val iso = view.innerIso
      implicit val eA = iso.eFrom
      implicit val eB = iso.eTo
      val filtered = view.source.filter { x => f(iso.to(x))}
      ViewList(filtered)(listIso(iso))
    }
    case view1@ViewList(Def(view2@ViewList(arr))) => {
      val compIso = composeIso(view1.innerIso, view2.innerIso)
      implicit val eAB = compIso.eTo
      ViewList(arr)(listIso(compIso))
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
          iso.from(step(Pair(iso.to(p._1), p._2)))})
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
          iso.from(step(Pair(p._1, iso.to(p._2))))})
        iso.to(res)
      }
      case _ =>
        super.rewriteDef(d)
    }
    case ListReplicate(len, HasViews(valueWithoutView, iso: Iso[a, b] @unchecked)) => {
      val v = valueWithoutView.asRep[a]
      implicit val eA = iso.eFrom
      implicit val eB = iso.eTo
      ViewList(SList.replicate(len, v))(listIso(iso))
    }
    case _ =>
      super.rewriteDef(d)
  }
}