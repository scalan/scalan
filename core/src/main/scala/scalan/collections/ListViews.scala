/**
 * User: Alexander Slesarenko
 * Date: 11/23/13
 */
package scalan.collections

import scala.reflect.runtime.universe._
import scalan._
import scalan.common.Default
import scalan.staged.BaseExp

trait ListViews extends ListOps with Views { self: Scalan =>
}

trait ListViewsSeq extends ListViews with ListOpsSeq with ViewsSeq { self: ScalanSeq =>
}

trait ListViewsExp extends ListViews with ListOpsExp with ViewsExp with BaseExp { self: ScalanExp =>

  case class ViewList[A, B](source: Lst[A])(iso: Iso1[A, B, List]) extends View1[A, B, List](iso) {
    //lazy val iso = listIso(innerIso)
    def copy(source: Lst[A]) = ViewList(source)(iso)
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
      val newIso = ListIso(iso)
      val repr = reifyObject(UnpackView(s.asRep[List[b]])(newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  implicit val listContainer: Cont[List] = new Container[List] {
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[List[T]]
    def lift[T](implicit eT: Elem[T]) = element[List[T]]
  }

  case class ListIso[A,B](iso: Iso[A,B]) extends Iso1[A, B, List](iso) {
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    def from(x: Lst[B]) = x.map(iso.from _)
    def to(x: Lst[A]) = x.map(iso.to _)
    lazy val defaultRepTo = Default.defaultVal(SList.empty[B])
  }

//  def listIso[A, B](iso: Iso[A, B]): Iso[List[A], List[B]] = {
//    implicit val eA = iso.eFrom
//    implicit val eB = iso.eTo
//    ListIso(iso)
//  }
  
//  val HasViewListArg = HasArg(hasViewListArg)
//
//  protected def hasViewListArg(s: Exp[_]): Boolean = s match {
//    case Def(_: ViewList[_, _]) => true
//    case _ => false
//  }

//  def mapUnderlyingList[A,B,C](view: ViewList[A,B], f: Rep[B=>C]): Lst[C] = {
//    val iso = view.innerIso
//    implicit val eA = iso.eFrom
//    implicit val eB = iso.eTo
//    implicit val eC: Elem[C] = f.elem.eRange
//    view.source.map { x => f(iso.to(x)) }
//  }
//
//  def filterUnderlyingList[A, B](view: ViewList[A, B], f: Rep[B => Boolean]): Lst[B] = {
//    val iso = view.innerIso
//    implicit val eA = iso.eFrom
//    implicit val eB = iso.eTo
//    val filtered = view.source.filter { x => f(iso.to(x)) }
//    ViewList(filtered)(iso)
//  }
//
//  def liftViewListFromArgs[T](d: Def[T])/*(implicit eT: Elem[T])*/: Option[Exp[_]] = d match {
//    case ListMap(Def(view: ViewList[_, _]), f) =>
//      Some(mapUnderlyingList(view, f))
//    case ListFilter(Def(view: ViewList[_, _]), f) =>
//      Some(filterUnderlyingList(view, f))
//    case _ => None
//  }

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
        val res = ViewList(s)(ListIso(iso))
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
      case (xs: Lst[a]@unchecked, f@Def(Lambda(_, _, _, UnpackableExp(_, arrIso: ArrayIso[b, c])))) => {
        val f1 = f.asRep[a => Array[c]]
        val xs1 = xs.asRep[List[a]]
        implicit val eA = xs1.elem.eItem
        implicit val eC = arrIso.iso.eFrom

        val s = xs1.flatMap { x =>
          val tmp = f1(x)
          arrIso.from(tmp)
        }
        val res = ViewList(s)(ListIso(arrIso.iso))
        res
      }
      case (Def(view: ViewList[a, b]), _) => {
        val iso = view.innerIso
        val f = lm.f.asRep[b => Array[c]]
        implicit val eA = iso.eFrom
        implicit val eB = iso.eTo
        implicit val eAC: Elem[Array[c]] = f.elem.eRange
        implicit val eC = eAC.eItem
        view.source.flatMap { x => f(iso.to(x))}
      }
      case _ =>
        super.rewriteDef(d)
    }
    case ListFilter(Def(view: ViewList[a, b]), f) => {
      val iso = view.innerIso
      implicit val eA = iso.eFrom
      implicit val eB = iso.eTo
      val filtered = view.source.filter { x => f(iso.to(x))}
      ViewList(filtered)(ListIso(iso))
    }
    case view1@ViewList(Def(view2@ViewList(arr))) => {
      val compIso = composeIso(view2.innerIso, view1.innerIso)
      implicit val eAB = compIso.eTo
      ViewList(arr)(ListIso(compIso))
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
      ViewList(SList.replicate(len, v))(ListIso(iso))
    }
    case _ =>
      super.rewriteDef(d)
  }
}