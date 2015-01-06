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
  case class ViewList[A, B](source: Lst[A])(implicit innerIso: Iso[A, B]) extends View1[A, B, List] {
    lazy val iso = listIso(innerIso)
    def copy(source: Lst[A]) = ViewList(source)
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
      Some((view.source, listIso(view.iso)))
    case UserTypeList(iso: Iso[a, b]) =>
      val newIso = listIso(iso)
      val repr = reifyObject(UnpackView(s.asRep[List[b]])(newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  case class ListIso[A:Elem,B:Elem](iso: Iso[A,B]) extends Iso[List[A], List[B]] {
    lazy val eTo = element[List[B]]
    def from(x: Lst[B]) = x.map(iso.from _)
    def to(x: Lst[A]) = x.map(iso.to _)
    lazy val tag = {
      implicit val tB = iso.tag
      weakTypeTag[List[B]]
    }
    lazy val defaultRepTo = Default.defaultVal(List.empty[B]/*toRep(scala.List.empty[B](eB.classTag))*/)
  }

  def listIso[A, B](iso: Iso[A, B]): Iso[List[A], List[B]] = {
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    ListIso(iso)
  }
  
  val HasViewListArg = HasArg(hasViewListArg)

  protected def hasViewListArg(s: Exp[_]): Boolean = s match {
    case Def(_: ViewList[_, _]) => true
    case _ => false
  }

  def mapUnderlyingList[A,B,C](view: ViewList[A,B], f: Rep[B=>C]): Lst[C] = {
    val iso = view.innerIso
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    implicit val eC: Elem[C] = f.elem.eRange
    view.source.map { x => f(iso.to(x)) }
  }

  def filterUnderlyingList[A, B](view: ViewList[A, B], f: Rep[B => Boolean]): Lst[B] = {
    val iso = view.innerIso
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    val filtered = view.source.filter { x => f(iso.to(x)) }
    ViewList(filtered)(iso)
  }
  
  def liftViewListFromArgs[T](d: Def[T])/*(implicit eT: Elem[T])*/: Option[Exp[_]] = d match {
    case ListMap(Def(view: ViewList[_, _]), f) =>
      Some(mapUnderlyingList(view, f))
    case ListFilter(Def(view: ViewList[_, _]), f) =>
      Some(filterUnderlyingList(view, f))
    case _ => None
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    case ListLength(Def(ViewList(arr: Lst[a] @unchecked))) =>
      list_length(arr)
    case HasViewListArg(_) => liftViewListFromArgs(d) match {
      case Some(s) => s
      case _ => super.rewriteDef(d)
    }
    case ListMap(xs: Lst[a] @unchecked, f@Def(Lambda(_, _, _, UnpackableExp(_, iso: Iso[c, b])))) =>
      val f1 = f.asRep[a => b]
      val xs1 = xs.asRep[List[a]]
      implicit val eA = xs1.elem.eItem
      implicit val eC = iso.eFrom
      val s = xs1.map { x =>
        val tmp = f1(x)
        iso.from(tmp)
      }
      val res = ViewList(s)(iso)
      res
    case view1@ViewList(Def(view2@ViewList(arr))) =>
      val compIso = composeIso(view2.innerIso, view1.innerIso)
      implicit val eAB = compIso.eTo
      ViewList(arr)(compIso)
    case _ =>
      super.rewriteDef(d)
  }
}