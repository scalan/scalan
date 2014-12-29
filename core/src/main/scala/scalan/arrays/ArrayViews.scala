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
  //  implicit def mkArrayView[A,B](arr: PA[A])(implicit iso: Iso[A,B]): PA[B] = {
  //    ExpViewArray(Some(arr), iso)
  //  }
  //  case class ExpViewArray[A, B](arr: Option[PA[A]], iso: Iso[A, B])
  //    extends ViewArray[A,B] with StagedArrayBase[B]
  //  {
  //    override val elem = iso.eB
  //    implicit def eA: Elem[A] = iso.eA
  //    implicit def eB: Elem[B] = iso.eB
  //    implicit def mB: Manifest[B] = eB.manifest
  //    override def mirror(f: Transformer)(implicit ctx: SourceContext) = ExpViewArray(arr map { a => f(a) }, iso)
  //
  //    def arrOrEmpty: PA[A] = arr.isEmpty match { case true => iso.eA.empty case _ => arr.get }
  //
  //    override def apply(i: IntRep) = iso.to(arrOrEmpty(i))
  //    override def toArray = ???
  //    //     {
  //    //      val _a = arr.toArray
  //    //      val f = (i: IntRep) => iso.to(_a(i))
  //    //      toExp(ArrayTabulate(length, mkLambda(f)))
  //    //    }
  //    //    def map[R: Elem](f: Rep[B] => Rep[R]): PA[R] = {
  //    //      val len = length
  //    //      element[R].tabulate(len)(i => f(iso.to(arr(i))))
  //    //    }
  //
  //    override def slice(start: IntRep, len: IntRep) = ExpViewArray(Some(arrOrEmpty.slice(start, len)), iso)
  //
  //    override def flagCombine(ifFalse: PA[B], flags: PA[Boolean]) = ???
  //    //    ifFalse.asInstanceOf[ExpViewArray[A, B]] match {
  //    //      case ExpViewArray(falseA, iso) => ExpViewArray(ExpSome(arr.flagCombine(falseA.get, flags)), iso)
  //    //      case _ => !!!("ExpViewArray expected by was", ifFalse)
  //    //    }
  //
  //    // length(this) + length(ifFalse) == length(flags)
  //    override def flagMerge(ifFalse: PA[B], flags: PA[Boolean]) = ???
  //    //    ifFalse.matchType {
  //    //      (a: ExpViewArray[A, B]) => ExpViewArray(arr.flagMerge(a.arr, flags), iso)
  //    //    }
  //
  //    // length(this) == length(flags) == (length(A) + length(B))
  //    override def flagSplit(flags: PA[Boolean]) = ???
  //    //    {
  //    //      val Pair(at, af) = arr.flagSplit(flags)
  //    //      Pair(ExpViewArray(at, iso), ExpViewArray(af, iso))
  //    //    }
  //  }
  //
  //  val viewArrayOptimizations = new PartialRewriter({
  //    case Def(UnpackViewArray(Def(ExpViewArray(Some(arr), iso)))) => arr
  //  })

  def arrayIso[A, B](iso: Iso[A, B]): Iso[Array[A], Array[B]] = {
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    new Iso[Array[A], Array[B]] {
      lazy val eTo = element[Array[B]]
      def from(x: Arr[B]) = x.map(iso.from _)
      def to(x: Arr[A]) = x.map(iso.to _)
      lazy val tag = {
        implicit val tB = iso.tag
        weakTypeTag[Array[B]]
      }
      lazy val defaultRepTo = Default.defaultVal(toRep(scala.Array.empty[B](eB.classTag)))
    }
  }
  
  case class HasArg(predicate: Exp[_] => Boolean) {
    def unapply[T](d: Def[T]): Option[Def[T]] = {
      val args = dep(d)
      if (args.exists(predicate)) Some(d) else None
    }
  }

  val HasViewArg = HasArg(hasViewArg)

  protected def hasViewArg(s: Exp[_]): Boolean = s match {
    case Def(_: ViewArray[_, _]) => true
    case _ => false
  }

  //  object View {
  //    def unapply[T](e: Def[T]): Option[(ExpViewArray[Any,Any], Elem[Any], Elem[Any])] =
  //      e match {
  //        case view@ExpViewArray(_,_) => Some((view, view.eA, view.eB))
  //        case _ => None
  //      }
  //  }
  //
  //  def LiftedCallFromView[TRepr](clazzUT: Class[_], methodName: String) = new {
  //    def unapply[T](d: Def[T]): Option[PA[TRepr]] = {
  //      d match {
  //        case MethodCallLifted(Def(View(view, eA, eB)), m, List())
  //          if eB.manifest.erasure equals clazzUT =>
  //          m.getName == methodName match {
  //            case true => Some(view.arrOrEmpty.asPA[TRepr])
  //            case _ => None
  //          }
  //        case _ => None
  //      }
  //    }
  //  }
  //
  //  def liftViewFromArgsDefault[T:Elem]
  //  (d: Def[T])(mirrorDef: (MapTransformer, Iso[Any,_]) => Exp[_]): Rep[_] =
  //  {
  //    val args = dep(d)
  //    val view = args collect { case Def(View(v, eA, eB)) => v } head
  //
  //    val subst = args collect {
  //      a => a match { case Def(View(v, eA, eB)) => (a.asRep[Any], v.arrOrEmpty.asRep[Any]) }} toMap
  //
  //    val t = new MapTransformer(subst)
  //    val s1 = mirrorDef(t, view.iso)
  //    mkView(s1.asRep[PArray[Any]])(view.iso)
  //  }
  //
  def mapUnderlyingArray[A,B,C](view: ViewArray[A,B], f: Rep[B=>C]): Arr[C] = {
    val iso = view.innerIso
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    implicit val eC: Elem[C] = f.elem.eRange
    view.source.map { x => f(iso.to(x)) }
  }

  def filterUnderlyingArray[A, B](view: ViewArray[A, B], f: Rep[B => Boolean]): Arr[B] = {
    val iso = view.innerIso
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    val filtered = view.source.filter { x => f(iso.to(x)) }
    ViewArray(filtered)(iso)
  }
  
  def liftViewFromArgs[T](d: Def[T])/*(implicit eT: Elem[T])*/: Option[Exp[_]] = d match {
    case ArrayApply(Def(view: ViewArray[a, b]), i) =>
      implicit val eA = view.innerIso.eFrom
      implicit val eB = view.innerIso.eTo
      val res = view.innerIso.to(view.source(i))
      Some(res)
    case ArrayApplyMany(Def(view: ViewArray[a, b]), is) =>
      implicit val eA = view.innerIso.eFrom
      implicit val eB = view.innerIso.eTo
      val res = ViewArray(view.source(is))(view.innerIso)
      Some(res)
    case ArrayMap(Def(view: ViewArray[_, _]), f) =>
      Some(mapUnderlyingArray(view, f))
    case ArrayFilter(Def(view: ViewArray[_, _]), f) =>
      Some(filterUnderlyingArray(view, f))
  //    case PackPA(arr, flags) =>
  //      val res = liftViewFromArgsDefault(d) { (t,iso) =>
  //        implicit val eAny: Elem[Any] = iso.eA
  //        PackPA[Any](t(arr.asPA[Any]), t(flags))
  //      }
  //      Some(res)
  //    case ExpNestedArray(Def(view@ExpViewArray(_, iso)), segs) =>
  //      val nested = view.arrOrEmpty.nestBy(segs)
  //      val nestedIso = nestIso(iso)
  //      Some(mkView(nested)(nestedIso))
  //    case NestedArrayValues(Def(view@ExpViewArray(arr, iso))) if iso.isInstanceOf[NestedIso[_,_]] =>
  //      val nestedIso = iso.asInstanceOf[NestedIso[Any,Any]]
  //      implicit val eA: Elem[Any] = nestedIso.innerIso.eA
  //      val nested = view.arrOrEmpty.asNA[Any]
  //      Some(mkView(NestedArrayValues(nested))(nestedIso.innerIso))
  //    //      case ReplicateSegPA(c, Def(view@ExpViewArray(arr, iso))) => {
  //    //        val eA: Elem[Any] = view.eA
  //    //        implicit val eB = view.eB
  //    //        ExpViewArray(ReplicateSegPA(c, arr)(eA), iso)//(eA, eB, eB.manifest)
  //    //      }
  //    //      case ExpIfArray(_,_,_) => liftViewFromArgsDefault(d)
  //    //
  //    //      case FirstPA(_) => liftViewFromArgsDefault(d)
  //    //      case SecondPA(_) => liftViewFromArgsDefault(d)
  //    //
  //    //      case ExpPairArray(
  //    //      Def(v1@ExpViewArray(arr1, iso1)),
  //    //      Def(v2@ExpViewArray(arr2, iso2))) => {
  //    //        val pIso = pairIso(iso1, iso2)
  //    //        val arr = ExpPairArray(arr1, arr2)(iso1.eA, iso2.eA)
  //    //        implicit val eAB = pIso.eB
  //    //        ExpViewArray(arr, pIso)
  //    //      }
  //    //      case ExpPairArray(Def(v1@ExpViewArray(arr1, iso1)), arr2) => {
  //    //        val iso2 = identityIso(arr2.ArrayElem.ea)
  //    //        val pIso = pairIso(iso1, iso2)
  //    //        val arr = ExpPairArray(arr1, arr2)(iso1.eA, iso2.eA)
  //    //        implicit val eAB = pIso.eB
  //    //        ExpViewArray(arr, pIso)
  //    //      }
  //    //      case ExpPairArray(arr2, Def(v1@ExpViewArray(arr1, iso1))) => {
  //    //        val iso2 = identityIso(arr2.ArrayElem.ea)
  //    //        val pIso = pairIso(iso2, iso1)
  //    //        val arr = ExpPairArray(arr2, arr1)(iso2.eA, iso1.eA)
  //    //        implicit val eAB = pIso.eB
  //    //        ExpViewArray(arr, pIso)
  //    //      }
  //    //      case ExpSumArray(_,_,_) => null
  //    //case Def(d) => !!!("Don't know how to lift view from " + d)
    case _ => None
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    case ArrayLength(Def(ViewArray(arr: Arr[a] @unchecked))) =>
      array_length(arr)
    case HasViewArg(_) => liftViewFromArgs(d) match {
      case Some(s) => s
      case _ => super.rewriteDef(d)
    }
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
    case view1@ViewArray(Def(view2@ViewArray(arr))) =>
      val compIso = composeIso(view2.innerIso, view1.innerIso)
      implicit val eAB = compIso.eTo
      ViewArray(arr)(compIso)
    case _ =>
      super.rewriteDef(d)
  }
}