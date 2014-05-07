/**
 * User: Alexander Slesarenko
 * Date: 11/23/13
 */
package scalan.arrays

import scalan._
import scalan.staged.BaseExp
import scala.reflect.runtime.universe._
import scalan.common.Default

trait ArrayViews extends Views { self: Scalan =>

//  trait ArrayView[A, B] extends PArray[B] {
//    def arr: Option[PA[A]]
//    def iso: Iso[A, B]
//    def arrOrEmpty: PA[A]
//  }
//
//  def mkArrayView[A,B](view: Arr[A])(implicit iso: Iso[A,B]): Arr[B]
//  def unmkArrayView[A,B](view: Arr[B])(implicit iso: Iso[A,B]): Arr[A]
}

trait ArrayViewsSeq extends ArrayViews { self: ScalanSeq =>

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

trait ArrayViewsExp extends ArrayViews with BaseExp { self: ScalanStaged =>

  case class ViewArray[From, To](arr: Arr[From])(implicit val iso: Iso[From, To]) extends ArrayDef[To] {
    implicit def eTo = iso.eTo
    val selfType = element[Array[To]]
    val uniqueOpId = name(arr.elem.ea, eTo)
    override def mirror(f: Transformer) = ViewArray(f(arr))
  }

//  case class UnpackView[A,B](view: Arr[B])(implicit val iso: Iso[A,B]) extends ArrayDef[A] {
//    implicit lazy val elem = iso.eFrom
//    val selfType = element[Array[A]]
//    val uniqueOpId = name(elem, view.elem)
//    override def mirror(f: Transformer) = UnpackView[A,B](f(view))
//  }
  //
  //  implicit def mkArrayView[A,B](arr: PA[A])(implicit iso: Iso[A,B]): PA[B] = {
  //    ExpViewArray(Some(arr), iso)
  //  }
  def unmkArrayView[A,B](view: Arr[B])(implicit iso: Iso[A,B]): Arr[A] = {
    view match {
      case Def(ViewArray(arr)) => arr.asRep[Array[A]]
      case _ =>
//        implicit val eA = iso.eFrom
//        UnpackView[A,B](view)
        !!!("UnpackView not defined yet")
    }
  }
  //
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
  //    case Def(UnpackView(Def(ExpViewArray(Some(arr), iso)))) => arr
  //  })

  def arrayIso[A, B](iso: Iso[A, B]): Iso[Array[A], Array[B]] = {
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    new IsoBase[Array[A], Array[B]] with StagedIso[Array[A], Array[B]] {
      lazy val eTo = element[Array[B]]
      override def from(x: Arr[B]) = x.map(fun(iso.from _))
      override def to(x: Arr[A]) = x.map(fun(iso.to _))
      lazy val tag = {
        implicit val tB = iso.tag
        typeTag[Array[B]]
      }
      lazy val defaultRepTo = Default.defaultVal(toRep(Array.empty[B](eB.classTag)))
    }
  }
  
  case class HasArg(predicate: Exp[_] => Boolean) {
    def unapply[T](d: Def[T]): Option[Def[T]] = {
      val args = dep(d)
      if (args.exists(predicate)) Some(d) else None
    }
  }

  val HasViewArg = HasArg {
    case Def(_: ViewArray[_, _]) => true
    case _ => false
  }
  //
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
    val iso = view.iso
    implicit val eA = view.arr.elem.ea
    implicit val eB = iso.eTo
    implicit val eC: Elem[C] = f.elem.eb
    view.arr.map(fun { (x: Exp[A]) => f(iso.to(x)) })
  }
  def filterUnderlyingArray[A, B](view: ViewArray[A, B], f: Rep[B => Boolean]): Arr[B] = {
    val iso = view.iso
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    val filtered = view.arr.filter(fun { (x: Exp[A]) => f(iso.to(x)) })
    ViewArray(filtered)(iso)
  }
  
  def liftViewFromArgs[T](d: Def[T])/*(implicit eT: Elem[T])*/: Option[Exp[_]] = d match {
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

  override def rewrite[T](d: Exp[T])(implicit eT: LElem[T]) = d match {
    case Def(d1) => d1 match {
      //    case MethodCall(Def(obj@UserTypeDef(_)), m, args) => {
      //      (m.getDeclaringClass.isAssignableFrom(obj.getClass) && invokeEnabled) match {
      //        case true =>
      //          val res = m.invoke(obj, args: _*)
      //          res.asInstanceOf[Exp[_]]
      //        case _ => super.rewrite(d)
      //      }
      //    }
      //    case LengthPA(Def(View(view, _, _))) => view.arrOrEmpty.length
      //    case UnpackView(Def(ExpViewArray(Some(arr), _))) => arr
      //    case d1@ExpViewArray(Some(Def(d2@UnpackView(view))), _) if d1.iso.getClass == d2.iso.getClass => view
      //
      //
      case HasViewArg(_) => liftViewFromArgs(d1) match {
        case Some(s) => s
        case _ => super.rewrite(d)
      }
      case ArrayMap(xs, f @ Def(Lambda(_, _, _, UserTypeSym(iso: Iso[a, b])))) =>
        val f1 = f.asInstanceOf[Rep[a => b]]
        val xs1 = xs.asRep[Array[a]]
        implicit val eA = xs1.elem.ea
        val s = xs1.map(fun { x =>
          val tmp = f1(x)
          iso.from(tmp)
        })
        val res = ViewArray(s)(iso)
        res
      case ArrayMap(xs: Arr[a], f@Def(lam@Lambda(_, _, _, Def(view: ViewArray[c, b])))) =>
        val f1 = f.asRep[a => Array[b]]
        val view1 = view.asInstanceOf[ViewArray[c, b]]
        val iso = view1.iso
        val xs1 = xs.asRep[Array[a]]
        implicit val eA = xs1.elem.ea
        implicit val eB = iso.eTo
        implicit val eC = iso.eFrom
        val s = xs1.map(fun { x =>
          unmkArrayView(f1(x))(iso)
        })
        val res = ViewArray(s)(arrayIso(iso))
        // val res = ViewArray(s.values)(iso).nestBy(s.segments)
        res
      //
      //    case LoopUntil(start@TupleTree(tree), step, isMatch) if tree.hasViews => {
      //      tree.eliminateViews.root match {
      //        case startTreeRoot: Rep[a] =>
      //          implicit val stateElem = startTreeRoot.Elem
      //          val loopRes = LoopUntil(
      //            startTreeRoot,
      //            fun { (x: Rep[a]) =>
      //              tree.toView(x) match {
      //                case x_viewed: Rep[a1] =>
      //                  implicit val eA1 = x_viewed.Elem
      //                  val res_viewed = mirrorApply(step.asRep[a1 => a1], x_viewed)
      //                  val res = tree.fromView(res_viewed)
      //                  res.asRep[a]
      //              }
      //            }(stateElem, stateElem),
      //            fun { (x: Rep[a]) =>
      //              tree.toView(x) match {
      //                case x_viewed: Rep[a1] =>
      //                  implicit val eA1 = x_viewed.Elem
      //                  val res = mirrorApply(isMatch.asRep[a1 => Boolean], x_viewed)
      //                  res
      //              }
      //            }(stateElem, element[Boolean]))
      //          tree.toView(loopRes)
      //      }
      //    }
      //
      ////    case ExpViewArray(Def(ExpViewArray(arr, iso1)), iso2) => {
      ////      val compIso = composeIso(iso2, iso1)
      ////      implicit val eAB = compIso.eB
      ////      ExpViewArray(arr, compIso)
      ////    }
      case _ =>
        super.rewrite(d)
    }
    case _ =>
      super.rewrite(d)
  }
//
//  //  val isoLifting = new PartialRewriter({
//  //    //case Def(ExpViewArray(Def(UnpackView(view, iso2)), iso1)) /*if iso1 == iso2*/ => view
//  //    //case ArrayFromView(Def(ExpViewArray(arr, iso1)), iso2) /*if iso1 == iso2*/ => arr
//  //
//  //    //    case ExpViewArray(Def(ExpViewArray(arr, iso1)), iso2) => {
//  //    //      val compIso = composeIso(iso2, iso1)
//  //    //      implicit val eAB = compIso.eB
//  //    //      ExpViewArray(arr, compIso)
//  //    //    }
//  //    //
//  //  })
//

}