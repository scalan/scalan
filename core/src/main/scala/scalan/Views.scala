package scalan

import java.lang.reflect.InvocationTargetException

import scalan.common.Default
import scala.language.higherKinds
import scalan.common.Lazy
import scala.reflect.runtime.universe._
import scalan.staged.BaseExp

trait Views extends Elems { self: Scalan =>
  trait Convertable[A] {
    def convert(x: Rep[Reifiable[_]]): Rep[A] = !!!("should not be called")
  }
  abstract class EntityElem[A] extends Elem[A] with Convertable[A] {
  }
  abstract class EntityElem1[A, To, C[_]](val eItem: Elem[A], val cont: Cont[C])
    extends Elem[To] with Convertable[To] {
  }

  // eFrom0 is used to avoid making eFrom implicit in subtypes
  // and support recursive types
  abstract class Iso[From, To](implicit eFrom0: Elem[From]) {
    def eFrom: Elem[From] = eFrom0
    def eTo: Elem[To]
    def tag: WeakTypeTag[To]
    def defaultRepTo: Default[Rep[To]]
    def from(p: Rep[To]): Rep[From]
    def to(p: Rep[From]): Rep[To]
    override def toString = s"${eFrom.name} <-> ${eTo.name}"
    override def equals(other: Any) = other match {
      case i: Iso[_, _] => eFrom == i.eFrom && eTo == i.eTo
      case _ => false
    }
  }

  abstract class Iso1[A, B, C[_]](val innerIso: Iso[A,B])(implicit cC: Cont[C])
    extends Iso[C[A], C[B]]()(cC.lift(innerIso.eFrom)) {
    lazy val eTo = cC.lift(innerIso.eTo)
    lazy val tag = cC.tag(innerIso.tag)
  }

  implicit def viewElement[From, To](implicit iso: Iso[From, To]): Elem[To] = iso.eTo // always ask elem from Iso

  trait ViewElem[From, To] extends Elem[To] {
    def iso: Iso[From, To]
    override def isEntityType = shouldUnpack(this)
    def tag: WeakTypeTag[To] = iso.tag
    protected def getDefaultRep = iso.defaultRepTo.value
  }

  object ViewElem {
    def unapply[From, To](ve: ViewElem[From, To]): Option[Iso[From, To]] = Some(ve.iso)
  }

  trait ViewElem1[A,From,To,C[_]]
    extends ViewElem[From, To] {
    def eItem: Elem[A]
    def cont: Cont[C]
  }

  object UnpackableElem {
    def unapply(e: ViewElem[_, _]) =
      if (shouldUnpack(e)) Some(e.iso) else None
  }

  def shouldUnpack(e: ViewElem[_, _]): Boolean

  trait CompanionElem[T] extends Elem[T] {
    override def isEntityType = false
  }

  trait TypeFamily1[F[_]] {
    def defaultOf[A](implicit ea: Elem[A]): Default[Rep[F[A]]]
  }
  trait TypeFamily2[F[_, _]] {
    def defaultOf[A, B](implicit ea: Elem[A], eb: Elem[B]): Default[Rep[F[A, B]]]
  }
  trait TypeFamily3[F[_, _, _]] {
    def defaultOf[A, B, C](implicit ea: Elem[A], eb: Elem[B], ec: Elem[C]): Default[Rep[F[A, B, C]]]
  }

  trait ConcreteClass0[C] {
    def defaultOf: Default[Rep[C]]
  }
  trait ConcreteClass1[C[_]] {
    def defaultOf[A](implicit ea: Elem[A]): Default[Rep[C[A]]]
  }
  trait ConcreteClass2[C[_, _]] {
    def defaultOf[A, B](implicit ea: Elem[A], eb: Elem[B]): Default[Rep[C[A, B]]]
  }
  trait ConcreteClass3[T[_, _, _]] {
    def defaultOf[A, B, C](implicit ea: Elem[A], eb: Elem[B], ec: Elem[C]): Default[Rep[T[A, B, C]]]
  }
  trait ConcreteClass4[T[_, _, _, _]] {
    def defaultOf[A, B, C, D](implicit ea: Elem[A], eb: Elem[B], ec: Elem[C], ed : Elem[D]): Default[Rep[T[A, B, C, D]]]
  }

  def identityIso[A](implicit elem: Elem[A]): Iso[A, A] =
    new Iso[A, A] {
      def eTo = elem
      def tag = elem.tag
      lazy val defaultRepTo = Default.defaultVal(elem.defaultRepValue)
      def from(x: Rep[A]) = x
      def to(x: Rep[A]) = x
    }

  def pairIso[A1, B1, A2, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2]): Iso[(A1, A2), (B1, B2)] = {
    implicit val eA1 = iso1.eFrom
    implicit val eA2 = iso2.eFrom
    implicit val eB1 = iso1.eTo
    implicit val eB2 = iso2.eTo
    val eBB = element[(B1, B2)]
    new Iso[(A1, A2), (B1, B2)] {
      def eTo = eBB
      var fromCacheKey:Option[Rep[(B1,B2)]] = None
      var fromCacheValue:Option[Rep[(A1,A2)]] = None
      var toCacheKey:Option[Rep[(A1,A2)]] = None
      var toCacheValue:Option[Rep[(B1,B2)]] = None

      def from(b: Rep[(B1, B2)]) = {
        if (fromCacheKey.isEmpty || b != fromCacheKey.get) {
          fromCacheKey = Some(b)
          fromCacheValue = Some((iso1.from(b._1), iso2.from(b._2)))
        }
        fromCacheValue.get
      }
      def to(a: Rep[(A1, A2)]) = {
        if (toCacheKey.isEmpty || a != toCacheKey.get) {
          toCacheKey = Some(a)
          toCacheValue = Some((iso1.to(a._1), iso2.to(a._2)))
        }
        toCacheValue.get
      }
      def tag = eBB.tag
      lazy val defaultRepTo = Default.defaultVal(eBB.defaultRepValue)
    }
  }

  def sumIso[A1, B1, A2, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2]): Iso[A1 | A2, B1 | B2] = {
    implicit val eA1 = iso1.eFrom
    implicit val eA2 = iso2.eFrom
    implicit val eB1 = iso1.eTo
    implicit val eB2 = iso2.eTo
    val eBB = element[B1 | B2]
    new Iso[A1 | A2, B1 | B2] {
      def eTo = eBB
      def from(b: Rep[B1 | B2]) =
        b.fold(b1 => toLeftSum(iso1.from(b1))(eA2),
               b2 => toRightSum(iso2.from(b2))(eA1))
      def to(a: Rep[A1 | A2]) =
        a.fold(a1 => toLeftSum(iso1.to(a1))(eB2),
               a2 => toRightSum(iso2.to(a2))(eB1))
      def tag = eBB.tag
      lazy val defaultRepTo = Default.defaultVal(eBB.defaultRepValue)
    }
  }

  def composeIso[A, B, C](iso2: Iso[B, C], iso1: Iso[A, B]): Iso[A, C] = {
    new Iso[A, C]()(iso1.eFrom) {
      def eTo = iso2.eTo
      def from(c: Rep[C]) = iso1.from(iso2.from(c))
      def to(a: Rep[A]) = iso2.to(iso1.to(a))
      def tag = iso2.tag
      def defaultRepTo = iso2.defaultRepTo
    }
  }

  def funcIso[A, B, C, D](iso1: Iso[A, B], iso2: Iso[C, D]): Iso[A => C, B => D] = {
    implicit val eA = iso1.eFrom
    implicit val eB = iso1.eTo
    implicit val eC = iso2.eFrom
    implicit val eD = iso2.eTo
    new Iso[A => C, B => D] {
      lazy val eTo = funcElement(eB, eD)
      def from(f: Rep[B => D]): Rep[A => C] = {
        fun { b => iso2.from(f(iso1.to(b))) }
      }
      def to(f: Rep[A => C]): Rep[B => D] = {
        fun { a => iso2.to(f(iso1.from(a))) }
      }
      def tag = eTo.tag
      lazy val defaultRepTo = Default.defaultVal(eTo.defaultRepValue)
    }
  }

  implicit class RepReifiableViewOps[T <: Reifiable[_]](x: Rep[T]) {
    def convertTo[R <: Reifiable[_]: Elem]: Rep[R] = repReifiable_convertTo[T,R](x)
  }

  def repReifiable_convertTo[T <: Reifiable[_], R <: Reifiable[_]]
                            (x: Rep[T])(implicit eR: Elem[R]): Rep[R] = {
    eR match {
      case entE: EntityElem[R] @unchecked => entE.convert(x)
      case _ => !!!(s"Cannot convert $x to a value of type ${eR.name}: ViewElem expected but ${eR.tag} found")
    }
  }
}

trait ViewsSeq extends Views { self: ScalanSeq =>
  trait UserTypeSeq[T, TImpl <: T] extends Reifiable[T] { thisType: T =>
    def self = this
  }

  def shouldUnpack(e: ViewElem[_, _]) = true
}

trait ViewsExp extends Views with BaseExp { self: ScalanExp =>
  case class MethodCallFromExp(clazzUT: Class[_], methodName: String) {
    def unapply[T](d: Def[T]): Option[(Exp[_], List[Exp[_]])] = d match {
      case MethodCall(obj, m, args, _) if m.getName == methodName =>
        Some((obj, args.asInstanceOf[List[Exp[_]]]))
      case _ => None
    }
  }

  type Unpacked[T] = (Rep[Source], Iso[Source, T]) forSome { type Source }

  type UnpackTester = Element[_] => Boolean

  private var unpackTesters: Set[UnpackTester] = Set.empty

  def addUnpackTester(tester: UnpackTester): Unit =
    unpackTesters += tester
  def removeUnpackTester(tester: UnpackTester): Unit =
    unpackTesters -= tester

  def shouldUnpack(e: ViewElem[_, _]) = unpackTesters.exists(_(e))

  trait UserTypeDef[T, TImpl <: T] extends ReifiableExp[T, TImpl] {
    def uniqueOpId = selfType.name
  }

  object HasViews {
    def unapply[T](s: Exp[T]): Option[Unpacked[T]] = unapplyViews(s)
  }

  // for simplifying unapplyViews
  protected def trivialView[T](s: Exp[T]) = (s, identityIso(s.elem))

  def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(Tup(s1, s2)) =>
      (unapplyViews(s1), unapplyViews(s2)) match {
        case (None, None) => None
        case (opt1, opt2) =>
          val (sv1, iso1) = opt1.getOrElse(trivialView(s1))
          val (sv2, iso2) = opt2.getOrElse(trivialView(s2))
          Some((Pair(sv1, sv2), pairIso(iso1, iso2)))
      }
    case _ =>
      UnpackableExp.unapply(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  object UnpackableDef {
    def unapply[T](d: Def[T]): Option[Unpacked[T]] =
      d match {
        case view: View[a, T] => Some((view.source, view.iso))
        // TODO make UserTypeDef extend View with lazy iso/source?
        case _ =>
          val eT = d.selfType
          eT match {
            case UnpackableElem(iso: Iso[a, T @unchecked]) =>
              Some((iso.from(d.self), iso))
            case _ => None
          }
      }
  }

  object UnpackableExp {
    def unapply[T](e: Exp[T]): Option[Unpacked[T]] =
      e match {
        case Def(d) => UnpackableDef.unapply(d)
        case _ =>
          val eT = e.elem
          eT match {
            case UnpackableElem(iso: Iso[a, T @unchecked]) =>
              Some((iso.from(e), iso))
            case _ => None
          }
      }
  }

  abstract class View[From, To] extends Def[To] {
    def source: Rep[From]
    def iso: Iso[From, To]
    implicit def selfType = iso.eTo
    def copy(source: Rep[From]): View[From, To]
    def mirror(t: Transformer) = copy(t(source))
    lazy val uniqueOpId = name(iso.eFrom, iso.eTo)
  }

  case class UnpackView[A, B](view: Rep[B])(implicit iso: Iso[A, B]) extends Def[A] {
    implicit def selfType = iso.eFrom
    override def mirror(f: Transformer) = UnpackView[A, B](f(view))
    lazy val uniqueOpId = name(selfType, view.elem)
  }

  abstract class View1[A, B, C[_]](val iso: Iso1[A,B,C]) extends View[C[A], C[B]] {
    def innerIso = iso.innerIso
  }

  abstract class View2[A1, A2, B1, B2, C[_, _]](implicit val iso1: Iso[A1, B1], val iso2: Iso[A2, B2]) extends View[C[A1, A2], C[B1, B2]]

  //  type Identity[T] = T
  //
  //  case class ViewVar[A, B](source: Rep[A])(implicit innerIso: Iso[A, B]) extends View1[A, B, Identity] {
  //    def iso = innerIso
  //    def copy(source: Rep[A]) = ViewVar(source)
  //  }

  case class PairView[A1, A2, B1, B2](source: Rep[(A1, A2)])(implicit iso1: Iso[A1, B1], iso2: Iso[A2, B2]) extends View2[A1, A2, B1, B2, Tuple2] {
    lazy val iso = pairIso(iso1, iso2)
    def copy(source: Rep[(A1, A2)]) = PairView(source)
  }

  case class SumView[A1, A2, B1, B2](source: Rep[A1|A2])(implicit iso1: Iso[A1, B1], iso2: Iso[A2, B2]) extends View2[A1, A2, B1, B2, | ] {
    lazy val iso = sumIso(iso1, iso2)
    def copy(source: Rep[A1|A2]) = SumView(source)
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    //      case ViewPair(Def(ViewPair(a, iso1)), iso2) =>
    //        ViewPair(a, composeIso(iso2, iso1))
    case Tup(Def(UnpackableDef(a, iso1: Iso[a, c])), Def(UnpackableDef(b, iso2: Iso[b, d]))) =>
      PairView((a.asRep[a], b.asRep[b]))(iso1, iso2)
    case Tup(Def(UnpackableDef(a, iso1: Iso[a, c])), b: Rep[b]) =>
      PairView((a.asRep[a], b))(iso1, identityIso(b.elem)).self
    case Tup(a: Rep[a], Def(UnpackableDef(b, iso2: Iso[b, d]))) =>
      PairView((a, b.asRep[b]))(identityIso(a.elem), iso2).self

    case block@Semicolon(Def(UnpackableDef(a, iso1: Iso[a, c])), Def(UnpackableDef(b, iso2: Iso[b, d]))) => iso2.to(Semicolon(a.asRep[a], b.asRep[b])(iso2.eFrom))
    case block@Semicolon(a: Rep[a], Def(UnpackableDef(b, iso2: Iso[b, d]))) => iso2.to(Semicolon(a, b.asRep[b])(iso2.eFrom))
    case block@Semicolon(Def(UnpackableDef(a, iso1: Iso[a, c])), b: Rep[b]) => Semicolon(a.asRep[a], b)(block.selfType.asElem[b])

    case First(Def(view@PairView(source))) =>
      view.iso1.to(source._1)
    case Second(Def(view@PairView(source))) =>
      view.iso2.to(source._2)

    case l @ Left(Def(UnpackableDef(a, iso: Iso[a1, b1]))) =>
      SumView(toLeftSum(a.asRep[a1])(l.eB))(iso, identityIso(l.eB)).self
    case r @ Right(Def(UnpackableDef(a, iso: Iso[a1, b1]))) =>
      SumView(toRightSum(a.asRep[a1])(r.eA))(identityIso(r.eA), iso).self

    // case UnpackableDef(Def(uv @ UnpackView(view)), iso) if iso.eTo == view.iso.eTo => view
    case UnpackView(Def(UnpackableDef(source, iso))) => source
    // case UnpackView(view @ UnpackableExp(iso)) => iso.from(view)

    case ParallelExecute(nJobs:Rep[Int], f@Def(Lambda(_, _, _, UnpackableExp(_, iso: Iso[a, b])))) => {
      val parRes = ParallelExecute(nJobs, fun { i => iso.from(f(i)) })(iso.eFrom)
      ViewArray(parRes)(ArrayIso(iso))
    }

    case ArrayFold(xs: Rep[Array[t]] @unchecked, HasViews(init, iso: Iso[a, b]), step) =>
      val init1 = init.asRep[a]
      implicit val eT = xs.elem.asElem[Array[t]].eItem
      implicit val eA = iso.eFrom
      implicit val eB = iso.eTo
      val step1 = fun { (p: Rep[(a,t)]) =>
        val x_viewed = (iso.to(p._1), p._2)
        val res_viewed = mirrorApply(step.asRep[((b,t)) => b], x_viewed)
        val res = iso.from(res_viewed)
        res
      }
      val foldRes = ArrayFold(xs, init1, step1)
      iso.to(foldRes)
    case LoopUntil(HasViews(startWithoutViews, iso: Iso[a, b]), step, isMatch) =>
      val start1 = startWithoutViews.asRep[a]
      implicit val eA = iso.eFrom
      implicit val eB = iso.eTo
      val step1 = fun { (x: Rep[a]) =>
        val x_viewed = iso.to(x)
        val res_viewed = step.asRep[b => b](x_viewed) // mirrorApply(step.asRep[b => b], x_viewed)
        val res = iso.from(res_viewed)
        res
      }
      val isMatch1 = fun { (x: Rep[a]) =>
        val x_viewed = iso.to(x)
        val res = isMatch.asRep[b => Boolean](x_viewed) // mirrorApply(isMatch.asRep[b => Boolean], x_viewed)
        res
      }
      val loopRes = LoopUntil(start1, step1, isMatch1)
      iso.to(loopRes)
    case call @ MethodCall(Def(obj), m, args, neverInvoke) =>
      call.tryInvoke match {
        case InvokeSuccess(res) => res
        case InvokeFailure(_) => super.rewriteDef(d)
        case InvokeImpossible =>
          call.selfType match {
            case resultElem: Elem[r] =>
              // asRep[r] cast below should be safe
              // explicit resultElem to make sure both branches have the same type
              def copyMethodCall(newReceiver: Exp[_]) =
                mkMethodCall(newReceiver, m, args, neverInvoke, resultElem).asRep[r]

              obj match {
                case foldD: SumFold[a,b,_] =>
                  val res = foldD.sum.fold (
                    a => copyMethodCall(foldD.left(a)),
                    b => copyMethodCall(foldD.right(b))
                  )(resultElem)
                  res.asInstanceOf[Exp[_]]
                case IfThenElse(cond, t, e) =>
                  implicit val elem: Elem[r] = resultElem
                  IF (cond) {
                    copyMethodCall(t)
                  } ELSE {
                    copyMethodCall(e)
                  }
                case _ =>
                  super.rewriteDef(d)
              }
          }
      }
    case _ => super.rewriteDef(d)
  }

//  override def rewriteVar[T](v: Exp[T]) = v.elem match {
//    case UnpackableElem(iso: Iso[a, T @unchecked]) =>
//      iso.to(fresh[a](Lazy(iso.eFrom)))
//    case _ => super.rewriteVar(v)
//  }
}
