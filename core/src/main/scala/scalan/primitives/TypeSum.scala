package scalan.primitives

import scalan.staged.BaseExp
import scalan.{ ScalanExp, Scalan, ScalanSeq }
import scalan.common.Lazy

trait TypeSum { self: Scalan =>

  trait SumOps[A, B] {
    //def eA: Elem[A]
    //def eB: Elem[B]
    def isLeft: Rep[Boolean]
    def isRight: Rep[Boolean]
    def fold[R: Elem](l: Rep[A] => Rep[R], r: Rep[B] => Rep[R]): Rep[R]
    def mapSum[C:Elem,D:Elem](fl: Rep[A] => Rep[C], fr: Rep[B] => Rep[D]): Rep[C|D] =
      fold(l => toLeftSum[C,D](fl(l)), r => toRightSum[C,D](fr(r)))
  }

  implicit def pimpSum[A, B](s: Rep[(A | B)]): SumOps[A, B]

  def toLeft[A](a: Rep[A]): Rep[A | Unit] = toLeftSum[A, Unit](a)

  def toRight[A](a: Rep[A]): Rep[Unit | A] = toRightSum[Unit, A](a)

  def toLeftSum[A, B: Elem](a: Rep[A]): Rep[(A | B)]

  def toRightSum[A: Elem, B](a: Rep[B]): Rep[(A | B)]

  implicit class RepExtensionsForSum[A](x: Rep[A]) {
    def asLeft[B:Elem]: Rep[A | B] = toLeftSum[A,B](x)
    def asRight[B:Elem]: Rep[B | A] = toRightSum[B,A](x)
  }

  implicit class JoinSumOps[A:Elem](sum: Rep[A|A]) {
    def joinSum: Rep[A] = sum.fold(a => a, a => a)
  }
  implicit class OptionOps[A:Elem](opt: Rep[Unit|A]) {
    def map[B:Elem](f: Rep[A] => Rep[B]): Rep[Unit | B] =
      opt.mapSum(identity, f)
    def flatMap[B: Elem](f: Rep[A] => Rep[Unit | B]): Rep[Unit | B] =
      opt.fold(_ => toLeftSum[Unit, B](()), f)
    def getOrElse[B >: A : Elem](default: Rep[B]): Rep[B] =
      opt.fold(l => default, r => r)
  }
}

trait TypeSumSeq extends TypeSum { self: ScalanSeq =>

  def toLeftSum[A, B: Elem](a: Rep[A]): Rep[(A | B)] = Left[A, B](a)

  def toRightSum[A: Elem, B](a: Rep[B]): Rep[(A | B)] = Right[A, B](a)

  class SeqSumOps[A, B](s: Rep[(A | B)]) extends SumOps[A, B] {
    //def eA = element[A]; def eB = element[B]
    def fold[R: Elem](l: Rep[A] => Rep[R], r: Rep[B] => Rep[R]): Rep[R] = s.fold(l, r)
    def isLeft = s.isLeft
    def isRight = s.isRight
  }
  implicit def pimpSum[A, B](s: Rep[(A | B)]): SumOps[A, B] = new SeqSumOps[A, B](s)

}

trait TypeSumExp extends TypeSum with BaseExp { self: ScalanExp =>

  case class Left[A, B](left: Exp[A])(implicit val eRight: Elem[B]) extends BaseDef[(A | B)]()(sumElement(left.elem, eRight)) {
    override def mirror(t: Transformer) = Left[A, B](t(left))
    lazy val uniqueOpId = name(selfType)
  }

  case class Right[A, B](right: Exp[B])(implicit val eLeft: Elem[A]) extends BaseDef[(A | B)]()(sumElement(eLeft, right.elem)) {
    override def mirror(t: Transformer) = Right[A, B](t(right))
    lazy val uniqueOpId = name(selfType)
  }

  def toLeftSum[A, B: Elem](a: Rep[A]): Rep[(A | B)] = withElemOf(a) { implicit e => Left[A, B](a) }
  def toRightSum[A: Elem, B](b: Rep[B]): Rep[(A | B)] = withElemOf(b) { implicit e => Right[A, B](b) }

  case class IsLeft[A, B](sum: Exp[(A | B)]) extends BaseDef[Boolean] {
    override def mirror(t: Transformer) = IsLeft(t(sum))
    // removing leads to compilation error
    override val selfType = BoolElement
    lazy val uniqueOpId = name(sum.elem.eLeft, sum.elem.eRight)
  }

  case class IsRight[A, B](sum: Exp[(A | B)]) extends BaseDef[Boolean] {
    override def mirror(t: Transformer) = IsRight(t(sum))
    // removing leads to compilation error
    override val selfType = BoolElement
    lazy val uniqueOpId = name(sum.elem.eLeft, sum.elem.eRight)
  }

  case class SumFold[A, B, R](sum: Exp[(A | B)], left: Exp[A => R], right: Exp[B => R])(implicit selfType: Elem[R]) extends BaseDef[R] {
    override def mirror(t: Transformer) = SumFold(t(sum), t(left), t(right))
    lazy val eA = sum.elem.eLeft
    lazy val eB = sum.elem.eRight
    lazy val uniqueOpId = name(eA, eB)
  }

  case class SumMap[A, B, C, D](sum: Exp[(A | B)], left: Exp[A => C], right: Exp[B => D]) extends BaseDef[(C | D)]()(sumElement(left.elem.eRange, right.elem.eRange)) {
    override def mirror(t: Transformer) = SumMap(t(sum), t(left), t(right))
    lazy val eA = sum.elem.eLeft
    lazy val eB = sum.elem.eRight
    lazy val uniqueOpId = name(left.elem, right.elem)
  }

  class SumOpsExp[A, B](s: Rep[(A | B)]) extends SumOps[A, B] {
    implicit def eA: Elem[A] = s.elem.eLeft
    implicit def eB: Elem[B] = s.elem.eRight
    def fold[R: Elem](l: Rep[A] => Rep[R], r: Rep[B] => Rep[R]): Rep[R] = SumFold(s, fun(l), fun(r))
    def map[C,D](l: Rep[A] => Rep[C], r: Rep[B] => Rep[D]): Rep[C | D] = SumMap(s, fun(l), fun(r))
    def isLeft = IsLeft(s)
    def isRight = IsRight(s)
  }
  implicit def pimpSum[A, B](s: Rep[(A | B)]): SumOps[A, B] = new SumOpsExp[A, B](s)

  object IsJoinSum {
    def unapply[T](d: Def[T]): Option[Rep[Source] forSome { type Source }] = d match {
      case SumFold(source, Def(Lambda(l,_,_,_)), Def(Lambda(r,_,_,_))) if l.isIdentity && r.isIdentity => Some(source)
      case _ => None
    }
  }

  def liftFromSumFold[T1,T2,A,B](
        sum: Rep[T1|T2], left: Rep[T1 => B], right: Rep[T2 => B], iso: Iso[A,B]): Rep[B] = {
    implicit val eA = iso.eFrom
    val l1 = { l: Rep[T1] => iso.from(left(l))}
    val r1 = { r: Rep[T2] => iso.from(right(r))}
    val res = sum.fold(l1, r1)
    iso.to(res)
  }

//  def liftFromNestedSumFold[B1,B2,T1,T2,A,B](
//         outer: SumFold[B1,B2,B], inner: SumFold[T1,T2,(B1|B2)], iso: Iso[A,(B1|B2)]): Rep[B] = {
//    implicit val eA = iso.eFrom
//    val l1 = { l: Rep[T1] => iso.from(left(l))}
//    val r1 = { r: Rep[T2] => iso.from(right(r))}
//    iso.to(sum.fold(l1, r1))
//  }

  def liftFromSumFold[T1,T2,A,B,C,D](
        sum: Rep[T1|T2], left: Rep[T1 => C], right: Rep[T2 => D],
        iso1: Iso[A,C], iso2: Iso[B,D],
        toD: Conv[C,D], toC: Conv[D,C]): Rep[C] = {
    implicit val eA = iso1.eFrom
    val l1 = { l: Rep[T1] => iso1.from(left(l))}
    val r1 = { r: Rep[T2] => iso1.from(toC(right(r)))}
    iso1.to(sum.fold(l1, r1))
  }

  val FindFoldArg = FindArg(a => a match {
    case Def(_: SumFold[_,_,_]) => true
    case _ => false
  })

  override def rewriteDef[T](d: Def[T]) = d match {
    case SumFold(sum, Def(Lambda(_, _, _, l)), Def(Lambda(_, _, _, r))) if l == r =>
      l

    // Rule: fold(s, l, r)._1 ==> fold(s, x => l(x)._1, y => r(y)._1)
    case First(Def(foldD: SumFold[a, b, (T, r2)]@unchecked)) =>
      foldD.sum.fold(a => foldD.left(a)._1, b => foldD.right(b)._1)(d.selfType)

    // Rule: fold(s, l, r)._2 ==> fold(s, x => l(x)._2, y => r(y)._2)
    case Second(Def(foldD: SumFold[a, b, (r1, T)]@unchecked)) =>
      foldD.sum.fold(a => foldD.left(a)._2, b => foldD.right(b)._2)(d.selfType)

    // Rule: Left(V(a, iso)) ==> V(Left(a), SumIso(iso, identityIso))
    case l@Left(Def(UnpackableDef(a, iso: Iso[a1, b1]))) =>
      SumView(toLeftSum(a.asRep[a1])(l.eRight))(iso, identityIso(l.eRight)).self

    // Rule: Right(V(a, iso)) ==> V(Right(a), SumIso(identityIso, iso))
    case r@Right(Def(UnpackableDef(a, iso: Iso[a1, b1]))) =>
      SumView(toRightSum(a.asRep[a1])(r.eLeft))(identityIso(r.eLeft), iso).self

    case foldD @ SumFold(sum,
      LambdaResultHasViews(left,  iso1: Iso[a, c]),
      LambdaResultHasViews(right, iso2: Iso[_, _])) if iso1 == iso2 =>
    {
      val newFold = liftFromSumFold(sum, left, right, iso1)
      newFold
    }

    case foldD: SumFold[a, b, T] => foldD.sum match {
      // this rule is only applied when result of fold is base type.
      // Otherwise it yields stack overflow as this rule is mutually recursive with lifting Views over IfThenElse
      case Def(IfThenElse(p, thenp: Rep[Either[_, _]]@unchecked, elsep: Rep[Either[_, _]]@unchecked)) if !d.selfType.isEntityType =>
        __ifThenElse[T](p, SumFold(thenp, foldD.left, foldD.right)(foldD.selfType), SumFold(elsep, foldD.left, foldD.right)(foldD.selfType))

      case Def(view: SumView[a1, a2, b1, b2]) /*if !d.selfType.isEntityType*/ =>
        view.source.fold(x => foldD.left.asRep[b1 => T](view.iso1.to(x)), y => foldD.right.asRep[b2 => T](view.iso2.to(y)))(d.selfType)

      case Def(join@IsJoinSum(sum)) =>
        val source = sum.asRep[(a | b) | (a | b)]
        implicit val eT = foldD.selfType
        source.fold(
          x => x.fold(a => foldD.left(a), b => foldD.right(b)),
          y => y.fold(a => foldD.left(a), b => foldD.right(b)))

      case Def(Left(left: Rep[a])) =>
        implicit val eLeft = left.elem
        foldD.left(left)

      case Def(Right(right: Rep[a])) =>
        implicit val eRight = right.elem
        foldD.right(right)

      case _ => super.rewriteDef(d)

      //      case _ => (foldD.left, foldD.right) match {
      //        case (Def(Lambda(left:  Lambda[t1,c], _, _, HasViews(a1, iso1: Iso[a1, _]))),
      //              Def(Lambda(right: Lambda[t2,d], _, _, HasViews(b1, iso2: Iso[b1, _])))) =>
      //          (iso1.eTo, iso2.eTo) match {
      //            case IsConvertible(cTo, cFrom) =>
      //              liftFromSumFold(foldD.sum, foldD.left, foldD.right, iso1, iso2, cTo, cFrom)
      //            case _ => super.rewriteDef(d)
      //          }
      //
      //        case _ => super.rewriteDef(d)
      //      }
      //    case sf @ SumFold(sum,
      //        Def(Lambda(left:  Lambda[t1,c], _, _, HasViews(a, iso1: Iso[a, _]))),
      //        Def(Lambda(right: Lambda[t2,d], _, _, HasViews(b, iso2: Iso[b, _])))) => {
      //        val ea = iso1.eFrom
      //        val et1 = left.eA
      //        val et2 = right.eA
      //        (iso1.eTo, iso2.eTo) match {
      //          case IsConvertible(cTo, cFrom) =>
      //
      //          case _ => super.rewriteDef(d)
      //        }
      //     }

    }

    //    case FindFoldArg(arg) => {
    //      arg match {
    //        case Def(foldD@SumFold(_, Def(Lambda(left: Lambda[t1, c], _, _, HasViews(a1, iso1: Iso[a1, _]))),
    //        Def(Lambda(right: Lambda[t2, d], _, _, HasViews(b1, iso2: Iso[b1, _])))))
    //          if !d.isInstanceOf[MethodCall] =>
    //          (iso1.eTo, iso2.eTo) match {
    //            case IsConvertible(cTo, cFrom) =>
    //              val newFold = liftFromSumFold(foldD.sum, foldD.left, foldD.right, iso1, iso2, cTo, cFrom)
    //              d.mirror(new MapTransformer(arg -> newFold))
    //            case _ => super.rewriteDef(d)
    //          }
    //        case _ => super.rewriteDef(d)
    //      }
    //    }


    case call @ MethodCall(
      Def(foldD @ SumFold(sum,
          LambdaResultHasViews(left,  iso1: Iso[a, c]),
          LambdaResultHasViews(right, iso2: Iso[_, _]))), m, args, neverInvoke)
      if iso1 == iso2 =>
    {
      val newFold = liftFromSumFold(foldD.sum, foldD.left, foldD.right, iso1)
      mkMethodCall(newFold, m, args, neverInvoke, call.selfType)
    }

    case call @ MethodCall(Def(foldD @ SumFold(sum, left, right)), m, args, neverInvoke) => {
      implicit val resultElem: Elem[T] = d.selfType
      def copyMethodCall(newReceiver: Exp[_]) =
        mkMethodCall(newReceiver, m, args, neverInvoke, resultElem).asRep[T]

      sum.fold(
        a => copyMethodCall(left(a)),
        b => copyMethodCall(right(b))
      )
    }

    case IsLeft(Def(Left(_))) => true
    case IsLeft(Def(Right(_))) => false
    case IsRight(Def(Left(_))) => false
    case IsRight(Def(Right(_))) => true
    case _ => super.rewriteDef(d)
  }

}
