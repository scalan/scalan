package scalan

import OverloadHack.Overloaded2

trait Converters extends ViewsModule { self: Scalan =>

  type Conv[T,R] = Rep[Converter[T,R]]
  trait Converter[T,R] extends Def[Converter[T,R]] {
    implicit def eT: Elem[T]
    implicit def eR: Elem[R]
    def convFun: Rep[T => R] = defaultConvFun
    private[this] lazy val defaultConvFun: Rep[T => R] = fun { apply(_: Rep[T]) }
    def apply(x: Rep[T]): Rep[R]
    def isIdentity: Boolean = false
    override def toString: String = s"${eT.name} --> ${eR.name}"
  }
  trait ConverterCompanion

  abstract class IdentityConv[A](implicit val eT: Elem[A]) extends Converter[A, A] {
    def eR: Elem[A] = eT
    def apply(x: Rep[A]) = x
    override val convFun = identityFun[A]
    override def isIdentity = true
    override def equals(other: Any) = other match {
      case i: Converters#IdentityConv[_] => (this eq i) || (eT == i.eT)
      case _ => false
    }
  }

  implicit class ConvOps[A,B](c: Conv[A,B]) {
    def >>[B1 >: B, C](c2: Conv[B1,C]): Conv[A,C] = composeConv(c2, c)
    def >>[B1 >: B, C](f: Rep[B1 => C])(implicit o2: Overloaded2): Rep[A => C] = {
      compose(f, funcFromConv(c).asRep[A => B1])
    }
  }
  implicit class AnyConvOps(c: Conv[_, _]) {
    def asConv[C,D] = c.asInstanceOf[Conv[C,D]]
  }

  abstract class BaseConverter[T,R](override val convFun: Rep[T => R])//(implicit val eT: Elem[T], val eR: Elem[R])
    extends Converter[T,R] {
    implicit def eT: Elem[T]
    implicit def eR: Elem[R]
    def apply(x: Rep[T]): Rep[R] = convFun(x)
    override def equals(other: Any): Boolean = other match {
      case c: Converters#BaseConverter[_, _] => eT == c.eT && eR == c.eR && convFun == c.convFun
      case _ => false
    }
  }
  trait BaseConverterCompanion

  abstract class PairConverter[A1, A2, B1, B2]
    (val conv1: Conv[A1, B1], val conv2: Conv[A2, B2])
//    (implicit val eA1: Elem[A1], val eA2: Elem[A2], val eB1: Elem[B1], val eB2: Elem[B2])
    extends Converter[(A1, A2), (B1, B2)] {
    def eA1: Elem[A1]; def eA2: Elem[A2]; def eB1: Elem[B1]; def eB2: Elem[B2]
    lazy val eT = pairElement(eA1, eA2)
    lazy val eR = pairElement(eB1, eB2)
    def apply(x: Rep[(A1,A2)]) = { val Pair(a1, a2) = x; Pair(conv1(a1), conv2(a2)) }
    override def isIdentity = conv1.isIdentity && conv2.isIdentity
  }
  trait PairConverterCompanion

  abstract class SumConverter[A1, A2, B1, B2]
    (val conv1: Conv[A1, B1], val conv2: Conv[A2, B2])
//    (implicit val eA1: Elem[A1], val eA2: Elem[A2], val eB1: Elem[B1], val eB2: Elem[B2])
    extends Converter[(A1 | A2), (B1 | B2)] {
    def eA1: Elem[A1]; def eA2: Elem[A2]; def eB1: Elem[B1]; def eB2: Elem[B2]
    lazy val eT = sumElement(eA1, eA2)
    lazy val eR = sumElement(eB1, eB2)
    def apply(x: Rep[(A1|A2)]) = { x.mapSumBy(conv1.convFun, conv2.convFun) }
    override def isIdentity = conv1.isIdentity && conv2.isIdentity
  }
  trait SumConverterCompanion

  abstract class ComposeConverter[A, B, C](val conv2: Conv[B, C], val conv1: Conv[A, B])/*(
    implicit val eA: Elem[A], val eB: Elem[B], val eC: Elem[C])*/ extends Converter[A, C] {
    val eT: Elem[A] = conv1.eT
    val eR: Elem[C] = conv2.eR
    def apply(a: Rep[A]) = conv2.apply(conv1.apply(a))
    override def isIdentity = conv1.isIdentity && conv2.isIdentity
    override def equals(other: Any) = other match {
      case i: Converters#ComposeConverter[_, _, _] => (this eq i) || (conv1 == i.conv1 && conv2 == i.conv2)
      case _ => false
    }
  }

  abstract class FunctorConverter[A,B,F[_]]
      (val itemConv: Conv[A, B])
      (implicit val F: Functor[F])
    extends Converter[F[A], F[B]] {
    def apply(xs: Rep[F[A]]): Rep[F[B]] = F.map(xs){ x => itemConv(x) }
    def eA: Elem[A]; def eB: Elem[B]
    lazy val eT = F.lift(eA)
    lazy val eR = F.lift(eB)
    override def isIdentity = itemConv.isIdentity
    override def equals(other: Any): Boolean = other match {
      case c: Converters#FunctorConverter[_, _, _] => eT == c.eT && eR == c.eR && itemConv == c.itemConv
      case _ => false
    }
  }
  trait FunctorConverterCompanion

  abstract class NaturalConverter[A,F[_],G[_]]
      (override val convFun: Rep[F[A] => G[A]])
      (implicit val eA: Elem[A], val cF: Cont[F], val cG: Cont[G])
    extends Converter[F[A], G[A]] {
    def apply(xs: Rep[F[A]]): Rep[G[A]] = convFun(xs)

    val eT = cF.lift(eA)
    val eR = cG.lift(eA)
    override def equals(other: Any): Boolean = other match {
      case c: Converters#NaturalConverter[_, _, _] => eT == c.eT && eR == c.eR && convFun == c.convFun
      case _ => false
    }
  }
}

trait ConvertersModule extends impl.ConvertersDefs { self: Scalan =>

  def identityConv[A](implicit elem: Elem[A]): Conv[A, A] = IdentityConv[A]()(elem)

  def baseConv[T,R](f: Rep[T => R]): Conv[T,R] = BaseConverter(f)
  def funcFromConv[T,R](c: Conv[T,R]): Rep[T => R] = c.convFun

  def pairConv[A1, A2, B1, B2](conv1: Conv[A1, B1], conv2: Conv[A2, B2]): Conv[(A1, A2), (B1, B2)] =
    PairConverter[A1, A2, B1, B2](conv1, conv2)
    
  def composeConv[A, B, B1 >: B, C](c2: Conv[B1, C], c1: Conv[A, B]): Conv[A, C] = {
    if (c2.isIdentity)
      c1
    else if (c1.isIdentity)
      c2
    else
      (c2, c1) match {
        case (Def(conv2d: PairConverter[b1, b2, c1, c2]), Def(conv1d: PairConverter[a1, a2, _, _])) =>
          val composedConv1 = composeConv(conv2d.conv1, conv1d.conv1.asInstanceOf[Conv[a1, b1]])
          val composedConv2 = composeConv(conv2d.conv2, conv1d.conv2.asInstanceOf[Conv[a2, b2]])
          pairConv(composedConv1, composedConv2)
        case _ =>
          ComposeConverter[A, B1, C](c2, c1.asConv[A,B1])
      }
  }.asInstanceOf[Conv[A, C]]

  object HasConv {
    def unapply[A,B](elems: (Elem[A], Elem[B])): Option[Conv[A,B]] = getConverter(elems._1, elems._2)
  }

  object IsConvertible {
    def unapply[A,B](elems: (Elem[A], Elem[B])): Option[(Conv[A,B], Conv[B,A])] =
      for {
        c1 <- HasConv.unapply(elems)
        c2 <- HasConv.unapply(elems.swap)
      }
        yield (c1, c2)
  }

  def getConverter[A,B](eA: Elem[A], eB: Elem[B]): Option[Conv[A,B]] = {
    (eA, eB) match {
      case (e1, e2) if e1 == e2 =>
        implicit val ea = e1
        Some(identityConv[A].asConv[A,B])
      case (pA: PairElem[a1,a2], pB: PairElem[b1,b2]) =>
        for {
          c1 <- getConverter(pA.eFst, pB.eFst)
          c2 <- getConverter(pA.eSnd, pB.eSnd)
        }
        yield pairConv(c1, c2)
      case (pA: SumElem[a1,a2], pB: SumElem[b1,b2]) =>
        implicit val ea1 = pA.eLeft
        implicit val eb1 = pB.eLeft
        implicit val ea2 = pA.eRight
        implicit val eb2 = pB.eRight
        for {
          c1 <- getConverter(ea1, eb1)
          c2 <- getConverter(ea2, eb2)
        }
        yield SumConverter(c1, c2)
      case (e1: EntityElem1[a1,to1,_], e2: EntityElem1[a2,to2,_])
        if e1.cont.name == e2.cont.name && e1.cont.isFunctor =>
        implicit val ea1 = e1.eItem
        implicit val ea2 = e2.eItem
        type F[T] = T
        val F = e1.cont.asInstanceOf[Functor[F]]
        for { c <- getConverter(ea1, ea2) }
          yield FunctorConverter(c)(F).asRep[Converter[A,B]]
      case (eEntity: EntityElem[_], eClass: ConcreteElem[tData,tClass]) =>
        val convOpt = eClass.getConverterFrom(eEntity)
        convOpt
      case (eClass: ConcreteElem[tData,tClass], eEntity: EntityElem[_]) if eClass <:< eEntity =>
        Some(BaseConverter(identityFun(eClass)).asRep[Converter[A,B]])
      case _ => None
    }
  }

  case class Convert[From,To](eFrom: Elem[From], eTo: Elem[To], x: Rep[Def[_]], conv: Rep[From => To])
    extends BaseDef[To]()(eTo)

  def tryConvert[From, To](eFrom: Elem[From], eTo: Elem[To], x: Rep[Def[_]], conv: Rep[From => To]): Rep[To] = {
    if (x.elem <:< eFrom)
      conv(x.asRep[From])
    else
      Convert(eFrom, eTo, x, conv)
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    // Rule: convert(eFrom, eTo, x, conv) if x.elem <:< eFrom  ==>  conv(x)
    case Convert(eFrom: Elem[from], eTo: Elem[to], x,  conv) if x.elem <:< eFrom =>
      conv(x)

    case Convert(eFrom: Elem[from], eTo: Elem[to], HasViews(_x, _iso: Iso[Def[_], _] @unchecked),  _conv) =>
      val iso = _iso.asInstanceOf[Iso[Def[_], from]]
      val conv = _conv.asRep[from => to]
      val x = _x.asRep[Def[_]]
      tryConvert(x.elem, eTo, x, iso.toFun >> conv)

    case _ => super.rewriteDef(d)
  }
}

