
package scalan.monads
package impl

import scalan.common.Default
import scalan._
import scala.reflect.runtime.universe._
import scalan.common.Default.defaultVal


trait MonadsAbs extends Monads
{ self: MonadsDsl =>

  // single proxy for each type family
  implicit def proxyMonad[A:Elem](p: MonadRep[A]): Monad[A] = {
    proxyOps[Monad[A]](p)
  }

  trait MonadElem[From,To] extends ViewElem[From, To]

  trait MonadCompanionElem extends CompanionElem[MonadCompanionAbs]
  implicit lazy val MonadCompanionElem: MonadCompanionElem = new MonadCompanionElem {
    lazy val tag = typeTag[MonadCompanionAbs]
    lazy val defaultRep = defaultVal(Monad)
  }

  trait MonadCompanionAbs extends MonadCompanion
  def Monad: Rep[MonadCompanionAbs]
  implicit def defaultOfMonad[A:Elem]: Default[Rep[Monad[A]]] = Monad.defaultOf[A]
  implicit def proxyMonadCompanion(p: Rep[MonadCompanion]): MonadCompanion = {
    proxyOps[MonadCompanion](p, true)
  }


  // elem for concrete class
  class IdMonadElem[A](implicit iso: Iso[IdMonadData[A], IdMonad[A]]) extends MonadElem[IdMonadData[A], IdMonad[A]]

  // state representation type
  type IdMonadData[A] = A

  // 3) Iso for concrete class
  class IdMonadIso[A](implicit elem: Elem[A])
    extends Iso[IdMonadData[A], IdMonad[A]] {
    override def from(p: Rep[IdMonad[A]]) =
      unmkIdMonad(p) match {
        case Some((value)) => value
        case None => !!!
      }
    override def to(p: Rep[A]) = {
      val value = p
      IdMonad(value)
    }
    lazy val tag = {
      implicit val tagA = element[A].tag
      typeTag[IdMonad[A]]
    }
    lazy val defaultRepTo = defaultVal[Rep[IdMonad[A]]](IdMonad(element[A].defaultRepValue))
    lazy val eTo = new IdMonadElem[A]()(this)
  }
  // 4) constructor and deconstructor
  trait IdMonadCompanionAbs extends IdMonadCompanion {

    def apply[A]
          (value: Rep[A])(implicit elem: Elem[A]): Rep[IdMonad[A]] =
      mkIdMonad(value)
    def unapply[A:Elem](p: Rep[IdMonad[A]]) = unmkIdMonad(p)
  }

  def IdMonad: Rep[IdMonadCompanionAbs]
  implicit def proxyIdMonadCompanion(p: Rep[IdMonadCompanionAbs]): IdMonadCompanionAbs = {
    proxyOps[IdMonadCompanionAbs](p, true)
  }

  trait IdMonadCompanionElem extends CompanionElem[IdMonadCompanionAbs]
  implicit lazy val IdMonadCompanionElem: IdMonadCompanionElem = new IdMonadCompanionElem {
    lazy val tag = typeTag[IdMonadCompanionAbs]
    lazy val defaultRep = defaultVal(IdMonad)
  }

  implicit def proxyIdMonad[A:Elem](p: Rep[IdMonad[A]]): IdMonad[A] = {
    proxyOps[IdMonad[A]](p)
  }

  implicit class ExtendedIdMonad[A](p: Rep[IdMonad[A]])(implicit elem: Elem[A]) {
    def toData: Rep[IdMonadData[A]] = isoIdMonad(elem).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoIdMonad[A](implicit elem: Elem[A]): Iso[IdMonadData[A], IdMonad[A]] =
    new IdMonadIso[A]

  // 6) smart constructor and deconstructor
  def mkIdMonad[A](value: Rep[A])(implicit elem: Elem[A]): Rep[IdMonad[A]]
  def unmkIdMonad[A:Elem](p: Rep[IdMonad[A]]): Option[(Rep[A])]


  // elem for concrete class
  class StateMonadElem[S, A](implicit iso: Iso[StateMonadData[S, A], StateMonad[S, A]]) extends MonadElem[StateMonadData[S, A], StateMonad[S, A]]

  // state representation type
  type StateMonadData[S, A] = S => (S,A)

  // 3) Iso for concrete class
  class StateMonadIso[S, A](implicit elem: Elem[A], eS: Elem[S])
    extends Iso[StateMonadData[S, A], StateMonad[S, A]] {
    override def from(p: Rep[StateMonad[S, A]]) =
      unmkStateMonad(p) match {
        case Some((step)) => step
        case None => !!!
      }
    override def to(p: Rep[S => (S,A)]) = {
      val step = p
      StateMonad(step)
    }
    lazy val tag = {
      implicit val tagS = element[S].tag
      implicit val tagA = element[A].tag
      typeTag[StateMonad[S, A]]
    }
    lazy val defaultRepTo = defaultVal[Rep[StateMonad[S, A]]](StateMonad(fun { (x: Rep[S]) => Pair(element[S].defaultRepValue, element[A].defaultRepValue) }))
    lazy val eTo = new StateMonadElem[S, A]()(this)
  }
  // 4) constructor and deconstructor
  trait StateMonadCompanionAbs extends StateMonadCompanion {

    def apply[S, A]
          (step: Rep[S => (S,A)])(implicit elem: Elem[A], eS: Elem[S]): Rep[StateMonad[S, A]] =
      mkStateMonad(step)
    def unapply[S:Elem, A:Elem](p: Rep[StateMonad[S, A]]) = unmkStateMonad(p)
  }

  def StateMonad: Rep[StateMonadCompanionAbs]
  implicit def proxyStateMonadCompanion(p: Rep[StateMonadCompanionAbs]): StateMonadCompanionAbs = {
    proxyOps[StateMonadCompanionAbs](p, true)
  }

  trait StateMonadCompanionElem extends CompanionElem[StateMonadCompanionAbs]
  implicit lazy val StateMonadCompanionElem: StateMonadCompanionElem = new StateMonadCompanionElem {
    lazy val tag = typeTag[StateMonadCompanionAbs]
    lazy val defaultRep = defaultVal(StateMonad)
  }

  implicit def proxyStateMonad[S:Elem, A:Elem](p: Rep[StateMonad[S, A]]): StateMonad[S, A] = {
    proxyOps[StateMonad[S, A]](p)
  }

  implicit class ExtendedStateMonad[S, A](p: Rep[StateMonad[S, A]])(implicit elem: Elem[A], eS: Elem[S]) {
    def toData: Rep[StateMonadData[S, A]] = isoStateMonad(elem, eS).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoStateMonad[S, A](implicit elem: Elem[A], eS: Elem[S]): Iso[StateMonadData[S, A], StateMonad[S, A]] =
    new StateMonadIso[S, A]

  // 6) smart constructor and deconstructor
  def mkStateMonad[S, A](step: Rep[S => (S,A)])(implicit elem: Elem[A], eS: Elem[S]): Rep[StateMonad[S, A]]
  def unmkStateMonad[S:Elem, A:Elem](p: Rep[StateMonad[S, A]]): Option[(Rep[S => (S,A)])]


  // elem for concrete class
  class IOMonadElem[A](implicit iso: Iso[IOMonadData[A], IOMonad[A]]) extends MonadElem[IOMonadData[A], IOMonad[A]]

  // state representation type
  type IOMonadData[A] = Int => (Int,A)

  // 3) Iso for concrete class
  class IOMonadIso[A](implicit elem: Elem[A])
    extends Iso[IOMonadData[A], IOMonad[A]] {
    override def from(p: Rep[IOMonad[A]]) =
      unmkIOMonad(p) match {
        case Some((step)) => step
        case None => !!!
      }
    override def to(p: Rep[Int => (Int,A)]) = {
      val step = p
      IOMonad(step)
    }
    lazy val tag = {
      implicit val tagA = element[A].tag
      typeTag[IOMonad[A]]
    }
    lazy val defaultRepTo = defaultVal[Rep[IOMonad[A]]](IOMonad(fun { (x: Rep[Int]) => Pair(0, element[A].defaultRepValue) }))
    lazy val eTo = new IOMonadElem[A]()(this)
  }
  // 4) constructor and deconstructor
  trait IOMonadCompanionAbs extends IOMonadCompanion {

    def apply[A]
          (step: Rep[Int => (Int,A)])(implicit elem: Elem[A]): Rep[IOMonad[A]] =
      mkIOMonad(step)
    def unapply[A:Elem](p: Rep[IOMonad[A]]) = unmkIOMonad(p)
  }

  def IOMonad: Rep[IOMonadCompanionAbs]
  implicit def proxyIOMonadCompanion(p: Rep[IOMonadCompanionAbs]): IOMonadCompanionAbs = {
    proxyOps[IOMonadCompanionAbs](p, true)
  }

  trait IOMonadCompanionElem extends CompanionElem[IOMonadCompanionAbs]
  implicit lazy val IOMonadCompanionElem: IOMonadCompanionElem = new IOMonadCompanionElem {
    lazy val tag = typeTag[IOMonadCompanionAbs]
    lazy val defaultRep = defaultVal(IOMonad)
  }

  implicit def proxyIOMonad[A:Elem](p: Rep[IOMonad[A]]): IOMonad[A] = {
    proxyOps[IOMonad[A]](p)
  }

  implicit class ExtendedIOMonad[A](p: Rep[IOMonad[A]])(implicit elem: Elem[A]) {
    def toData: Rep[IOMonadData[A]] = isoIOMonad(elem).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoIOMonad[A](implicit elem: Elem[A]): Iso[IOMonadData[A], IOMonad[A]] =
    new IOMonadIso[A]

  // 6) smart constructor and deconstructor
  def mkIOMonad[A](step: Rep[Int => (Int,A)])(implicit elem: Elem[A]): Rep[IOMonad[A]]
  def unmkIOMonad[A:Elem](p: Rep[IOMonad[A]]): Option[(Rep[Int => (Int,A)])]

}


trait MonadsSeq extends MonadsAbs { self: ScalanSeq with MonadsDsl =>

  lazy val Monad: Rep[MonadCompanionAbs] = new MonadCompanionAbs with UserTypeSeq[MonadCompanionAbs, MonadCompanionAbs] {
    lazy val selfType = element[MonadCompanionAbs]
  }

  case class SeqIdMonad[A]
      (override val value: Rep[A])
      (implicit override val elem: Elem[A])
    extends IdMonad[A](value) with UserTypeSeq[Monad[A], IdMonad[A]] {
    lazy val selfType = element[IdMonad[A]].asInstanceOf[Elem[Monad[A]]]
  }

  lazy val IdMonad = new IdMonadCompanionAbs with UserTypeSeq[IdMonadCompanionAbs, IdMonadCompanionAbs] {
    lazy val selfType = element[IdMonadCompanionAbs]
  }



  def mkIdMonad[A]
      (value: Rep[A])(implicit elem: Elem[A]) =
      new SeqIdMonad[A](value)
  def unmkIdMonad[A:Elem](p: Rep[IdMonad[A]]) =
    Some((p.value))


  case class SeqStateMonad[S, A]
      (override val step: Rep[S => (S,A)])
      (implicit override val elem: Elem[A], override val eS: Elem[S])
    extends StateMonad[S, A](step) with UserTypeSeq[Monad[A], StateMonad[S, A]] {
    lazy val selfType = element[StateMonad[S, A]].asInstanceOf[Elem[Monad[A]]]
  }

  lazy val StateMonad = new StateMonadCompanionAbs with UserTypeSeq[StateMonadCompanionAbs, StateMonadCompanionAbs] {
    lazy val selfType = element[StateMonadCompanionAbs]
  }



  def mkStateMonad[S, A]
      (step: Rep[S => (S,A)])(implicit elem: Elem[A], eS: Elem[S]) =
      new SeqStateMonad[S, A](step)
  def unmkStateMonad[S:Elem, A:Elem](p: Rep[StateMonad[S, A]]) =
    Some((p.step))


  case class SeqIOMonad[A]
      (override val step: Rep[Int => (Int,A)])
      (implicit override val elem: Elem[A])
    extends IOMonad[A](step) with UserTypeSeq[Monad[A], IOMonad[A]] {
    lazy val selfType = element[IOMonad[A]].asInstanceOf[Elem[Monad[A]]]
  }

  lazy val IOMonad = new IOMonadCompanionAbs with UserTypeSeq[IOMonadCompanionAbs, IOMonadCompanionAbs] {
    lazy val selfType = element[IOMonadCompanionAbs]
  }



  def mkIOMonad[A]
      (step: Rep[Int => (Int,A)])(implicit elem: Elem[A]) =
      new SeqIOMonad[A](step)
  def unmkIOMonad[A:Elem](p: Rep[IOMonad[A]]) =
    Some((p.step))

}


trait MonadsExp extends MonadsAbs with scalan.ProxyExp with scalan.ViewsExp { self: ScalanExp with MonadsDsl =>

  lazy val Monad: Rep[MonadCompanionAbs] = new MonadCompanionAbs with UserTypeDef[MonadCompanionAbs, MonadCompanionAbs] {
    lazy val selfType = element[MonadCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpIdMonad[A]
      (override val value: Rep[A])
      (implicit override val elem: Elem[A])
    extends IdMonad[A](value) with UserTypeDef[Monad[A], IdMonad[A]] {
    lazy val selfType = element[IdMonad[A]].asInstanceOf[Elem[Monad[A]]]
    override def mirror(t: Transformer) = ExpIdMonad[A](t(value))
  }

  lazy val IdMonad: Rep[IdMonadCompanionAbs] = new IdMonadCompanionAbs with UserTypeDef[IdMonadCompanionAbs, IdMonadCompanionAbs] {
    lazy val selfType = element[IdMonadCompanionAbs]
    override def mirror(t: Transformer) = this
  }



  def mkIdMonad[A]
    (value: Rep[A])(implicit elem: Elem[A]) =
    new ExpIdMonad[A](value)
  def unmkIdMonad[A:Elem](p: Rep[IdMonad[A]]) =
    Some((p.value))


  case class ExpStateMonad[S, A]
      (override val step: Rep[S => (S,A)])
      (implicit override val elem: Elem[A], override val eS: Elem[S])
    extends StateMonad[S, A](step) with UserTypeDef[Monad[A], StateMonad[S, A]] {
    lazy val selfType = element[StateMonad[S, A]].asInstanceOf[Elem[Monad[A]]]
    override def mirror(t: Transformer) = ExpStateMonad[S, A](t(step))
  }

  lazy val StateMonad: Rep[StateMonadCompanionAbs] = new StateMonadCompanionAbs with UserTypeDef[StateMonadCompanionAbs, StateMonadCompanionAbs] {
    lazy val selfType = element[StateMonadCompanionAbs]
    override def mirror(t: Transformer) = this
  }



  def mkStateMonad[S, A]
    (step: Rep[S => (S,A)])(implicit elem: Elem[A], eS: Elem[S]) =
    new ExpStateMonad[S, A](step)
  def unmkStateMonad[S:Elem, A:Elem](p: Rep[StateMonad[S, A]]) =
    Some((p.step))


  case class ExpIOMonad[A]
      (override val step: Rep[Int => (Int,A)])
      (implicit override val elem: Elem[A])
    extends IOMonad[A](step) with UserTypeDef[Monad[A], IOMonad[A]] {
    lazy val selfType = element[IOMonad[A]].asInstanceOf[Elem[Monad[A]]]
    override def mirror(t: Transformer) = ExpIOMonad[A](t(step))
  }

  lazy val IOMonad: Rep[IOMonadCompanionAbs] = new IOMonadCompanionAbs with UserTypeDef[IOMonadCompanionAbs, IOMonadCompanionAbs] {
    lazy val selfType = element[IOMonadCompanionAbs]
    override def mirror(t: Transformer) = this
  }



  def mkIOMonad[A]
    (step: Rep[Int => (Int,A)])(implicit elem: Elem[A]) =
    new ExpIOMonad[A](step)
  def unmkIOMonad[A:Elem](p: Rep[IOMonad[A]]) =
    Some((p.step))

}
