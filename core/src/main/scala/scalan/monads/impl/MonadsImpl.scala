package scalan.monads
package impl

import scalan.common.Default
import scalan._
import scala.reflect.runtime.universe._
import scalan.common.Default

trait MonadsAbs extends Monads
{ self: MonadsDsl =>
  // single proxy for each type family
  implicit def proxyMonad[A](p: Rep[Monad[A]]): Monad[A] =
    proxyOps[Monad[A]](p)

  abstract class MonadElem[A, From, To <: Monad[A]](iso: Iso[From, To]) extends ViewElem[From, To]()(iso)

  trait MonadCompanionElem extends CompanionElem[MonadCompanionAbs]
  implicit lazy val MonadCompanionElem: MonadCompanionElem = new MonadCompanionElem {
    lazy val tag = typeTag[MonadCompanionAbs]
    lazy val defaultRep = Default.defaultVal(Monad)
  }

  trait MonadCompanionAbs extends MonadCompanion {
    override def toString = "Monad"
  }
  def Monad: Rep[MonadCompanionAbs]
  implicit def proxyMonadCompanion(p: Rep[MonadCompanion]): MonadCompanion = {
    proxyOps[MonadCompanion](p)
  }

  // elem for concrete class
  class IdMonadElem[A](iso: Iso[IdMonadData[A], IdMonad[A]]) extends MonadElem[A, IdMonadData[A], IdMonad[A]](iso)

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
    lazy val defaultRepTo = Default.defaultVal[Rep[IdMonad[A]]](IdMonad(element[A].defaultRepValue))
    lazy val eTo = new IdMonadElem[A](this)
  }
  // 4) constructor and deconstructor
  trait IdMonadCompanionAbs extends IdMonadCompanion {
    override def toString = "IdMonad"

    def apply[A](value: Rep[A])(implicit elem: Elem[A]): Rep[IdMonad[A]] =
      mkIdMonad(value)
    def unapply[A:Elem](p: Rep[IdMonad[A]]) = unmkIdMonad(p)
  }
  def IdMonad: Rep[IdMonadCompanionAbs]
  implicit def proxyIdMonadCompanion(p: Rep[IdMonadCompanionAbs]): IdMonadCompanionAbs = {
    proxyOps[IdMonadCompanionAbs](p)
  }

  class IdMonadCompanionElem extends CompanionElem[IdMonadCompanionAbs] {
    lazy val tag = typeTag[IdMonadCompanionAbs]
    lazy val defaultRep = Default.defaultVal(IdMonad)
  }
  implicit lazy val IdMonadCompanionElem: IdMonadCompanionElem = new IdMonadCompanionElem

  implicit def proxyIdMonad[A:Elem](p: Rep[IdMonad[A]]): IdMonad[A] =
    proxyOps[IdMonad[A]](p)

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
  class StateMonadElem[S, A](iso: Iso[StateMonadData[S, A], StateMonad[S, A]]) extends MonadElem[A, StateMonadData[S, A], StateMonad[S, A]](iso)

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
    lazy val defaultRepTo = Default.defaultVal[Rep[StateMonad[S, A]]](StateMonad(fun { (x: Rep[S]) => Pair(element[S].defaultRepValue, element[A].defaultRepValue) }))
    lazy val eTo = new StateMonadElem[S, A](this)
  }
  // 4) constructor and deconstructor
  trait StateMonadCompanionAbs extends StateMonadCompanion {
    override def toString = "StateMonad"

    def apply[S, A](step: Rep[S => (S,A)])(implicit elem: Elem[A], eS: Elem[S]): Rep[StateMonad[S, A]] =
      mkStateMonad(step)
    def unapply[S:Elem, A:Elem](p: Rep[StateMonad[S, A]]) = unmkStateMonad(p)
  }
  def StateMonad: Rep[StateMonadCompanionAbs]
  implicit def proxyStateMonadCompanion(p: Rep[StateMonadCompanionAbs]): StateMonadCompanionAbs = {
    proxyOps[StateMonadCompanionAbs](p)
  }

  class StateMonadCompanionElem extends CompanionElem[StateMonadCompanionAbs] {
    lazy val tag = typeTag[StateMonadCompanionAbs]
    lazy val defaultRep = Default.defaultVal(StateMonad)
  }
  implicit lazy val StateMonadCompanionElem: StateMonadCompanionElem = new StateMonadCompanionElem

  implicit def proxyStateMonad[S:Elem, A:Elem](p: Rep[StateMonad[S, A]]): StateMonad[S, A] =
    proxyOps[StateMonad[S, A]](p)

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
  class IOMonadElem[A](iso: Iso[IOMonadData[A], IOMonad[A]]) extends MonadElem[A, IOMonadData[A], IOMonad[A]](iso)

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
    lazy val defaultRepTo = Default.defaultVal[Rep[IOMonad[A]]](IOMonad(fun { (x: Rep[Int]) => Pair(0, element[A].defaultRepValue) }))
    lazy val eTo = new IOMonadElem[A](this)
  }
  // 4) constructor and deconstructor
  trait IOMonadCompanionAbs extends IOMonadCompanion {
    override def toString = "IOMonad"

    def apply[A](step: Rep[Int => (Int,A)])(implicit elem: Elem[A]): Rep[IOMonad[A]] =
      mkIOMonad(step)
    def unapply[A:Elem](p: Rep[IOMonad[A]]) = unmkIOMonad(p)
  }
  def IOMonad: Rep[IOMonadCompanionAbs]
  implicit def proxyIOMonadCompanion(p: Rep[IOMonadCompanionAbs]): IOMonadCompanionAbs = {
    proxyOps[IOMonadCompanionAbs](p)
  }

  class IOMonadCompanionElem extends CompanionElem[IOMonadCompanionAbs] {
    lazy val tag = typeTag[IOMonadCompanionAbs]
    lazy val defaultRep = Default.defaultVal(IOMonad)
  }
  implicit lazy val IOMonadCompanionElem: IOMonadCompanionElem = new IOMonadCompanionElem

  implicit def proxyIOMonad[A:Elem](p: Rep[IOMonad[A]]): IOMonad[A] =
    proxyOps[IOMonad[A]](p)

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
      (implicit elem: Elem[A])
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
      (implicit elem: Elem[A], eS: Elem[S])
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
      (implicit elem: Elem[A])
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

trait MonadsExp extends MonadsAbs { self: ScalanExp with MonadsDsl =>
  lazy val Monad: Rep[MonadCompanionAbs] = new MonadCompanionAbs with UserTypeDef[MonadCompanionAbs, MonadCompanionAbs] {
    lazy val selfType = element[MonadCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpIdMonad[A]
      (override val value: Rep[A])
      (implicit elem: Elem[A])
    extends IdMonad[A](value) with UserTypeDef[Monad[A], IdMonad[A]] {
    lazy val selfType = element[IdMonad[A]].asInstanceOf[Elem[Monad[A]]]
    override def mirror(t: Transformer) = ExpIdMonad[A](t(value))
  }

  lazy val IdMonad: Rep[IdMonadCompanionAbs] = new IdMonadCompanionAbs with UserTypeDef[IdMonadCompanionAbs, IdMonadCompanionAbs] {
    lazy val selfType = element[IdMonadCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object IdMonadMethods {
    object companion {
      def unapply(d: Def[_]): Option[Rep[IdMonad[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "companion" && receiver.elem.isInstanceOf[IdMonadElem[_]] =>
          Some(receiver).asInstanceOf[Option[Rep[IdMonad[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IdMonad[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `flatMap` : method has Non-Rep argument f: Rep[A] => Rep[Monad[B]] 

    object run {
      def unapply(d: Def[_]): Option[Rep[IdMonad[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "run" && receiver.elem.isInstanceOf[IdMonadElem[_]] =>
          Some(receiver).asInstanceOf[Option[Rep[IdMonad[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IdMonad[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object IdMonadCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit forSome {type A}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "defaultOf" && receiver.elem.isInstanceOf[IdMonadCompanionElem] =>
          Some(()).asInstanceOf[Option[Unit forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object point {
      def unapply(d: Def[_]): Option[Rep[A] forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(value, _*)) if method.getName == "point" && receiver.elem.isInstanceOf[IdMonadCompanionElem] =>
          Some(value).asInstanceOf[Option[Rep[A] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[A] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkIdMonad[A]
    (value: Rep[A])(implicit elem: Elem[A]) =
    new ExpIdMonad[A](value)
  def unmkIdMonad[A:Elem](p: Rep[IdMonad[A]]) =
    Some((p.value))

  case class ExpStateMonad[S, A]
      (override val step: Rep[S => (S,A)])
      (implicit elem: Elem[A], eS: Elem[S])
    extends StateMonad[S, A](step) with UserTypeDef[Monad[A], StateMonad[S, A]] {
    lazy val selfType = element[StateMonad[S, A]].asInstanceOf[Elem[Monad[A]]]
    override def mirror(t: Transformer) = ExpStateMonad[S, A](t(step))
  }

  lazy val StateMonad: Rep[StateMonadCompanionAbs] = new StateMonadCompanionAbs with UserTypeDef[StateMonadCompanionAbs, StateMonadCompanionAbs] {
    lazy val selfType = element[StateMonadCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object StateMonadMethods {
    object companion {
      def unapply(d: Def[_]): Option[Rep[StateMonad[S, A]] forSome {type S; type A}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "companion" && receiver.elem.isInstanceOf[StateMonadElem[_, _]] =>
          Some(receiver).asInstanceOf[Option[Rep[StateMonad[S, A]] forSome {type S; type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[StateMonad[S, A]] forSome {type S; type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `flatMap` : method has Non-Rep argument f: Rep[A] => Rep[Monad[B]] 

    object run {
      def unapply(d: Def[_]): Option[Rep[StateMonad[S, A]] forSome {type S; type A}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "run" && receiver.elem.isInstanceOf[StateMonadElem[_, _]] =>
          Some(receiver).asInstanceOf[Option[Rep[StateMonad[S, A]] forSome {type S; type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[StateMonad[S, A]] forSome {type S; type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object runFrom {
      def unapply(d: Def[_]): Option[(Rep[StateMonad[S, A]], Rep[S]) forSome {type S; type A}] = d match {
        case MethodCall(receiver, method, Seq(s, _*)) if method.getName == "runFrom" && receiver.elem.isInstanceOf[StateMonadElem[_, _]] =>
          Some((receiver, s)).asInstanceOf[Option[(Rep[StateMonad[S, A]], Rep[S]) forSome {type S; type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[StateMonad[S, A]], Rep[S]) forSome {type S; type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object StateMonadCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit forSome {type S; type A}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "defaultOf" && receiver.elem.isInstanceOf[StateMonadCompanionElem] =>
          Some(()).asInstanceOf[Option[Unit forSome {type S; type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type S; type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object point {
      def unapply(d: Def[_]): Option[Rep[A] forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(value, _*)) if method.getName == "point" && receiver.elem.isInstanceOf[StateMonadCompanionElem] =>
          Some(value).asInstanceOf[Option[Rep[A] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[A] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object get {
      def unapply(d: Def[_]): Option[Unit forSome {type S}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "get" && receiver.elem.isInstanceOf[StateMonadCompanionElem] =>
          Some(()).asInstanceOf[Option[Unit forSome {type S}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type S}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object put {
      def unapply(d: Def[_]): Option[Rep[S] forSome {type S}] = d match {
        case MethodCall(receiver, method, Seq(s, _*)) if method.getName == "put" && receiver.elem.isInstanceOf[StateMonadCompanionElem] =>
          Some(s).asInstanceOf[Option[Rep[S] forSome {type S}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[S] forSome {type S}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `modify` : method has Non-Rep argument f: Rep[S] => Rep[S] 
  }

  def mkStateMonad[S, A]
    (step: Rep[S => (S,A)])(implicit elem: Elem[A], eS: Elem[S]) =
    new ExpStateMonad[S, A](step)
  def unmkStateMonad[S:Elem, A:Elem](p: Rep[StateMonad[S, A]]) =
    Some((p.step))

  case class ExpIOMonad[A]
      (override val step: Rep[Int => (Int,A)])
      (implicit elem: Elem[A])
    extends IOMonad[A](step) with UserTypeDef[Monad[A], IOMonad[A]] {
    lazy val selfType = element[IOMonad[A]].asInstanceOf[Elem[Monad[A]]]
    override def mirror(t: Transformer) = ExpIOMonad[A](t(step))
  }

  lazy val IOMonad: Rep[IOMonadCompanionAbs] = new IOMonadCompanionAbs with UserTypeDef[IOMonadCompanionAbs, IOMonadCompanionAbs] {
    lazy val selfType = element[IOMonadCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object IOMonadMethods {
    object companion {
      def unapply(d: Def[_]): Option[Rep[IOMonad[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "companion" && receiver.elem.isInstanceOf[IOMonadElem[_]] =>
          Some(receiver).asInstanceOf[Option[Rep[IOMonad[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IOMonad[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `flatMap` : method has Non-Rep argument f: Rep[A] => Rep[Monad[B]] 

    object run {
      def unapply(d: Def[_]): Option[Rep[IOMonad[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "run" && receiver.elem.isInstanceOf[IOMonadElem[_]] =>
          Some(receiver).asInstanceOf[Option[Rep[IOMonad[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IOMonad[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object runIO {
      def unapply(d: Def[_]): Option[Rep[IOMonad[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "runIO" && receiver.elem.isInstanceOf[IOMonadElem[_]] =>
          Some(receiver).asInstanceOf[Option[Rep[IOMonad[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IOMonad[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object IOMonadCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit forSome {type A}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "defaultOf" && receiver.elem.isInstanceOf[IOMonadCompanionElem] =>
          Some(()).asInstanceOf[Option[Unit forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object point {
      def unapply(d: Def[_]): Option[Rep[A] forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(value, _*)) if method.getName == "point" && receiver.elem.isInstanceOf[IOMonadCompanionElem] =>
          Some(value).asInstanceOf[Option[Rep[A] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[A] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object println {
      def unapply(d: Def[_]): Option[Rep[String]] = d match {
        case MethodCall(receiver, method, Seq(s, _*)) if method.getName == "println" && receiver.elem.isInstanceOf[IOMonadCompanionElem] =>
          Some(s).asInstanceOf[Option[Rep[String]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[String]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkIOMonad[A]
    (step: Rep[Int => (Int,A)])(implicit elem: Elem[A]) =
    new ExpIOMonad[A](step)
  def unmkIOMonad[A:Elem](p: Rep[IOMonad[A]]) =
    Some((p.step))

  object MonadMethods {
    // WARNING: Cannot generate matcher for method `flatMap` : method has Non-Rep argument f: Rep[A] => Rep[Monad[B]] 

    // WARNING: Cannot generate matcher for method `map` : method has Non-Rep argument f: Rep[A] => Rep[B] 

    // WARNING: Cannot generate matcher for method `filter` : method has Non-Rep argument p: Rep[A] => Rep[Boolean] 

    // WARNING: Cannot generate matcher for method `withFilter` : method has Non-Rep argument p: Rep[A] => Rep[Boolean] 

    object run {
      def unapply(d: Def[_]): Option[Rep[Monad[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "run" && receiver.elem.isInstanceOf[MonadElem[_, _, _]] =>
          Some(receiver).asInstanceOf[Option[Rep[Monad[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Monad[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object companion {
      def unapply(d: Def[_]): Option[Rep[Monad[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "companion" && receiver.elem.isInstanceOf[MonadElem[_, _, _]] =>
          Some(receiver).asInstanceOf[Option[Rep[Monad[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Monad[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object MonadCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit forSome {type A}] = d match {
        case MethodCall(receiver, method, _) if method.getName == "defaultOf" && receiver.elem.isInstanceOf[MonadCompanionElem] =>
          Some(()).asInstanceOf[Option[Unit forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object point {
      def unapply(d: Def[_]): Option[Rep[A] forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(value, _*)) if method.getName == "point" && receiver.elem.isInstanceOf[MonadCompanionElem] =>
          Some(value).asInstanceOf[Option[Rep[A] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[A] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}
