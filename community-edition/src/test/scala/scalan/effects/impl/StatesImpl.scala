package scalan.monads
package impl

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe._
import scala.reflect._
import scalan.common.Default

// Abs -----------------------------------
trait StatesAbs extends States with Scalan {
  self: MonadsDsl =>

  // single proxy for each type family
  implicit def proxyState[S, A](p: Rep[State[S, A]]): State[S, A] = {
    proxyOps[State[S, A]](p)(classTag[State[S, A]])
  }

  // familyElem
  class StateElem[S, A, To <: State[S, A]](implicit val eS: Elem[S], val eA: Elem[A])
    extends EntityElem[To] {
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagS = eS.tag
      implicit val tagA = eA.tag
      weakTypeTag[State[S, A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = {
      val conv = fun {x: Rep[State[S, A]] =>  convertState(x) }
      tryConvert(element[State[S, A]], this, x, conv)
    }

    def convertState(x : Rep[State[S, A]]): Rep[To] = {
      assert(x.selfType1 match { case _: StateElem[_, _, _] => true; case _ => false })
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def stateElement[S, A](implicit eS: Elem[S], eA: Elem[A]): Elem[State[S, A]] =
    new StateElem[S, A, State[S, A]]

  implicit object StateCompanionElem extends CompanionElem[StateCompanionAbs] {
    lazy val tag = weakTypeTag[StateCompanionAbs]
    protected def getDefaultRep = State
  }

  abstract class StateCompanionAbs extends CompanionBase[StateCompanionAbs] with StateCompanion {
    override def toString = "State"
  }
  def State: Rep[StateCompanionAbs]
  implicit def proxyStateCompanion(p: Rep[StateCompanion]): StateCompanion = {
    proxyOps[StateCompanion](p)
  }

  // elem for concrete class
  class StateBaseElem[S, A](val iso: Iso[StateBaseData[S, A], StateBase[S, A]])(implicit eS: Elem[S], eA: Elem[A])
    extends StateElem[S, A, StateBase[S, A]]
    with ConcreteElem[StateBaseData[S, A], StateBase[S, A]] {
    override def convertState(x: Rep[State[S, A]]) = StateBase(x.run)
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
    override lazy val tag = {
      implicit val tagS = eS.tag
      implicit val tagA = eA.tag
      weakTypeTag[StateBase[S, A]]
    }
  }

  // state representation type
  type StateBaseData[S, A] = S => (A, S)

  // 3) Iso for concrete class
  class StateBaseIso[S, A](implicit eS: Elem[S], eA: Elem[A])
    extends Iso[StateBaseData[S, A], StateBase[S, A]] {
    override def from(p: Rep[StateBase[S, A]]) =
      p.run
    override def to(p: Rep[S => (A, S)]) = {
      val run = p
      StateBase(run)
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[StateBase[S, A]]](StateBase(fun { (x: Rep[S]) => Pair(element[A].defaultRepValue, element[S].defaultRepValue) }))
    lazy val eTo = new StateBaseElem[S, A](this)
  }
  // 4) constructor and deconstructor
  abstract class StateBaseCompanionAbs extends CompanionBase[StateBaseCompanionAbs] with StateBaseCompanion {
    override def toString = "StateBase"

    def apply[S, A](run: Rep[S => (A, S)])(implicit eS: Elem[S], eA: Elem[A]): Rep[StateBase[S, A]] =
      mkStateBase(run)
  }
  object StateBaseMatcher {
    def unapply[S, A](p: Rep[State[S, A]]) = unmkStateBase(p)
  }
  def StateBase: Rep[StateBaseCompanionAbs]
  implicit def proxyStateBaseCompanion(p: Rep[StateBaseCompanionAbs]): StateBaseCompanionAbs = {
    proxyOps[StateBaseCompanionAbs](p)
  }

  implicit object StateBaseCompanionElem extends CompanionElem[StateBaseCompanionAbs] {
    lazy val tag = weakTypeTag[StateBaseCompanionAbs]
    protected def getDefaultRep = StateBase
  }

  implicit def proxyStateBase[S, A](p: Rep[StateBase[S, A]]): StateBase[S, A] =
    proxyOps[StateBase[S, A]](p)

  implicit class ExtendedStateBase[S, A](p: Rep[StateBase[S, A]])(implicit eS: Elem[S], eA: Elem[A]) {
    def toData: Rep[StateBaseData[S, A]] = isoStateBase(eS, eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoStateBase[S, A](implicit eS: Elem[S], eA: Elem[A]): Iso[StateBaseData[S, A], StateBase[S, A]] =
    new StateBaseIso[S, A]

  // 6) smart constructor and deconstructor
  def mkStateBase[S, A](run: Rep[S => (A, S)])(implicit eS: Elem[S], eA: Elem[A]): Rep[StateBase[S, A]]
  def unmkStateBase[S, A](p: Rep[State[S, A]]): Option[(Rep[S => (A, S)])]
}

// Seq -----------------------------------
trait StatesSeq extends StatesDsl with ScalanSeq {
  self: MonadsDslSeq =>
  lazy val State: Rep[StateCompanionAbs] = new StateCompanionAbs with UserTypeSeq[StateCompanionAbs] {
    lazy val selfType = element[StateCompanionAbs]
  }

  case class SeqStateBase[S, A]
      (override val run: Rep[S => (A, S)])
      (implicit eS: Elem[S], eA: Elem[A])
    extends StateBase[S, A](run)
        with UserTypeSeq[StateBase[S, A]] {
    lazy val selfType = element[StateBase[S, A]]
  }
  lazy val StateBase = new StateBaseCompanionAbs with UserTypeSeq[StateBaseCompanionAbs] {
    lazy val selfType = element[StateBaseCompanionAbs]
  }

  def mkStateBase[S, A]
      (run: Rep[S => (A, S)])(implicit eS: Elem[S], eA: Elem[A]): Rep[StateBase[S, A]] =
      new SeqStateBase[S, A](run)
  def unmkStateBase[S, A](p: Rep[State[S, A]]) = p match {
    case p: StateBase[S, A] @unchecked =>
      Some((p.run))
    case _ => None
  }
}

// Exp -----------------------------------
trait StatesExp extends StatesDsl with ScalanExp {
  self: MonadsDslExp =>
  lazy val State: Rep[StateCompanionAbs] = new StateCompanionAbs with UserTypeDef[StateCompanionAbs] {
    lazy val selfType = element[StateCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpStateBase[S, A]
      (override val run: Rep[S => (A, S)])
      (implicit eS: Elem[S], eA: Elem[A])
    extends StateBase[S, A](run) with UserTypeDef[StateBase[S, A]] {
    lazy val selfType = element[StateBase[S, A]]
    override def mirror(t: Transformer) = ExpStateBase[S, A](t(run))
  }

  lazy val StateBase: Rep[StateBaseCompanionAbs] = new StateBaseCompanionAbs with UserTypeDef[StateBaseCompanionAbs] {
    lazy val selfType = element[StateBaseCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object StateBaseMethods {
  }

  object StateBaseCompanionMethods {
  }

  def mkStateBase[S, A]
    (run: Rep[S => (A, S)])(implicit eS: Elem[S], eA: Elem[A]): Rep[StateBase[S, A]] =
    new ExpStateBase[S, A](run)
  def unmkStateBase[S, A](p: Rep[State[S, A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: StateBaseElem[S, A] @unchecked =>
      Some((p.asRep[StateBase[S, A]].run))
    case _ =>
      None
  }

  object StateMethods {
    object run {
      def unapply(d: Def[_]): Option[Rep[State[S, A]] forSome {type S; type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[StateElem[_, _, _]] && method.getName == "run" =>
          Some(receiver).asInstanceOf[Option[Rep[State[S, A]] forSome {type S; type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[State[S, A]] forSome {type S; type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object StateCompanionMethods {
    // WARNING: Cannot generate matcher for method `apply`: Method has function arguments r

    object get {
      def unapply(d: Def[_]): Option[Unit forSome {type S}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem == StateCompanionElem && method.getName == "get" =>
          Some(()).asInstanceOf[Option[Unit forSome {type S}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type S}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object set {
      def unapply(d: Def[_]): Option[Rep[S] forSome {type S}] = d match {
        case MethodCall(receiver, method, Seq(s, _*), _) if receiver.elem == StateCompanionElem && method.getName == "set" =>
          Some(s).asInstanceOf[Option[Rep[S] forSome {type S}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[S] forSome {type S}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}
