package scalan.monads

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait StatesAbs extends States with scalan.Scalan {
  self: MonadsDsl =>

  // single proxy for each type family
  implicit def proxyState0[S, A](p: Rep[State0[S, A]]): State0[S, A] = {
    proxyOps[State0[S, A]](p)(scala.reflect.classTag[State0[S, A]])
  }

  // familyElem
  class State0Elem[S, A, To <: State0[S, A]](implicit val eS: Elem[S], val eA: Elem[A])
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    lazy val entityDef: STraitOrClassDef = {
      val module = getModules("States")
      module.entities.find(_.name == "State0").get
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagS = eS.tag
      implicit val tagA = eA.tag
      weakTypeTag[State0[S, A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[State0[S, A]] => convertState0(x) }
      tryConvert(element[State0[S, A]], this, x, conv)
    }

    def convertState0(x : Rep[State0[S, A]]): Rep[To] = {
      assert(x.selfType1 match { case _: State0Elem[_, _, _] => true; case _ => false })
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def state0Element[S, A](implicit eS: Elem[S], eA: Elem[A]): Elem[State0[S, A]] =
    new State0Elem[S, A, State0[S, A]]

  implicit case object State0CompanionElem extends CompanionElem[State0CompanionAbs] {
    lazy val tag = weakTypeTag[State0CompanionAbs]
    protected def getDefaultRep = State0
  }

  abstract class State0CompanionAbs extends CompanionBase[State0CompanionAbs] with State0Companion {
    override def toString = "State0"
  }
  def State0: Rep[State0CompanionAbs]
  implicit def proxyState0Companion(p: Rep[State0Companion]): State0Companion =
    proxyOps[State0Companion](p)

  // elem for concrete class
  class StateBaseElem[S, A](val iso: Iso[StateBaseData[S, A], StateBase[S, A]])(implicit eS: Elem[S], eA: Elem[A])
    extends State0Elem[S, A, StateBase[S, A]]
    with ConcreteElem[StateBaseData[S, A], StateBase[S, A]] {
    override lazy val parent: Option[Elem[_]] = Some(state0Element(element[S], element[A]))
    override lazy val entityDef = {
      val module = getModules("States")
      module.concreteSClasses.find(_.name == "StateBase").get
    }

    override def convertState0(x: Rep[State0[S, A]]) = StateBase(x.run)
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
    lazy val defaultRepTo: Rep[StateBase[S, A]] = StateBase(fun { (x: Rep[S]) => Pair(element[A].defaultRepValue, element[S].defaultRepValue) })
    lazy val eTo = new StateBaseElem[S, A](this)
  }
  // 4) constructor and deconstructor
  abstract class StateBaseCompanionAbs extends CompanionBase[StateBaseCompanionAbs] with StateBaseCompanion {
    override def toString = "StateBase"

    def apply[S, A](run: Rep[S => (A, S)])(implicit eS: Elem[S], eA: Elem[A]): Rep[StateBase[S, A]] =
      mkStateBase(run)
  }
  object StateBaseMatcher {
    def unapply[S, A](p: Rep[State0[S, A]]) = unmkStateBase(p)
  }
  def StateBase: Rep[StateBaseCompanionAbs]
  implicit def proxyStateBaseCompanion(p: Rep[StateBaseCompanionAbs]): StateBaseCompanionAbs = {
    proxyOps[StateBaseCompanionAbs](p)
  }

  implicit case object StateBaseCompanionElem extends CompanionElem[StateBaseCompanionAbs] {
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
  def unmkStateBase[S, A](p: Rep[State0[S, A]]): Option[(Rep[S => (A, S)])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(States_Module.dump))
}

// Seq -----------------------------------
trait StatesSeq extends StatesDsl with scalan.ScalanSeq {
  self: MonadsDslSeq =>
  lazy val State0: Rep[State0CompanionAbs] = new State0CompanionAbs with UserTypeSeq[State0CompanionAbs] {
    lazy val selfType = element[State0CompanionAbs]
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
  def unmkStateBase[S, A](p: Rep[State0[S, A]]) = p match {
    case p: StateBase[S, A] @unchecked =>
      Some((p.run))
    case _ => None
  }
}

// Exp -----------------------------------
trait StatesExp extends StatesDsl with scalan.ScalanExp {
  self: MonadsDslExp =>
  lazy val State0: Rep[State0CompanionAbs] = new State0CompanionAbs with UserTypeDef[State0CompanionAbs] {
    lazy val selfType = element[State0CompanionAbs]
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
  def unmkStateBase[S, A](p: Rep[State0[S, A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: StateBaseElem[S, A] @unchecked =>
      Some((p.asRep[StateBase[S, A]].run))
    case _ =>
      None
  }

  object State0Methods {
    object run {
      def unapply(d: Def[_]): Option[Rep[State0[S, A]] forSome {type S; type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[State0Elem[_, _, _]] && method.getName == "run" =>
          Some(receiver).asInstanceOf[Option[Rep[State0[S, A]] forSome {type S; type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[State0[S, A]] forSome {type S; type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object State0CompanionMethods {
    object unit {
      def unapply(d: Def[_]): Option[Rep[A] forSome {type S; type A}] = d match {
        case MethodCall(receiver, method, Seq(a, _*), _) if receiver.elem == State0CompanionElem && method.getName == "unit" =>
          Some(a).asInstanceOf[Option[Rep[A] forSome {type S; type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[A] forSome {type S; type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object get {
      def unapply(d: Def[_]): Option[Unit forSome {type S}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem == State0CompanionElem && method.getName == "get" =>
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
        case MethodCall(receiver, method, Seq(s, _*), _) if receiver.elem == State0CompanionElem && method.getName == "set" =>
          Some(s).asInstanceOf[Option[Rep[S] forSome {type S}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[S] forSome {type S}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object run {
      def unapply(d: Def[_]): Option[(Rep[State0[S,A]], Rep[S]) forSome {type S; type A}] = d match {
        case MethodCall(receiver, method, Seq(t, s, _*), _) if receiver.elem == State0CompanionElem && method.getName == "run" =>
          Some((t, s)).asInstanceOf[Option[(Rep[State0[S,A]], Rep[S]) forSome {type S; type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[State0[S,A]], Rep[S]) forSome {type S; type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object eval {
      def unapply(d: Def[_]): Option[(Rep[State0[S,A]], Rep[S]) forSome {type S; type A}] = d match {
        case MethodCall(receiver, method, Seq(t, s, _*), _) if receiver.elem == State0CompanionElem && method.getName == "eval" =>
          Some((t, s)).asInstanceOf[Option[(Rep[State0[S,A]], Rep[S]) forSome {type S; type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[State0[S,A]], Rep[S]) forSome {type S; type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object States_Module {
  val packageName = "scalan.monads"
  val name = "States"
  val dump = "H4sIAAAAAAAAALVWTWwbRRQer5M4a4emlAMqVZsSmSIQ2FGFVKQckJs4tMhNomwOyK2KxutxMmF3ZjMzjmwOFUd+xKXighBCPXDrjSNnJMSBU0WROHHgVIpEBVQcQLyZ/U+yoQjhw2hn5u37+b7vvfXt+2hSCnROutjDrOEThRuOeW5JVXfaTFE1vsL7Q48sk8Hyzt2XxWfbH1hotoumtrFcll4X2eFDexQkzw7Z7SAbM5dIxYVU6OmOidB0uecRV1HOmtT3hwr3PNLsUKkWO2iix/vjXXQDlTrouMuZK4gizpKHpSQyOp8mOiOa7G2zH68FaQzW1FU0M1VsCkwVpA8xjof2GyRwxoyzsa/QsSi1tUCnBTYV6gdcqDhEBdxt8368nWAYDtCJzg7ew00IsdV0lKBsC96sBdh9E2+RVTDR5hOQsCTeYHMcmH25g6qS7AJAl/3AMyejACEEDJw3STRSfBoJPg2NT90hgmKPvoX15brgozEKf6UyQqMAXLzwDy5iD6TN+vX3rrlXHzo139Ivj3QqFVPhFDiaK1CDoQJw/Grjpnzw6q0LFqp2UZXKVk8qgV2VpTxCq4YZ48rknACIxRawNV/ElonSApt9krBd7geYgacIyhngyaMuVdpYn81E7BRAX1EBiU1Lo6CU1Hu2oF6jmyXseev3Tr74zE/t1y1k5UPY4NIB4YvYqUJTDpRLFiLnep1VqOSkCOtty2z1Yo/StXJELgkqz977uf/lArpmJVhGoR+NPnAxKb/7tnbnuVcsNN01Yl/x8FYX4JRtj/hrYokz1UXTfI+I8Kayhz39dCidlT4Z4KGnIpCz6JQBHYXOFrZlQDR0i6YFSjEAtVDFq5yR+sp6/Xfn6w9va5EKNBPehH36F73w5/fHBsroV6GyGLIY3TJ0dwLGmSJuA7IyZO6dyx89MXv6jR8Ms1N97mNq5HWqgyYF9LYp5VQEbobIYpTB8eYw8MhLX/xx/Z23LwWGpwM62SePVl4ezqHySEVSDZFwuE8en39Ar996X5kwpVF+MK31dmASLJr3zhyVczQgf+suWL+evPuphWwQQI8qHwf1hUds6/+xVVEertmwxZayQTJoVZJlDpRxwhhfxJLk7OdSxJ/MeH+qFEvRGClkEScOO6Hb49C+DmMXOWgd5eDgJFDITlI2PhItny6WHIB18+q5S+KXj9+1NKCTPT5k/Rh9+JgqMlIX47NSHn1AGwvsx2in8OQH12u5i4OJZyo7v48xe4PQAdVfovz5fxiWWa6NaTOKnJdAooPDRBMWkw7tLNwHK/4XUOh1PbWJDMMo8O17LCaSM9yXUfYCzRfw60TNAR164+Enq89/8/mPZl5VdZvBnGTJf5WU1f0jxr5iYsFfj0yyIEndeCbRvwHIheMPCgoAAA=="
}
}

