package scalan.monads

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait StatesAbs extends scalan.ScalanDsl with States {
  self: MonadsDsl =>

  // single proxy for each type family
  implicit def proxyState0[S, A](p: Rep[State0[S, A]]): State0[S, A] = {
    proxyOps[State0[S, A]](p)(scala.reflect.classTag[State0[S, A]])
  }

  // familyElem
  class State0Elem[S, A, To <: State0[S, A]](implicit _eS: Elem[S], _eA: Elem[A])
    extends EntityElem[To] {
    def eS = _eS
    def eA = _eA
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("S" -> Left(eS), "A" -> Left(eA))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagS = eS.tag
      implicit val tagA = eA.tag
      weakTypeTag[State0[S, A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[State0[S, A]] => convertState0(x) }
      tryConvert(element[State0[S, A]], this, x, conv)
    }

    def convertState0(x: Rep[State0[S, A]]): Rep[To] = {
      x.selfType1 match {
        case _: State0Elem[_, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have State0Elem[_, _, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def state0Element[S, A](implicit eS: Elem[S], eA: Elem[A]): Elem[State0[S, A]] =
    cachedElem[State0Elem[S, A, State0[S, A]]](eS, eA)

  implicit case object State0CompanionElem extends CompanionElem[State0CompanionAbs] {
    lazy val tag = weakTypeTag[State0CompanionAbs]
    protected def getDefaultRep = State0
  }

  abstract class State0CompanionAbs extends CompanionDef[State0CompanionAbs] with State0Companion {
    def selfType = State0CompanionElem
    override def toString = "State0"
  }
  def State0: Rep[State0CompanionAbs]
  implicit def proxyState0CompanionAbs(p: Rep[State0CompanionAbs]): State0CompanionAbs =
    proxyOps[State0CompanionAbs](p)

  abstract class AbsStateBase[S, A]
      (run: Rep[S => (A, S)])(implicit eS: Elem[S], eA: Elem[A])
    extends StateBase[S, A](run) with Def[StateBase[S, A]] {
    lazy val selfType = element[StateBase[S, A]]
  }
  // elem for concrete class
  class StateBaseElem[S, A](val iso: Iso[StateBaseData[S, A], StateBase[S, A]])(implicit override val eS: Elem[S], override val eA: Elem[A])
    extends State0Elem[S, A, StateBase[S, A]]
    with ConcreteElem[StateBaseData[S, A], StateBase[S, A]] {
    override lazy val parent: Option[Elem[_]] = Some(state0Element(element[S], element[A]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("S" -> Left(eS), "A" -> Left(eA))
    }

    override def convertState0(x: Rep[State0[S, A]]) = StateBase(x.run)
    override def getDefaultRep = StateBase(constFun[S, (A, S)](Pair(element[A].defaultRepValue, element[S].defaultRepValue)))
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
    extends EntityIso[StateBaseData[S, A], StateBase[S, A]] with Def[StateBaseIso[S, A]] {
    override def from(p: Rep[StateBase[S, A]]) =
      p.run
    override def to(p: Rep[S => (A, S)]) = {
      val run = p
      StateBase(run)
    }
    lazy val eFrom = element[S => (A, S)]
    lazy val eTo = new StateBaseElem[S, A](self)
    lazy val selfType = new StateBaseIsoElem[S, A](eS, eA)
    def productArity = 2
    def productElement(n: Int) = (eS, eA).productElement(n)
  }
  case class StateBaseIsoElem[S, A](eS: Elem[S], eA: Elem[A]) extends Elem[StateBaseIso[S, A]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new StateBaseIso[S, A]()(eS, eA))
    lazy val tag = {
      implicit val tagS = eS.tag
      implicit val tagA = eA.tag
      weakTypeTag[StateBaseIso[S, A]]
    }
  }
  // 4) constructor and deconstructor
  class StateBaseCompanionAbs extends CompanionDef[StateBaseCompanionAbs] with StateBaseCompanion {
    def selfType = StateBaseCompanionElem
    override def toString = "StateBase"

    @scalan.OverloadId("fromFields")
    def apply[S, A](run: Rep[S => (A, S)])(implicit eS: Elem[S], eA: Elem[A]): Rep[StateBase[S, A]] =
      mkStateBase(run)

    def unapply[S, A](p: Rep[State0[S, A]]) = unmkStateBase(p)
  }
  lazy val StateBaseRep: Rep[StateBaseCompanionAbs] = new StateBaseCompanionAbs
  lazy val StateBase: StateBaseCompanionAbs = proxyStateBaseCompanion(StateBaseRep)
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
    reifyObject(new StateBaseIso[S, A]()(eS, eA))

  // 6) smart constructor and deconstructor
  def mkStateBase[S, A](run: Rep[S => (A, S)])(implicit eS: Elem[S], eA: Elem[A]): Rep[StateBase[S, A]]
  def unmkStateBase[S, A](p: Rep[State0[S, A]]): Option[(Rep[S => (A, S)])]

  registerModule(States_Module)
}

// Std -----------------------------------
trait StatesStd extends scalan.ScalanDslStd with StatesDsl {
  self: MonadsDslStd =>
  lazy val State0: Rep[State0CompanionAbs] = new State0CompanionAbs {
  }

  case class StdStateBase[S, A]
      (override val run: Rep[S => (A, S)])(implicit eS: Elem[S], eA: Elem[A])
    extends AbsStateBase[S, A](run) {
  }

  def mkStateBase[S, A]
    (run: Rep[S => (A, S)])(implicit eS: Elem[S], eA: Elem[A]): Rep[StateBase[S, A]] =
    new StdStateBase[S, A](run)
  def unmkStateBase[S, A](p: Rep[State0[S, A]]) = p match {
    case p: StateBase[S, A] @unchecked =>
      Some((p.run))
    case _ => None
  }
}

// Exp -----------------------------------
trait StatesExp extends scalan.ScalanDslExp with StatesDsl {
  self: MonadsDslExp =>
  lazy val State0: Rep[State0CompanionAbs] = new State0CompanionAbs {
  }

  case class ExpStateBase[S, A]
      (override val run: Rep[S => (A, S)])(implicit eS: Elem[S], eA: Elem[A])
    extends AbsStateBase[S, A](run)

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
      def unapply(d: Def[_]): Option[(Rep[State0[S, A]], Rep[S]) forSome {type S; type A}] = d match {
        case MethodCall(receiver, method, Seq(t, s, _*), _) if receiver.elem == State0CompanionElem && method.getName == "run" =>
          Some((t, s)).asInstanceOf[Option[(Rep[State0[S, A]], Rep[S]) forSome {type S; type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[State0[S, A]], Rep[S]) forSome {type S; type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object eval {
      def unapply(d: Def[_]): Option[(Rep[State0[S, A]], Rep[S]) forSome {type S; type A}] = d match {
        case MethodCall(receiver, method, Seq(t, s, _*), _) if receiver.elem == State0CompanionElem && method.getName == "eval" =>
          Some((t, s)).asInstanceOf[Option[(Rep[State0[S, A]], Rep[S]) forSome {type S; type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[State0[S, A]], Rep[S]) forSome {type S; type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object States_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWS2wbRRge23Fsx6EpLQ9B1CaNzKMI7Kgg9WChynUdSHEeZHNApmo13h2nW2Znh51xZHMonCIEN8SFA4cKEEKqkBBHJE4gIYQ4oKqqBBcOnEoRyqEVBxD/zD68drzhJfYwmpn95398//f/M1dvoazw0MPCxBSzskMkLht6XhOyZDSYtGV/xbW6lJwhneLt6rEfdurLaTTTQpMXsTgjaAsV/Emjx6O5Ia0mKmBmEiFdT0h0rKktVEyXUmJK22UV23G6ErcpqTRtIatNNNF2rf7L6DJKNdFB02WmRyQx6hQLQUSwnyfKIztaF/S6v8YHNlhFRVGJRbHpYVuC+2DjoC+/QbjRZy7rOxIdCFxb48otkMnZDnc9GZrIgbqLrhUuJxiGDXSoeQlv4wqY2KoY0rPZFpwscmy+hLfIKogo8QlwWBDa2exzvc6AiJAWALTscKo1ZnocIQQpOKG9KA8AKkcAlRVAJYN4Nqb2K1j9XPfcXh/5XyqDUI+Disf/QkWogTSYVXrjnPniHaPopNXhnnIlpx2aBEVzCXTQuQAgv9p4S+w+c+VkGk210JQtam0hPWzKeM4DuIqYMVdqnyMEsbcF6VpISpe2UgOZEU4UTNfhmIGmAMtpSBS1TVsqYbU3HaQnAfuc5CQUTfV4Kop3PiFeTZw6pnT95gNPPPRz44U0Sg+bKIBKA5jvhUolmjQgXLIYKFfjjEQpY4CwWtb0Ug2F3mDM7eNLhMojN3+xvlxE59IRloHpv5c+UJEVN64Xrx0/lUb5lmb7EsVbLYBTNChx1ry6y2QL5d1t4vl/ctuYqtnYdOYs0sFdKgOQ4+hkAB2J5hPrkhMFXVWXQCoEoOizeNVlpLS0XrptfP32VUVSD037f/xC/cM++fv3BzpS81eijNdlIboZKO8IjKNJueVkqcvMa8vvHJ45cuFHndlJy3Wwrek120RZD4pbhzIbgBtLZDLKoHizyyl56rPfzr/+2rNc52kPT0boURumhzGWHgOSTPlIGK5D7l7Ytc9feVNqM6necGdaa1+CTlDV547u53PQIT/Z2bn31/cvHNaFnW/b0sG8tPgPyjqswv+xbNEwdDN+udXjRnJxtNR4f7SrhzkgzCF97jQWZOjoXOxQzNCDqZChWkiiNDFCDyZU1Ywtd9+NJAW1/RTsbRASFSKXtY6I4keSmQi43bfRvIfeOvV5GmXPomwHylgAt9tul1lhQuCylaQnT4d7qeGEQAKwh50oAfqbRwO4hvvbc2MF9gYUi/jESFIzwMXhnf/QVROIoNeVwPzI9hi+jOOZH9yg58fTkozEv4BKjc8PZAJB3ypcpXeFBHAZtkQQmIcWEnhhBPUFIF++8+7qY99++pNuf1OqUqHtsujtM2DBaMcqrGhb8JSJOQtUVrUbOXA8yQFpqQcQccCU30N2z35UmP24/Z7uYnn1RiImFdHdMf7duIJ5VT98Ht3n4QNCpYbD4RULkye/ePq7V7/58AN9afwJsBHGe/wKAAA="
}
}

