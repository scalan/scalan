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
  class State0Elem[S, A, To <: State0[S, A]](implicit _eS: Elem[S], _eA: Elem[A])
    extends EntityElem[To] {
    def eS = _eS
    def eA = _eA
    lazy val parent: Option[Elem[_]] = None
    lazy val entityDef: STraitOrClassDef = {
      val module = getModules("States")
      module.entities.find(_.name == "State0").get
    }
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
        case e => !!!(s"Expected $x to have State0Elem[_, _, _], but got $e")
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
  implicit def proxyState0Companion(p: Rep[State0Companion]): State0Companion =
    proxyOps[State0Companion](p)

  abstract class AbsStateBase[S, A]
      (run: Rep[S => (A, S)])(implicit eS: Elem[S], eA: Elem[A])
    extends StateBase[S, A](run) with Def[StateBase[S, A]] {
    lazy val selfType = element[StateBase[S, A]]
  }
  // elem for concrete class
  class StateBaseElem[S, A](val iso: Iso[StateBaseData[S, A], StateBase[S, A]])(implicit eS: Elem[S], eA: Elem[A])
    extends State0Elem[S, A, StateBase[S, A]]
    with ConcreteElem[StateBaseData[S, A], StateBase[S, A]] {
    override lazy val parent: Option[Elem[_]] = Some(state0Element(element[S], element[A]))
    override lazy val entityDef = {
      val module = getModules("States")
      module.concreteSClasses.find(_.name == "StateBase").get
    }
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
    extends Iso[StateBaseData[S, A], StateBase[S, A]] {
    override def from(p: Rep[StateBase[S, A]]) =
      p.run
    override def to(p: Rep[S => (A, S)]) = {
      val run = p
      StateBase(run)
    }
    lazy val eTo = new StateBaseElem[S, A](this)
  }
  // 4) constructor and deconstructor
  class StateBaseCompanionAbs extends CompanionDef[StateBaseCompanionAbs] with StateBaseCompanion {
    def selfType = StateBaseCompanionElem
    override def toString = "StateBase"

    def apply[S, A](run: Rep[S => (A, S)])(implicit eS: Elem[S], eA: Elem[A]): Rep[StateBase[S, A]] =
      mkStateBase(run)
  }
  object StateBaseMatcher {
    def unapply[S, A](p: Rep[State0[S, A]]) = unmkStateBase(p)
  }
  lazy val StateBase: Rep[StateBaseCompanionAbs] = new StateBaseCompanionAbs
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
    cachedIso[StateBaseIso[S, A]](eS, eA)

  // 6) smart constructor and deconstructor
  def mkStateBase[S, A](run: Rep[S => (A, S)])(implicit eS: Elem[S], eA: Elem[A]): Rep[StateBase[S, A]]
  def unmkStateBase[S, A](p: Rep[State0[S, A]]): Option[(Rep[S => (A, S)])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(States_Module.dump))
}

// Seq -----------------------------------
trait StatesSeq extends StatesDsl with scalan.ScalanSeq {
  self: MonadsDslSeq =>
  lazy val State0: Rep[State0CompanionAbs] = new State0CompanionAbs {
  }

  case class SeqStateBase[S, A]
      (override val run: Rep[S => (A, S)])(implicit eS: Elem[S], eA: Elem[A])
    extends AbsStateBase[S, A](run) {
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

object States_Module {
  val packageName = "scalan.monads"
  val name = "States"
  val dump = "H4sIAAAAAAAAALVWTWwbRRR+XsdxbIemFFRUqjYhMiAqsKMKqYccKid1oJXzo2wOlamKxuuxu2V2drMzjmwOFeKE4Ia4cECo9964ICFxQ0IcOFWAxIkDp9IeKmjVAxVvZn+deENVxB5GO7Nv38/3fe/t3roLBeHDK8IijPCaQyWpmfq+IWTVbHJpy9G62x0weoH2Pjz+tbXOV4QBc22YvkbEBcHaUApumkMvvjfpbgtKhFtUSNcXEl5q6Qh1y2WMWtJ2ed12nIEkHUbrLVvI5RZMddzuaBduQK4FRy2XWz6V1FxlRAgqwvMZqjKy431J70ebXhKD11UV9VQVOz6xJaaPMY4G9tvUM0fc5SNHwpEwtU1PpYU2RdvxXF9GIYro7prbjbZTnOABHGtdJ3ukjiH6dVP6Nu/jmxWPWO+RPt1AE2U+hQkLyno7I0/v8y0oC7qLAF10PKZPhh4AIANndRK1BJ9ajE9N4VM1qW8TZr9P1MMt3x2OILhyeYChhy5e/xcXkQfa5N3qx1esdx6aFcdQLw9VKkVd4TQ6ms9Qg6YCcfx++1Nx/62b5wwot6Fsi0ZHSJ9YMk15iFaFcO5KnXMMIPH7yNZiFls6SgNt9kmiZLmORzh6CqGcRZ6YbdlSGauz2ZCdDOiL0qORaW7o5eJ6FzLq1bpZJYxt3Tnxxst/NC8bYIyHKKFLE4XvR04lTJtYLl0Knat1TkLOTBBW24beqqU0TNbiIbnEqLx65173uyW4YsRYhqGfjD50URC//FS5/dp5A2baWuxrjPTbCKdoMups+qsul22YcfeoHzwp7hGm7ibSWezSHhkwGYKcRieP6EhYyGxLjyrolnUL5CIAKoGKN1xOq2tb1QfmD5/dUiL1YTZ4EvTpY/vc378e6UmtXwl5f8AjdPPY3TEYp7O49ejagFu3L37+3Nypd3/TzE53XYfYWl4nW1Dwsbd1KSdDcFNEZqOMjncGHqNvfvPo6kcfvO1png7oZJ88GuPyMCfKIxFJOUDCdB367OJ9++rNT6QOkxuOD6bNznWcBMv6vdOH5RwOyL/aS8afJ37+0oASCqBjS4d41aUnbOv/sVVhHK65oMVW00FSaBXjZR6VcUwbrxBBx+znE8RfSHl/MRdJURtJMKgZhZ1S7TGxr4PYWQ4ahzk4OAkklOKUtY9Yy6eyJYdgHd9uPc/unv/WgMIlKPSwXwWKuOMOeDdiAT+qkg7lSnSWG2cBUSc+cWLU9bUACVzjg+zSRIODBaUqPruPyTyKbvzkP4zPNPvatB7GHBdFrIxJMgrKSMZ4moDsmp8CFLVuJTahYRAVv47PRFS7nHRFWI0PixkKMMP2QThvPPxi48yPX/2uJ1pZNSJOUh7/zSR87x9CpXUdC39OUsmiaFVr6kT/AfsglOMsCgAA"
}
}

