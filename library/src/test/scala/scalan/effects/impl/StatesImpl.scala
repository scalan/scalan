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
    override def convert(x: Rep[Reifiable[_]]) = {
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
    cachedIso[StateBaseIso[S, A]](eS, eA)

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
  val dump = "H4sIAAAAAAAAALVWTWwbRRR+Xsdx1g5NKaioVG1CZEAgsKMKqYccKid1oJXzo2wOlamKxuuxu2V3ZrMzjmwOFeKE4Ia4cECo9964ICFxQ0IcOFWAxIkDp9IeKqDiQMWb2V8n2VCB2MNoZ/bt+/m+773d2/egJAJ4QdjEJazuUUnqlr5vClmzWkw6crzOe0OXXqT9905+Ya+zFWHAXAemrxNxUbgdMMOb1shP7i262waTMJsKyQMh4bm2jtCwuetSWzqcNRzPG0rSdWmj7Qi53IapLu+Nd+EmFNpw3ObMDqik1qpLhKAiOp+hKiMn2Zt6P9700xisoapoZKrYCYgjMX2McTy036a+NWacjT0Jx6LUNn2VFtqUHc/ngYxDlNHddd6Lt1OM4AGcaN8ge6SBIQYNSwYOG+CbVZ/Yb5MB3UATZT6FCQvq9nfGvt4X21ARdBcBuuT5rj4Z+QCADJzTSdRTfOoJPnWFT82igUNc5x2iHm4FfDSG8CoUAUY+unjlH1zEHmiL9WofXLXffGhVPUO9PFKplHWF0+hoPkcNmgrE8Zvtj8SD12+dN6DSgYojml0hA2LLLOURWlXCGJc65wRAEgyQrcU8tnSUJtrsk4Rpc88nDD1FUM4iT65jO1IZq7PZiJ0c6MvSp7FpYeQXknoXcurVulklrrt199Srz//aumKAMRnCRJcWCj+InUqYtrBcuhQ5V+uchIKVIqy2Tb1VizlK1/IRuSSovHj3fu/rJbhqJFhGoR+PPnRREj9+X73z0gUDZjpa7GsuGXQQTtFyqbcZrHImOzDD92gQPinvEVfdHUpnuUf7ZOjKCOQsOkVER8JCblv6VEG3rFugEANQDVW8wRmtrW3V/rC+/fi2EmkAs+GTsE8fOef/+ulYX2r9SigGQxajW8TuTsA4m8etT9eGzL5z6ZOn5s689bNmdrrHPeJoeZ1uQynA3talnI7AzRCZjzI63hn6Ln3tyz+vvf/uG77m6YBO9smjOSkP61B5pCKphEhY3KNPLj5wrt36UOowhdHkYNrs3sBJsKzfO3tUztGA/L2zZPx26ofPDDBRAF1HesSvLT1mW/+PrQqTcM2FLbaaDZJBq5ws86iME9p4hQg6YT+fIv5MxvuzhViK2kiCQa047JRqj0P7Ooyd56B5lIODk0CCmaSsfSRaPpMvOQTr5Hb7affeha8MKF2GUh/7VaCIu3zIejEL+FGVdCRX4rPCJAuIOgmIl6CurwVI4ZocZJcPNThYUKbic/uYNLep03fUF2ry/D8M0awGtGkjijwpjUQfh4kpLCYd5lka8iv/F9CodSu1iQzDqPiNfCImnDPSE1E1ASzm6MCKmgg7+ebDTzde/u7zX/Rcq6h2xHnKkn+alPX9o8hc17HwFyWTLEpXNahO9G+6x9XYMgoAAA=="
}
}

