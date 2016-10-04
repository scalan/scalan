package scalan.monads

import scalan._
import scala.reflect.runtime.universe._
import scalan.collections.CollectionsDslExp
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait FreeStatesAbs extends scalan.ScalanDsl with FreeStates {
  self: MonadsDsl =>

  // single proxy for each type family
  implicit def proxyStateF[S, A](p: Rep[StateF[S, A]]): StateF[S, A] = {
    proxyOps[StateF[S, A]](p)(scala.reflect.classTag[StateF[S, A]])
  }

  // familyElem
  class StateFElem[S, A, To <: StateF[S, A]](implicit _eS: Elem[S], _eA: Elem[A])
    extends EntityElem[To] {
    def eS = _eS
    def eA = _eA
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = TypeArgs("S" -> eS, "A" -> eA)
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagS = eS.tag
      implicit val tagA = eA.tag
      weakTypeTag[StateF[S, A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[StateF[S, A]] => convertStateF(x) }
      tryConvert(element[StateF[S, A]], this, x, conv)
    }

    def convertStateF(x: Rep[StateF[S, A]]): Rep[To] = {
      x.selfType1 match {
        case _: StateFElem[_, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have StateFElem[_, _, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def stateFElement[S, A](implicit eS: Elem[S], eA: Elem[A]): Elem[StateF[S, A]] =
    cachedElem[StateFElem[S, A, StateF[S, A]]](eS, eA)

  implicit case object StateFCompanionElem extends CompanionElem[StateFCompanionAbs] {
    lazy val tag = weakTypeTag[StateFCompanionAbs]
    protected def getDefaultRep = StateF
  }

  abstract class StateFCompanionAbs extends CompanionDef[StateFCompanionAbs] with StateFCompanion {
    def selfType = StateFCompanionElem
    override def toString = "StateF"
  }
  def StateF: Rep[StateFCompanionAbs]
  implicit def proxyStateFCompanionAbs(p: Rep[StateFCompanionAbs]): StateFCompanionAbs =
    proxyOps[StateFCompanionAbs](p)

  abstract class AbsStateGet[S, A]
      (f: Rep[S => A])(implicit eS: Elem[S], eA: Elem[A])
    extends StateGet[S, A](f) with Def[StateGet[S, A]] {
    lazy val selfType = element[StateGet[S, A]]
  }
  // elem for concrete class
  class StateGetElem[S, A](val iso: Iso[StateGetData[S, A], StateGet[S, A]])(implicit override val eS: Elem[S], override val eA: Elem[A])
    extends StateFElem[S, A, StateGet[S, A]]
    with ConcreteElem[StateGetData[S, A], StateGet[S, A]] {
    override lazy val parent: Option[Elem[_]] = Some(stateFElement(element[S], element[A]))
    override lazy val typeArgs = TypeArgs("S" -> eS, "A" -> eA)

    override def convertStateF(x: Rep[StateF[S, A]]) = // Converter is not generated by meta
!!!("Cannot convert from StateF to StateGet: missing fields List(f)")
    override def getDefaultRep = StateGet(constFun[S, A](element[A].defaultRepValue))
    override lazy val tag = {
      implicit val tagS = eS.tag
      implicit val tagA = eA.tag
      weakTypeTag[StateGet[S, A]]
    }
  }

  // state representation type
  type StateGetData[S, A] = S => A

  // 3) Iso for concrete class
  class StateGetIso[S, A](implicit eS: Elem[S], eA: Elem[A])
    extends EntityIso[StateGetData[S, A], StateGet[S, A]] with Def[StateGetIso[S, A]] {
    override def from(p: Rep[StateGet[S, A]]) =
      p.f
    override def to(p: Rep[S => A]) = {
      val f = p
      StateGet(f)
    }
    lazy val eFrom = element[S => A]
    lazy val eTo = new StateGetElem[S, A](self)
    lazy val selfType = new StateGetIsoElem[S, A](eS, eA)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eS
      case 1 => eA
    }
  }
  case class StateGetIsoElem[S, A](eS: Elem[S], eA: Elem[A]) extends Elem[StateGetIso[S, A]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new StateGetIso[S, A]()(eS, eA))
    lazy val tag = {
      implicit val tagS = eS.tag
      implicit val tagA = eA.tag
      weakTypeTag[StateGetIso[S, A]]
    }
    lazy val typeArgs = TypeArgs("S" -> eS, "A" -> eA)
  }
  // 4) constructor and deconstructor
  class StateGetCompanionAbs extends CompanionDef[StateGetCompanionAbs] with StateGetCompanion {
    def selfType = StateGetCompanionElem
    override def toString = "StateGet"

    @scalan.OverloadId("fromFields")
    def apply[S, A](f: Rep[S => A])(implicit eS: Elem[S], eA: Elem[A]): Rep[StateGet[S, A]] =
      mkStateGet(f)

    def unapply[S, A](p: Rep[StateF[S, A]]) = unmkStateGet(p)
  }
  lazy val StateGetRep: Rep[StateGetCompanionAbs] = new StateGetCompanionAbs
  lazy val StateGet: StateGetCompanionAbs = proxyStateGetCompanion(StateGetRep)
  implicit def proxyStateGetCompanion(p: Rep[StateGetCompanionAbs]): StateGetCompanionAbs = {
    proxyOps[StateGetCompanionAbs](p)
  }

  implicit case object StateGetCompanionElem extends CompanionElem[StateGetCompanionAbs] {
    lazy val tag = weakTypeTag[StateGetCompanionAbs]
    protected def getDefaultRep = StateGet
  }

  implicit def proxyStateGet[S, A](p: Rep[StateGet[S, A]]): StateGet[S, A] =
    proxyOps[StateGet[S, A]](p)

  implicit class ExtendedStateGet[S, A](p: Rep[StateGet[S, A]])(implicit eS: Elem[S], eA: Elem[A]) {
    def toData: Rep[StateGetData[S, A]] = isoStateGet(eS, eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoStateGet[S, A](implicit eS: Elem[S], eA: Elem[A]): Iso[StateGetData[S, A], StateGet[S, A]] =
    reifyObject(new StateGetIso[S, A]()(eS, eA))

  // 6) smart constructor and deconstructor
  def mkStateGet[S, A](f: Rep[S => A])(implicit eS: Elem[S], eA: Elem[A]): Rep[StateGet[S, A]]
  def unmkStateGet[S, A](p: Rep[StateF[S, A]]): Option[(Rep[S => A])]

  abstract class AbsStatePut[S, A]
      (s: Rep[S], a: Rep[A])(implicit eS: Elem[S], eA: Elem[A])
    extends StatePut[S, A](s, a) with Def[StatePut[S, A]] {
    lazy val selfType = element[StatePut[S, A]]
  }
  // elem for concrete class
  class StatePutElem[S, A](val iso: Iso[StatePutData[S, A], StatePut[S, A]])(implicit override val eS: Elem[S], override val eA: Elem[A])
    extends StateFElem[S, A, StatePut[S, A]]
    with ConcreteElem[StatePutData[S, A], StatePut[S, A]] {
    override lazy val parent: Option[Elem[_]] = Some(stateFElement(element[S], element[A]))
    override lazy val typeArgs = TypeArgs("S" -> eS, "A" -> eA)

    override def convertStateF(x: Rep[StateF[S, A]]) = // Converter is not generated by meta
!!!("Cannot convert from StateF to StatePut: missing fields List(s, a)")
    override def getDefaultRep = StatePut(element[S].defaultRepValue, element[A].defaultRepValue)
    override lazy val tag = {
      implicit val tagS = eS.tag
      implicit val tagA = eA.tag
      weakTypeTag[StatePut[S, A]]
    }
  }

  // state representation type
  type StatePutData[S, A] = (S, A)

  // 3) Iso for concrete class
  class StatePutIso[S, A](implicit eS: Elem[S], eA: Elem[A])
    extends EntityIso[StatePutData[S, A], StatePut[S, A]] with Def[StatePutIso[S, A]] {
    override def from(p: Rep[StatePut[S, A]]) =
      (p.s, p.a)
    override def to(p: Rep[(S, A)]) = {
      val Pair(s, a) = p
      StatePut(s, a)
    }
    lazy val eFrom = pairElement(element[S], element[A])
    lazy val eTo = new StatePutElem[S, A](self)
    lazy val selfType = new StatePutIsoElem[S, A](eS, eA)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eS
      case 1 => eA
    }
  }
  case class StatePutIsoElem[S, A](eS: Elem[S], eA: Elem[A]) extends Elem[StatePutIso[S, A]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new StatePutIso[S, A]()(eS, eA))
    lazy val tag = {
      implicit val tagS = eS.tag
      implicit val tagA = eA.tag
      weakTypeTag[StatePutIso[S, A]]
    }
    lazy val typeArgs = TypeArgs("S" -> eS, "A" -> eA)
  }
  // 4) constructor and deconstructor
  class StatePutCompanionAbs extends CompanionDef[StatePutCompanionAbs] with StatePutCompanion {
    def selfType = StatePutCompanionElem
    override def toString = "StatePut"
    @scalan.OverloadId("fromData")
    def apply[S, A](p: Rep[StatePutData[S, A]])(implicit eS: Elem[S], eA: Elem[A]): Rep[StatePut[S, A]] =
      isoStatePut(eS, eA).to(p)
    @scalan.OverloadId("fromFields")
    def apply[S, A](s: Rep[S], a: Rep[A])(implicit eS: Elem[S], eA: Elem[A]): Rep[StatePut[S, A]] =
      mkStatePut(s, a)

    def unapply[S, A](p: Rep[StateF[S, A]]) = unmkStatePut(p)
  }
  lazy val StatePutRep: Rep[StatePutCompanionAbs] = new StatePutCompanionAbs
  lazy val StatePut: StatePutCompanionAbs = proxyStatePutCompanion(StatePutRep)
  implicit def proxyStatePutCompanion(p: Rep[StatePutCompanionAbs]): StatePutCompanionAbs = {
    proxyOps[StatePutCompanionAbs](p)
  }

  implicit case object StatePutCompanionElem extends CompanionElem[StatePutCompanionAbs] {
    lazy val tag = weakTypeTag[StatePutCompanionAbs]
    protected def getDefaultRep = StatePut
  }

  implicit def proxyStatePut[S, A](p: Rep[StatePut[S, A]]): StatePut[S, A] =
    proxyOps[StatePut[S, A]](p)

  implicit class ExtendedStatePut[S, A](p: Rep[StatePut[S, A]])(implicit eS: Elem[S], eA: Elem[A]) {
    def toData: Rep[StatePutData[S, A]] = isoStatePut(eS, eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoStatePut[S, A](implicit eS: Elem[S], eA: Elem[A]): Iso[StatePutData[S, A], StatePut[S, A]] =
    reifyObject(new StatePutIso[S, A]()(eS, eA))

  // 6) smart constructor and deconstructor
  def mkStatePut[S, A](s: Rep[S], a: Rep[A])(implicit eS: Elem[S], eA: Elem[A]): Rep[StatePut[S, A]]
  def unmkStatePut[S, A](p: Rep[StateF[S, A]]): Option[(Rep[S], Rep[A])]

  registerModule(FreeStates_Module)
}

// Std -----------------------------------
trait FreeStatesStd extends scalan.ScalanDslStd with FreeStatesDsl {
  self: MonadsDslStd =>

  lazy val StateF: Rep[StateFCompanionAbs] = new StateFCompanionAbs {
  }

  case class StdStateGet[S, A]
      (override val f: Rep[S => A])(implicit eS: Elem[S], eA: Elem[A])
    extends AbsStateGet[S, A](f) {
  }

  def mkStateGet[S, A]
    (f: Rep[S => A])(implicit eS: Elem[S], eA: Elem[A]): Rep[StateGet[S, A]] =
    new StdStateGet[S, A](f)
  def unmkStateGet[S, A](p: Rep[StateF[S, A]]) = p match {
    case p: StateGet[S, A] @unchecked =>
      Some((p.f))
    case _ => None
  }

  case class StdStatePut[S, A]
      (override val s: Rep[S], override val a: Rep[A])(implicit eS: Elem[S], eA: Elem[A])
    extends AbsStatePut[S, A](s, a) {
  }

  def mkStatePut[S, A]
    (s: Rep[S], a: Rep[A])(implicit eS: Elem[S], eA: Elem[A]): Rep[StatePut[S, A]] =
    new StdStatePut[S, A](s, a)
  def unmkStatePut[S, A](p: Rep[StateF[S, A]]) = p match {
    case p: StatePut[S, A] @unchecked =>
      Some((p.s, p.a))
    case _ => None
  }
}

// Exp -----------------------------------
trait FreeStatesExp extends scalan.ScalanDslExp with FreeStatesDsl {
  self: MonadsDslExp =>

  lazy val StateF: Rep[StateFCompanionAbs] = new StateFCompanionAbs {
  }

  case class ExpStateGet[S, A]
      (override val f: Rep[S => A])(implicit eS: Elem[S], eA: Elem[A])
    extends AbsStateGet[S, A](f)

  object StateGetMethods {
  }

  object StateGetCompanionMethods {
  }

  def mkStateGet[S, A]
    (f: Rep[S => A])(implicit eS: Elem[S], eA: Elem[A]): Rep[StateGet[S, A]] =
    new ExpStateGet[S, A](f)
  def unmkStateGet[S, A](p: Rep[StateF[S, A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: StateGetElem[S, A] @unchecked =>
      Some((p.asRep[StateGet[S, A]].f))
    case _ =>
      None
  }

  case class ExpStatePut[S, A]
      (override val s: Rep[S], override val a: Rep[A])(implicit eS: Elem[S], eA: Elem[A])
    extends AbsStatePut[S, A](s, a)

  object StatePutMethods {
  }

  object StatePutCompanionMethods {
  }

  def mkStatePut[S, A]
    (s: Rep[S], a: Rep[A])(implicit eS: Elem[S], eA: Elem[A]): Rep[StatePut[S, A]] =
    new ExpStatePut[S, A](s, a)
  def unmkStatePut[S, A](p: Rep[StateF[S, A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: StatePutElem[S, A] @unchecked =>
      Some((p.asRep[StatePut[S, A]].s, p.asRep[StatePut[S, A]].a))
    case _ =>
      None
  }

  object StateFMethods {
  }

  object StateFCompanionMethods {
    object unit {
      def unapply(d: Def[_]): Option[Rep[A] forSome {type S; type A}] = d match {
        case MethodCall(receiver, method, Seq(a, _*), _) if receiver.elem == StateFCompanionElem && method.getName == "unit" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem == StateFCompanionElem && method.getName == "get" =>
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
        case MethodCall(receiver, method, Seq(s, _*), _) if receiver.elem == StateFCompanionElem && method.getName == "set" =>
          Some(s).asInstanceOf[Option[Rep[S] forSome {type S}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[S] forSome {type S}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object run {
      def unapply(d: Def[_]): Option[(Rep[FreeState[S, A]], Rep[S]) forSome {type S; type A}] = d match {
        case MethodCall(receiver, method, Seq(t, s, _*), _) if receiver.elem == StateFCompanionElem && method.getName == "run" =>
          Some((t, s)).asInstanceOf[Option[(Rep[FreeState[S, A]], Rep[S]) forSome {type S; type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[FreeState[S, A]], Rep[S]) forSome {type S; type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object FreeStates_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAANVXTYgcRRSuntmd3zW7SZBVQ7LrMjGymJlgDhEWkXF3JibM7g7pSGQSNtR010w6dle3XTVLj4cIHgLqTUQxoBBRvARFvHlWEBEP4s2TB09JRHIwIER8Vf0zPbPTo4vJwTkU3dWv3s/3fa+q5sZtNM1c9ATTsIlp2SIcl1X5XGW8pNYoN3h/3dZ7Jlkjna0XPvrzvPXGfArNtVDmEmZrzGyhvP9Q85zoWeV6A+Ux1Qjjtss4erwhI1Q02zSJxg2bVgzL6nHcNkmlYTC+0kBTbVvvv4KuIKWB5jSbai7hRF01MWOEBfM5IjIyove8fO9vOoMYtCKqqMSqOOtig0P6EGPOtz9DHLVPbdq3ONoTpLbpiLTApkg8B2o4ZTmmDJNuoKxhObbLw6hZiHDJ1sPXKYphAu1rXMbbuAJRuxWVuwbtCmcO1l7GXbIBJsJ8CmpgxOyc7TskcF5kXB+K5zkIIWDlaZlYeYBZOcKsLDArqcQ1sGm8isXHpmt7feT/lDRCngMunvoHF6EHUqN66c0L2vm7atFKicWeSCUrE8qAo4UEhUh6ANtvz7zN7py8fiKFCi1UMFi1zbiLNR6XQQBXEVNqc5lzhCB2u8DgUhKDMkoVbEZkktdsy8EUPAVYzgBRpqEZXBiLuZmAngTss9whoaniOUpU72JCvVJLq9g0mzcfPXr4Vu2lFEoNh8iDSxWawQ2dcpRRoVxSD5yLcZYjRR0gLF6r8lUMeW8wZifkEqFy5OZv+jfH0IVUhGUQ+t/RBy72PXPtq8Ok+VkK5VpS7XUTdyWRAqw1wrQWytnbxPXns9vYFE9jyczqpIN7Jg8gjmOTBmw4WkxsVIcI4FZkAyhh+UVfwxs2JaV6s/SH+t07N4REXTTjf/E79y/jxL2f93S4VC/g2QmRTUO3R0AcSuLVIfUe1X469f7+2YMXf5GsZnTbwoaU1oEGmnahsWUhBwJgd0Viwc9VtS2yd+mOsXX9LS7pUrzhnWOzfRk6dUWuOzSBuXBT++Lq1Yd///jiftl4ubbBLeyUju2i7cIueYBthYaVP+u3w2o8SDaOlhjno1k5LACle+W6k4QPrVyIrYnFeUwJJSSNOEoRNUxgqmYSa2w3+lkkOahOcrCTeo5yYcbSRSTCg8kiBNTovLr+3ucLWyk0fRpNd6DNGKivbfeoHtIBpyMnHn8+nFOG6QD4sYutCH75W0QDtIaFWx9rUB0FpKgMV/yfdrUddKERtBU2pn+TydqxHE9YPimpZTkeva+Sbfb+b5KFjOOSTVbJrmQUSzUzFuY0bGj3TWQJ1AyxPDI9hsFxzPvFDQ72BwaVGF8feF+GvaOcsHesEc3ELtHFNZJYcM31d/rj7z537vQj516UZ82MLo38L9E5PP5Svo6dFXmFfHLCFRKMSjXLgb8I8HD862d/fO37Tz+RB/AASI4KdZcQiRQEfSjM36ZYZ1FZSwllqcHBAsK4cveDjeUfvvxVns0FcUTBjYBGl/LBBuiN9Ht+XcaCO3YMYOgIcWjF1HBNDB/+DSahItQSDQAA"
}
}

