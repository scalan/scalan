package scalan.common

import scalan._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait KindsAbs extends scalan.ScalanDsl with Kinds {
  self: KindsDsl =>

  // single proxy for each type family
  implicit def proxyKind[F[_], A](p: Rep[Kind[F, A]]): Kind[F, A] = {
    proxyOps[Kind[F, A]](p)(scala.reflect.classTag[Kind[F, A]])
  }

  // familyElem
  class KindElem[F[_], A, To <: Kind[F, A]](implicit _cF: Cont[F], _eA: Elem[A])
    extends EntityElem[To] {
    def cF = _cF
    def eA = _eA
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = TypeArgs("F" -> (cF -> scalan.util.Invariant), "A" -> (eA -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[Kind[F, A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Kind[F, A]] => convertKind(x) }
      tryConvert(element[Kind[F, A]], this, x, conv)
    }

    def convertKind(x: Rep[Kind[F, A]]): Rep[To] = {
      x.elem.asInstanceOf[Elem[_]] match {
        case _: KindElem[_, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have KindElem[_, _, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def kindElement[F[_], A](implicit cF: Cont[F], eA: Elem[A]): Elem[Kind[F, A]] =
    cachedElem[KindElem[F, A, Kind[F, A]]](cF, eA)

  implicit case object KindCompanionElem extends CompanionElem[KindCompanionAbs] {
    lazy val tag = weakTypeTag[KindCompanionAbs]
    protected def getDefaultRep = Kind
  }

  abstract class KindCompanionAbs extends CompanionDef[KindCompanionAbs] with KindCompanion {
    def selfType = KindCompanionElem
    override def toString = "Kind"
  }
  def Kind: Rep[KindCompanionAbs]
  implicit def proxyKindCompanionAbs(p: Rep[KindCompanionAbs]): KindCompanionAbs =
    proxyOps[KindCompanionAbs](p)

  abstract class AbsReturn[F[_], A]
      (a: Rep[A])(implicit eA: Elem[A], cF: Cont[F])
    extends Return[F, A](a) with Def[Return[F, A]] {
    lazy val selfType = element[Return[F, A]]
  }
  // elem for concrete class
  class ReturnElem[F[_], A](val iso: Iso[ReturnData[F, A], Return[F, A]])(implicit override val eA: Elem[A], override val cF: Cont[F])
    extends KindElem[F, A, Return[F, A]]
    with ConcreteElem[ReturnData[F, A], Return[F, A]] {
    override lazy val parent: Option[Elem[_]] = Some(kindElement(container[F], element[A]))
    override lazy val typeArgs = TypeArgs("F" -> (cF -> scalan.util.Invariant), "A" -> (eA -> scalan.util.Invariant))

    override def convertKind(x: Rep[Kind[F, A]]) = // Converter is not generated by meta
!!!("Cannot convert from Kind to Return: missing fields List(a)")
    override def getDefaultRep = Return(element[A].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[Return[F, A]]
    }
  }

  // state representation type
  type ReturnData[F[_], A] = A

  // 3) Iso for concrete class
  class ReturnIso[F[_], A](implicit eA: Elem[A], cF: Cont[F])
    extends EntityIso[ReturnData[F, A], Return[F, A]] with Def[ReturnIso[F, A]] {
    override def from(p: Rep[Return[F, A]]) =
      p.a
    override def to(p: Rep[A]) = {
      val a = p
      Return(a)
    }
    lazy val eFrom = element[A]
    lazy val eTo = new ReturnElem[F, A](self)
    lazy val selfType = new ReturnIsoElem[F, A](eA, cF)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eA
      case 1 => cF
    }
  }
  case class ReturnIsoElem[F[_], A](eA: Elem[A], cF: Cont[F]) extends Elem[ReturnIso[F, A]] {
    def getDefaultRep = reifyObject(new ReturnIso[F, A]()(eA, cF))
    lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[ReturnIso[F, A]]
    }
    lazy val typeArgs = TypeArgs("F" -> (cF -> scalan.util.Invariant), "A" -> (eA -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class ReturnCompanionAbs extends CompanionDef[ReturnCompanionAbs] with ReturnCompanion {
    def selfType = ReturnCompanionElem
    override def toString = "Return"

    @scalan.OverloadId("fromFields")
    def apply[F[_], A](a: Rep[A])(implicit eA: Elem[A], cF: Cont[F]): Rep[Return[F, A]] =
      mkReturn(a)

    def unapply[F[_], A](p: Rep[Kind[F, A]]) = unmkReturn(p)
  }
  lazy val ReturnRep: Rep[ReturnCompanionAbs] = new ReturnCompanionAbs
  lazy val Return: ReturnCompanionAbs = proxyReturnCompanion(ReturnRep)
  implicit def proxyReturnCompanion(p: Rep[ReturnCompanionAbs]): ReturnCompanionAbs = {
    proxyOps[ReturnCompanionAbs](p)
  }

  implicit case object ReturnCompanionElem extends CompanionElem[ReturnCompanionAbs] {
    lazy val tag = weakTypeTag[ReturnCompanionAbs]
    protected def getDefaultRep = Return
  }

  implicit def proxyReturn[F[_], A](p: Rep[Return[F, A]]): Return[F, A] =
    proxyOps[Return[F, A]](p)

  implicit class ExtendedReturn[F[_], A](p: Rep[Return[F, A]])(implicit eA: Elem[A], cF: Cont[F]) {
    def toData: Rep[ReturnData[F, A]] = isoReturn(eA, cF).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoReturn[F[_], A](implicit eA: Elem[A], cF: Cont[F]): Iso[ReturnData[F, A], Return[F, A]] =
    reifyObject(new ReturnIso[F, A]()(eA, cF))

  // 6) smart constructor and deconstructor
  def mkReturn[F[_], A](a: Rep[A])(implicit eA: Elem[A], cF: Cont[F]): Rep[Return[F, A]]
  def unmkReturn[F[_], A](p: Rep[Kind[F, A]]): Option[(Rep[A])]

  abstract class AbsBind[F[_], S, B]
      (a: Rep[Kind[F, S]], f: Rep[S => Kind[F, B]])(implicit eS: Elem[S], eA: Elem[B], cF: Cont[F])
    extends Bind[F, S, B](a, f) with Def[Bind[F, S, B]] {
    lazy val selfType = element[Bind[F, S, B]]
  }
  // elem for concrete class
  class BindElem[F[_], S, B](val iso: Iso[BindData[F, S, B], Bind[F, S, B]])(implicit val eS: Elem[S], override val eA: Elem[B], override val cF: Cont[F])
    extends KindElem[F, B, Bind[F, S, B]]
    with ConcreteElem[BindData[F, S, B], Bind[F, S, B]] {
    override lazy val parent: Option[Elem[_]] = Some(kindElement(container[F], element[B]))
    override lazy val typeArgs = TypeArgs("F" -> (cF -> scalan.util.Invariant), "S" -> (eS -> scalan.util.Invariant), "B" -> (eA -> scalan.util.Invariant))

    override def convertKind(x: Rep[Kind[F, B]]) = // Converter is not generated by meta
!!!("Cannot convert from Kind to Bind: missing fields List(a, f)")
    override def getDefaultRep = Bind(element[Kind[F, S]].defaultRepValue, constFun[S, Kind[F, B]](element[Kind[F, B]].defaultRepValue))
    override lazy val tag = {
      implicit val tagS = eS.tag
      implicit val tagB = eA.tag
      weakTypeTag[Bind[F, S, B]]
    }
  }

  // state representation type
  type BindData[F[_], S, B] = (Kind[F, S], S => Kind[F, B])

  // 3) Iso for concrete class
  class BindIso[F[_], S, B](implicit eS: Elem[S], eA: Elem[B], cF: Cont[F])
    extends EntityIso[BindData[F, S, B], Bind[F, S, B]] with Def[BindIso[F, S, B]] {
    override def from(p: Rep[Bind[F, S, B]]) =
      (p.a, p.f)
    override def to(p: Rep[(Kind[F, S], S => Kind[F, B])]) = {
      val Pair(a, f) = p
      Bind(a, f)
    }
    lazy val eFrom = pairElement(element[Kind[F, S]], element[S => Kind[F, B]])
    lazy val eTo = new BindElem[F, S, B](self)
    lazy val selfType = new BindIsoElem[F, S, B](eS, eA, cF)
    def productArity = 3
    def productElement(n: Int) = n match {
      case 0 => eS
      case 1 => eA
      case 2 => cF
    }
  }
  case class BindIsoElem[F[_], S, B](eS: Elem[S], eA: Elem[B], cF: Cont[F]) extends Elem[BindIso[F, S, B]] {
    def getDefaultRep = reifyObject(new BindIso[F, S, B]()(eS, eA, cF))
    lazy val tag = {
      implicit val tagS = eS.tag
      implicit val tagB = eA.tag
      weakTypeTag[BindIso[F, S, B]]
    }
    lazy val typeArgs = TypeArgs("F" -> (cF -> scalan.util.Invariant), "S" -> (eS -> scalan.util.Invariant), "B" -> (eA -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class BindCompanionAbs extends CompanionDef[BindCompanionAbs] with BindCompanion {
    def selfType = BindCompanionElem
    override def toString = "Bind"
    @scalan.OverloadId("fromData")
    def apply[F[_], S, B](p: Rep[BindData[F, S, B]])(implicit eS: Elem[S], eA: Elem[B], cF: Cont[F]): Rep[Bind[F, S, B]] =
      isoBind(eS, eA, cF).to(p)
    @scalan.OverloadId("fromFields")
    def apply[F[_], S, B](a: Rep[Kind[F, S]], f: Rep[S => Kind[F, B]])(implicit eS: Elem[S], eA: Elem[B], cF: Cont[F]): Rep[Bind[F, S, B]] =
      mkBind(a, f)

    def unapply[F[_], S, B](p: Rep[Kind[F, B]]) = unmkBind(p)
  }
  lazy val BindRep: Rep[BindCompanionAbs] = new BindCompanionAbs
  lazy val Bind: BindCompanionAbs = proxyBindCompanion(BindRep)
  implicit def proxyBindCompanion(p: Rep[BindCompanionAbs]): BindCompanionAbs = {
    proxyOps[BindCompanionAbs](p)
  }

  implicit case object BindCompanionElem extends CompanionElem[BindCompanionAbs] {
    lazy val tag = weakTypeTag[BindCompanionAbs]
    protected def getDefaultRep = Bind
  }

  implicit def proxyBind[F[_], S, B](p: Rep[Bind[F, S, B]]): Bind[F, S, B] =
    proxyOps[Bind[F, S, B]](p)

  implicit class ExtendedBind[F[_], S, B](p: Rep[Bind[F, S, B]])(implicit eS: Elem[S], eA: Elem[B], cF: Cont[F]) {
    def toData: Rep[BindData[F, S, B]] = isoBind(eS, eA, cF).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoBind[F[_], S, B](implicit eS: Elem[S], eA: Elem[B], cF: Cont[F]): Iso[BindData[F, S, B], Bind[F, S, B]] =
    reifyObject(new BindIso[F, S, B]()(eS, eA, cF))

  // 6) smart constructor and deconstructor
  def mkBind[F[_], S, B](a: Rep[Kind[F, S]], f: Rep[S => Kind[F, B]])(implicit eS: Elem[S], eA: Elem[B], cF: Cont[F]): Rep[Bind[F, S, B]]
  def unmkBind[F[_], S, B](p: Rep[Kind[F, B]]): Option[(Rep[Kind[F, S]], Rep[S => Kind[F, B]])]

  registerModule(Kinds_Module)
}

// Exp -----------------------------------
trait KindsExp extends scalan.ScalanDslExp with KindsDsl {
  self: KindsDslExp =>

  lazy val Kind: Rep[KindCompanionAbs] = new KindCompanionAbs {
  }

  case class ExpReturn[F[_], A]
      (override val a: Rep[A])(implicit eA: Elem[A], cF: Cont[F])
    extends AbsReturn[F, A](a)

  object ReturnMethods {
    // WARNING: Cannot generate matcher for method `flatMap`: Method has function arguments f
  }

  object ReturnCompanionMethods {
  }

  def mkReturn[F[_], A]
    (a: Rep[A])(implicit eA: Elem[A], cF: Cont[F]): Rep[Return[F, A]] =
    new ExpReturn[F, A](a)
  def unmkReturn[F[_], A](p: Rep[Kind[F, A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ReturnElem[F, A] @unchecked =>
      Some((p.asRep[Return[F, A]].a))
    case _ =>
      None
  }

  case class ExpBind[F[_], S, B]
      (override val a: Rep[Kind[F, S]], override val f: Rep[S => Kind[F, B]])(implicit eS: Elem[S], eA: Elem[B], cF: Cont[F])
    extends AbsBind[F, S, B](a, f)

  object BindMethods {
    // WARNING: Cannot generate matcher for method `flatMap`: Method has function arguments f1
  }

  object BindCompanionMethods {
  }

  def mkBind[F[_], S, B]
    (a: Rep[Kind[F, S]], f: Rep[S => Kind[F, B]])(implicit eS: Elem[S], eA: Elem[B], cF: Cont[F]): Rep[Bind[F, S, B]] =
    new ExpBind[F, S, B](a, f)
  def unmkBind[F[_], S, B](p: Rep[Kind[F, B]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: BindElem[F, S, B] @unchecked =>
      Some((p.asRep[Bind[F, S, B]].a, p.asRep[Bind[F, S, B]].f))
    case _ =>
      None
  }

  object KindMethods {
    // WARNING: Cannot generate matcher for method `flatMap`: Method has function arguments f

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[Kind[F, A]], Rep[A => B]) forSome {type F[_]; type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if (receiver.elem.asInstanceOf[Elem[_]] match { case _: KindElem[_, _, _] => true; case _ => false }) && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Kind[F, A]], Rep[A => B]) forSome {type F[_]; type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Kind[F, A]], Rep[A => B]) forSome {type F[_]; type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object KindCompanionMethods {
  }
}

object Kinds_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAAMVXTWwbRRSedRw7jkMTGlEKVUiI3NLyY1f0UKQIFTuxo7RuEmULhVARjXfH6Zbd2WV3nK45lFsPcEMIBBKHSiAuEVJVISHOrYQQygHBiRMHDqilQj3QE4g3s792vE5ShPBhtDN++36+930zs5u/o0HHRlOOgnVMiwZhuCiL57LDCvJZU23pZI40f/tp7aVxZ+toCo2tosxF7Mw5+irKeQ9V1wqfZabWUQ5ThTjMtB2GnqwL3yXF1HWiMM2kJc0wWgw3dFKqaw6bqaN0w1Tbb6ErSKqjMcWkik0YkWd17DjE8deHCGUa08J5TszbS1YUg5Z4/qVY/udsrDFIH2KMefYrxJLb1KRtg6F9fmpLFk8LbPLEtaCGBcPSRZiBOspqhmXaLIiahQgXTTWYpimGBbS/fglv4BJEXS/JzNboOndmYeVNvE4WwYSbp6EGh+jNc22L+M7zDlM74rkWQgj68bxIrBhhVgwxK3LMCjKxNaxrb2P+57Jtum3k/aQBhFwLXDy7g4vAA6lStfDuBeX1+3LeSPGXXZ5KViSUAUeTCdwQ7QFsv11537k3f+1kCg2vomHNKTccZmOFxWngw5XHlJpM5BwiiO116OB0UgdFlDLYdNEkp5iGhSl48rEcgUbpmqIxbszXRvz2JGCfZRYJTCXXksJ6k7QguDSLdX359mPPHb5TfTWFUp0hcuBSBjHYgVOG0mc0qvqu+TjKkFSL8OXTspjyIedGY7ZPJiEmT92+q946ji6kQiT9wLtrHrjY/8In3xwmy1+m0NCq4HpNx+uijRyqOeIoq2jI3CC2t57dwDp/6tnKrEqauKUzH+A4MgOADENTiTK1CIdtRtBfCsrPewxeNCkp1JYLf8rffbDJCWqjEe8fT7d/ayf/+nlfkwnuAp44QHYAtN4FfTLWw55L2TTIw9P3tDeuvccEqpLbKe+lxiWQ04x474k+AAc7z/WrVx/547O1caGOoYbGDGwVju9BGwGV/0PuoxAVj5WPRnM+TAKqoyuEtWw6Gw88GXsjhvPjUtBDYcRQipSDBqSrOjH69CTBgVILHcyalPXUU7ypDGW8fIWDUAwTSb0SaLx2Ry0evDtxOYUyp9FgE1ju1NFgw2xRNYAZjiZGXFYJ1qROmAFWbGMjPLE2MGyx0GaGDgTMbzFNL73ir3t8h98UivDkBcUKPNXTYi2g7QG/JO63uEC9iKzwzNebl7WtYzWhiQiZ+T4uozbMx3qRlzrB3sV+VknQ2DaeoK4291PuLuLK2+ImhGn2CGODmBPJUWtR5ceFj8dHJ9Z+EZt+RjUNrAl2HQKO2LAxCA4c8nfeKJ1/jVscvSNiPLYLuT5UgXAPKFa5n1jjID+Q2is7O9i72tO83LjWk2m+g7b4UO1Wwk7SiUDpZ1Xp5dYHIeN3trPWATg/9iyD3kdchwi7+LMHWp3pTavgovP/tWDb7hWNH0VZHQGdFxN0PkcUHdtE5ZdxYsDHgncUn/jw1PnTB8+/LC4DI6ow8v4J7zO9P23OYmtGXMSP9rmIg1GhaliszR9O3Hzxh3e+/+LzcNPO+tUNiq4D/H7qcPwb/i2BVzSdUJHsH/rAoiv3P118euvGr2L3GubXB7hU0fCrJjrE3K59d0iEhm+UGKug2fw+EePOdT589Q8/D0wSTA4AAA=="
}
}

trait KindsDsl extends impl.KindsAbs
trait KindsDslExp extends impl.KindsExp
