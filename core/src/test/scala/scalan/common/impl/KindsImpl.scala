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
    lazy val typeArgs = TypeArgs("F" -> cF, "A" -> eA)
    override def isEntityType = true
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
      x.selfType1.asInstanceOf[Elem[_]] match {
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
    override lazy val typeArgs = TypeArgs("F" -> cF, "A" -> eA)

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
    def isEntityType = true
    def getDefaultRep = reifyObject(new ReturnIso[F, A]()(eA, cF))
    lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[ReturnIso[F, A]]
    }
    lazy val typeArgs = TypeArgs("F" -> cF, "A" -> eA)
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
    override lazy val typeArgs = TypeArgs("F" -> cF, "S" -> eS, "B" -> eA)

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
    def isEntityType = true
    def getDefaultRep = reifyObject(new BindIso[F, S, B]()(eS, eA, cF))
    lazy val tag = {
      implicit val tagS = eS.tag
      implicit val tagB = eA.tag
      weakTypeTag[BindIso[F, S, B]]
    }
    lazy val typeArgs = TypeArgs("F" -> cF, "S" -> eS, "B" -> eA)
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

// Std -----------------------------------
trait KindsStd extends scalan.ScalanDslStd with KindsDsl {
  self: KindsDslStd =>

  lazy val Kind: Rep[KindCompanionAbs] = new KindCompanionAbs {
  }

  case class StdReturn[F[_], A]
      (override val a: Rep[A])(implicit eA: Elem[A], cF: Cont[F])
    extends AbsReturn[F, A](a) {
  }

  def mkReturn[F[_], A]
    (a: Rep[A])(implicit eA: Elem[A], cF: Cont[F]): Rep[Return[F, A]] =
    new StdReturn[F, A](a)
  def unmkReturn[F[_], A](p: Rep[Kind[F, A]]) = p match {
    case p: Return[F, A] @unchecked =>
      Some((p.a))
    case _ => None
  }

  case class StdBind[F[_], S, B]
      (override val a: Rep[Kind[F, S]], override val f: Rep[S => Kind[F, B]])(implicit eS: Elem[S], eA: Elem[B], cF: Cont[F])
    extends AbsBind[F, S, B](a, f) {
  }

  def mkBind[F[_], S, B]
    (a: Rep[Kind[F, S]], f: Rep[S => Kind[F, B]])(implicit eS: Elem[S], eA: Elem[B], cF: Cont[F]): Rep[Bind[F, S, B]] =
    new StdBind[F, S, B](a, f)
  def unmkBind[F[_], S, B](p: Rep[Kind[F, B]]) = p match {
    case p: Bind[F, S, B] @unchecked =>
      Some((p.a, p.f))
    case _ => None
  }
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
  val dump = "H4sIAAAAAAAAAL1XS2wbRRgev2I7Dk1ohQJUJSFyA0VgV/RQpAhVTmxDW+ehbFGRWyUa747dLbuzy+44WnMot0rADVUgkDgUgbhURcCtZ5AQQhyq3jhx4FRAqAcqIYH4Z/Zpx+s4RbCH0c7uv//j+/5vZvbGryhjW2jRlrGGaUknDJckcV+xWVGqUaay3qqhdDVSJe2tlz/+84L+1mwSzTTRxCVsV22tifLuTc0xg3uJKQ2Ux1QmNjMsm6EnGyJCWTY0jchMNWhZ1fUuwy2NlBuqzZYaKN0ylN7r6ApKNNCMbFDZIoxIKxq2bWJ7z3OEZ6QG87yY99bNMAYt8yrKkSrOWVhlkD7EmHHtN4kp9ahBezpDB7zU1k2eFtgUiGNCDad1UxNhUg2UVXXTsJgfNQsRLhmKP01TDA/QwcZlvIPLELVTlpil0g53ZmL5Ndwha2DCzdNQg0209rmeSTznBZspffEcEyEErDwvEiuFmJUCzEocs6JELBVr6huYv9ywDKeH3CuRQsgxwcWze7jwPZAaVYpvX5Qv3JcKepJ/7PBUsiKhCXA0F9Mhgh7A9tvNd+17L10/mUSTTTSp2pWWzSwss2gbeHAVMKUGEzkHCGKrAwwuxDEoolTAZqBN8rKhm5iCJw/LKSBKU2WVcWP+bMqjJwb7LDOJb5pwzERQ73xMvaKXVrCmbdx97Lmjv9ReTaJkf4g8uJRADJbvlKH0WZUqnms+TjOUqIf48mlFTPmQd8IxOyKTAJOn7v6mfHMcXUwGSHqBxyMPXBx84cNbR8nGzSTKNUWv1zXcETRyqKrElpsoZ+wQy32e3cEavxtKZVYhbdzVmAdwFJkUIMPQfKxMTcJhWxLtn/DLL7gdvGZQUqxvFP+Qvrt2gzeohabcN65u/1ZP/vXjgTYTvQt4Yh/ZFGh9APp4rCddl5Khk4cX7qlb199hAtWE0y/v9dZlkNOS+O6JEQD7K88XV68+8vsn24eEOnItlenYLB7fhzb8Vv4Pex8FqLhdORvO+TAHqE5vEta16Eo08FzkiwjOjyd8DoURQ0lS8QlI1zSij+AkxoFcDxysGJQN1VOUVIYm3HyFg0AMR+K4EmjQWWn1/c/ntpIocwZl2tDldgNlWkaXKj7MsDUx4rBl/1miH2aAFVtYD2AV1zwKseLJRpI/NdRiexCP4Wa7YCsk+nEZY+lZjpHDLkrRACOjRDZGXGlX3Jgw7SFhLNBdLI/1LpXvnP7g0PSR7Z/E+jyhGDpWRSMcBjot0LCg67C3SIbp/GvcougtivHYGMp6aBnCPaCupFG6ioL8QMJc3tvB/oWZ5uVGZRnf4XtIhQ/V8bQSIXi4wW5CI7GPof66UrCs77vlh+88fYIb6JV9tNDZ4S3knz/+b7iH13otTGMRRFyKEXGVyBq2iMIPxUSHQ7u7JZ5479T5M4+ef0VsylOKMHLfBOeK4b8Yq9hcEgfip0cciMGoWNNN+OGBmxNfv3j7ze8/+1QcKEK0GcoImgFvL3XYhnVvt+YVLcRUJHmbL7TNlfsfrT3zw1c/i6Vpkm/jcLihwd9FuJk4A4tqToSGf4UItMAu39cjzXKTD1/+A5JJYIjaDQAA"
}
}

trait KindsDsl extends impl.KindsAbs
trait KindsDslStd extends impl.KindsStd
trait KindsDslExp extends impl.KindsExp
