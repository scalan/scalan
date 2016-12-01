package scalan.monads

import scala.reflect.runtime.universe._
import scalan._
import scalan.monads._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait AuthenticationsAbs extends scalan.ScalanDsl with Authentications {
  self: AuthenticationsDsl =>

  // single proxy for each type family
  implicit def proxyAuth[A](p: Rep[Auth[A]]): Auth[A] = {
    proxyOps[Auth[A]](p)(scala.reflect.classTag[Auth[A]])
  }

  // familyElem
  class AuthElem[A, To <: Auth[A]](implicit _eA: Elem[A])
    extends EntityElem[To] {
    def eA = _eA
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant))
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[Auth[A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Auth[A]] => convertAuth(x) }
      tryConvert(element[Auth[A]], this, x, conv)
    }

    def convertAuth(x: Rep[Auth[A]]): Rep[To] = {
      x.selfType1 match {
        case _: AuthElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have AuthElem[_, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def authElement[A](implicit eA: Elem[A]): Elem[Auth[A]] =
    cachedElem[AuthElem[A, Auth[A]]](eA)

  implicit case object AuthCompanionElem extends CompanionElem[AuthCompanionAbs] {
    lazy val tag = weakTypeTag[AuthCompanionAbs]
    protected def getDefaultRep = Auth
  }

  abstract class AuthCompanionAbs extends CompanionDef[AuthCompanionAbs] with AuthCompanion {
    def selfType = AuthCompanionElem
    override def toString = "Auth"
  }
  def Auth: Rep[AuthCompanionAbs]
  implicit def proxyAuthCompanionAbs(p: Rep[AuthCompanionAbs]): AuthCompanionAbs =
    proxyOps[AuthCompanionAbs](p)

  abstract class AbsLogin
      (user: Rep[String], password: Rep[String])
    extends Login(user, password) with Def[Login] {
    lazy val selfType = element[Login]
  }
  // elem for concrete class
  class LoginElem(val iso: Iso[LoginData, Login])
    extends AuthElem[SOption[String], Login]
    with ConcreteElem[LoginData, Login] {
    override lazy val parent: Option[Elem[_]] = Some(authElement(sOptionElement(StringElement)))
    override lazy val typeArgs = TypeArgs()

    override def convertAuth(x: Rep[Auth[SOption[String]]]) = // Converter is not generated by meta
!!!("Cannot convert from Auth to Login: missing fields List(user, password)")
    override def getDefaultRep = Login("", "")
    override lazy val tag = {
      weakTypeTag[Login]
    }
  }

  // state representation type
  type LoginData = (String, String)

  // 3) Iso for concrete class
  class LoginIso
    extends EntityIso[LoginData, Login] with Def[LoginIso] {
    override def from(p: Rep[Login]) =
      (p.user, p.password)
    override def to(p: Rep[(String, String)]) = {
      val Pair(user, password) = p
      Login(user, password)
    }
    lazy val eFrom = pairElement(element[String], element[String])
    lazy val eTo = new LoginElem(self)
    lazy val selfType = new LoginIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class LoginIsoElem() extends Elem[LoginIso] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new LoginIso())
    lazy val tag = {
      weakTypeTag[LoginIso]
    }
    lazy val typeArgs = TypeArgs()
  }
  // 4) constructor and deconstructor
  class LoginCompanionAbs extends CompanionDef[LoginCompanionAbs] with LoginCompanion {
    def selfType = LoginCompanionElem
    override def toString = "Login"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[LoginData]): Rep[Login] =
      isoLogin.to(p)
    @scalan.OverloadId("fromFields")
    def apply(user: Rep[String], password: Rep[String]): Rep[Login] =
      mkLogin(user, password)

    def unapply(p: Rep[Auth[SOption[String]]]) = unmkLogin(p)
  }
  lazy val LoginRep: Rep[LoginCompanionAbs] = new LoginCompanionAbs
  lazy val Login: LoginCompanionAbs = proxyLoginCompanion(LoginRep)
  implicit def proxyLoginCompanion(p: Rep[LoginCompanionAbs]): LoginCompanionAbs = {
    proxyOps[LoginCompanionAbs](p)
  }

  implicit case object LoginCompanionElem extends CompanionElem[LoginCompanionAbs] {
    lazy val tag = weakTypeTag[LoginCompanionAbs]
    protected def getDefaultRep = Login
  }

  implicit def proxyLogin(p: Rep[Login]): Login =
    proxyOps[Login](p)

  implicit class ExtendedLogin(p: Rep[Login]) {
    def toData: Rep[LoginData] = isoLogin.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoLogin: Iso[LoginData, Login] =
    reifyObject(new LoginIso())

  // 6) smart constructor and deconstructor
  def mkLogin(user: Rep[String], password: Rep[String]): Rep[Login]
  def unmkLogin(p: Rep[Auth[SOption[String]]]): Option[(Rep[String], Rep[String])]

  abstract class AbsHasPermission
      (user: Rep[String], password: Rep[String])
    extends HasPermission(user, password) with Def[HasPermission] {
    lazy val selfType = element[HasPermission]
  }
  // elem for concrete class
  class HasPermissionElem(val iso: Iso[HasPermissionData, HasPermission])
    extends AuthElem[Boolean, HasPermission]
    with ConcreteElem[HasPermissionData, HasPermission] {
    override lazy val parent: Option[Elem[_]] = Some(authElement(BooleanElement))
    override lazy val typeArgs = TypeArgs()

    override def convertAuth(x: Rep[Auth[Boolean]]) = // Converter is not generated by meta
!!!("Cannot convert from Auth to HasPermission: missing fields List(user, password)")
    override def getDefaultRep = HasPermission("", "")
    override lazy val tag = {
      weakTypeTag[HasPermission]
    }
  }

  // state representation type
  type HasPermissionData = (String, String)

  // 3) Iso for concrete class
  class HasPermissionIso
    extends EntityIso[HasPermissionData, HasPermission] with Def[HasPermissionIso] {
    override def from(p: Rep[HasPermission]) =
      (p.user, p.password)
    override def to(p: Rep[(String, String)]) = {
      val Pair(user, password) = p
      HasPermission(user, password)
    }
    lazy val eFrom = pairElement(element[String], element[String])
    lazy val eTo = new HasPermissionElem(self)
    lazy val selfType = new HasPermissionIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class HasPermissionIsoElem() extends Elem[HasPermissionIso] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new HasPermissionIso())
    lazy val tag = {
      weakTypeTag[HasPermissionIso]
    }
    lazy val typeArgs = TypeArgs()
  }
  // 4) constructor and deconstructor
  class HasPermissionCompanionAbs extends CompanionDef[HasPermissionCompanionAbs] with HasPermissionCompanion {
    def selfType = HasPermissionCompanionElem
    override def toString = "HasPermission"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[HasPermissionData]): Rep[HasPermission] =
      isoHasPermission.to(p)
    @scalan.OverloadId("fromFields")
    def apply(user: Rep[String], password: Rep[String]): Rep[HasPermission] =
      mkHasPermission(user, password)

    def unapply(p: Rep[Auth[Boolean]]) = unmkHasPermission(p)
  }
  lazy val HasPermissionRep: Rep[HasPermissionCompanionAbs] = new HasPermissionCompanionAbs
  lazy val HasPermission: HasPermissionCompanionAbs = proxyHasPermissionCompanion(HasPermissionRep)
  implicit def proxyHasPermissionCompanion(p: Rep[HasPermissionCompanionAbs]): HasPermissionCompanionAbs = {
    proxyOps[HasPermissionCompanionAbs](p)
  }

  implicit case object HasPermissionCompanionElem extends CompanionElem[HasPermissionCompanionAbs] {
    lazy val tag = weakTypeTag[HasPermissionCompanionAbs]
    protected def getDefaultRep = HasPermission
  }

  implicit def proxyHasPermission(p: Rep[HasPermission]): HasPermission =
    proxyOps[HasPermission](p)

  implicit class ExtendedHasPermission(p: Rep[HasPermission]) {
    def toData: Rep[HasPermissionData] = isoHasPermission.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoHasPermission: Iso[HasPermissionData, HasPermission] =
    reifyObject(new HasPermissionIso())

  // 6) smart constructor and deconstructor
  def mkHasPermission(user: Rep[String], password: Rep[String]): Rep[HasPermission]
  def unmkHasPermission(p: Rep[Auth[Boolean]]): Option[(Rep[String], Rep[String])]

  registerModule(Authentications_Module)
}

// Std -----------------------------------
trait AuthenticationsStd extends scalan.ScalanDslStd with AuthenticationsDsl {
  self: AuthenticationsDslStd =>

  lazy val Auth: Rep[AuthCompanionAbs] = new AuthCompanionAbs {
  }

  case class StdLogin
      (override val user: Rep[String], override val password: Rep[String])
    extends AbsLogin(user, password) {
  }

  def mkLogin
    (user: Rep[String], password: Rep[String]): Rep[Login] =
    new StdLogin(user, password)
  def unmkLogin(p: Rep[Auth[SOption[String]]]) = p match {
    case p: Login @unchecked =>
      Some((p.user, p.password))
    case _ => None
  }

  case class StdHasPermission
      (override val user: Rep[String], override val password: Rep[String])
    extends AbsHasPermission(user, password) {
  }

  def mkHasPermission
    (user: Rep[String], password: Rep[String]): Rep[HasPermission] =
    new StdHasPermission(user, password)
  def unmkHasPermission(p: Rep[Auth[Boolean]]) = p match {
    case p: HasPermission @unchecked =>
      Some((p.user, p.password))
    case _ => None
  }
}

// Exp -----------------------------------
trait AuthenticationsExp extends scalan.ScalanDslExp with AuthenticationsDsl {
  self: AuthenticationsDslExp =>

  lazy val Auth: Rep[AuthCompanionAbs] = new AuthCompanionAbs {
  }

  case class ExpLogin
      (override val user: Rep[String], override val password: Rep[String])
    extends AbsLogin(user, password)

  object LoginMethods {
    object toOper {
      def unapply(d: Def[_]): Option[Rep[Login]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[LoginElem] && method.getName == "toOper" =>
          Some(receiver).asInstanceOf[Option[Rep[Login]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Login]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object LoginCompanionMethods {
  }

  def mkLogin
    (user: Rep[String], password: Rep[String]): Rep[Login] =
    new ExpLogin(user, password)
  def unmkLogin(p: Rep[Auth[SOption[String]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: LoginElem @unchecked =>
      Some((p.asRep[Login].user, p.asRep[Login].password))
    case _ =>
      None
  }

  case class ExpHasPermission
      (override val user: Rep[String], override val password: Rep[String])
    extends AbsHasPermission(user, password)

  object HasPermissionMethods {
    object eA {
      def unapply(d: Def[_]): Option[Rep[HasPermission]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HasPermissionElem] && method.getName == "eA" =>
          Some(receiver).asInstanceOf[Option[Rep[HasPermission]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[HasPermission]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toOper {
      def unapply(d: Def[_]): Option[Rep[HasPermission]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HasPermissionElem] && method.getName == "toOper" =>
          Some(receiver).asInstanceOf[Option[Rep[HasPermission]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[HasPermission]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object HasPermissionCompanionMethods {
  }

  def mkHasPermission
    (user: Rep[String], password: Rep[String]): Rep[HasPermission] =
    new ExpHasPermission(user, password)
  def unmkHasPermission(p: Rep[Auth[Boolean]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: HasPermissionElem @unchecked =>
      Some((p.asRep[HasPermission].user, p.asRep[HasPermission].password))
    case _ =>
      None
  }

  object AuthMethods {
    object toOper {
      def unapply(d: Def[_]): Option[Rep[Auth[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AuthElem[_, _]] && method.getName == "toOper" =>
          Some(receiver).asInstanceOf[Option[Rep[Auth[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Auth[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object AuthCompanionMethods {
  }
}

object Authentications_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAAL1WTWwbRRQe20kcx0kbgkgLUkRqXH6DXVGhIkUIpfmhrdzE6vYH0qpovDtxpszODDvjxOZQbpWAG0IckDgUgbhESIgLAokLrYQQ6oErZ04tVdUDlZBAvJn98U/jNBxgD6PZ2Tfv53vfe/u2fkeDKkBPKhczzEs+0bjk2P2c0kVnkWuqWyeF12BkgaxdPPbpn+f9d/el0fgqGlrHakGxVZQLN4tNmewd7VVQDnOXKC0CpdGBirVQdgVjxNVU8DL1/YbGNUbKFar0bAUN1ITXegtdRqkKGncFdwOiiTPPsFJERefDxHhEk/ecfW+tyLYNXjZRlDuiOB1gqsF9sDEeyp8i0mlxwVu+Rnsi11akcQtk8qQpIYbjvmTWTKaCstSXItCx1SxYWBde/DrAMRygicolvIHLYLVednRAed0ok9h9E9fJMogY8QGIQRG2drolSaQ8r7TXZa8pEUKQlResY6U2ZqUEs5LBrOiQgGJG38bmYzUQzRYKn1QGoaYEFTMPUBFrIIvcK753wT1/z8n7aXO5aVzJWoeGQNHjfRhi0wPY/njqA3X31atH0mhkFY1QNVdTOsCu7qRBBFcecy609TlBEAd1yGChXwatlTmQ6aFJzhW+xBw0RViOQqIYdak2wuZsNEpPH+yzWpJYNNWUqSTe6T7xWi7NY8aqNx99/uCtxdfSKN1tIgcqHSiGIFaq0cBcQ69Hqs26V6OsE/ItMfhEP4OSVAPqA+k3yIs/fHfmzvfLg9bmhEfWcIPps5g1SMi3yIO2N8Z4ulDQaKgtkGv2rtkd4k2Qf+rmbe/6IXQhneQrCm93FAEVEy99/O1BUv0yjYZXbUUtMVy3ZDEJWSDKXUXDYoME4Xl2AzOz25Yw2Sj4KI2d+GcAf42m+zYDSUxyZm2RpeLw82GdLAtOikvV4h/OTx9umTII0Gj4JczW3/TIX7/uWdO2QiCvDUWCOKMZaCohFmbZF8JrD6YSS2aBbAxLQGRTBN6Od7sTNBL64QifPFS4Sy9efV/bVKSa3Z1npXYJKn3W3juwQ1bipvjVlSuP3PnsjYdt4Q7XqPaxLB76F2UbV9l/WJYoAS9sSI+13y2i0nRxUad8vtNuofeCRoNWqudjPtVdmr3VOgn31jBTwKrsUSEYwfz+BFkrHZfuy/n/xxWzzti1vAvgJo9hVSWBT5UC3B4E4FiXdFuoI4KhyHQ3qBmg2s4ww1FqrkNZH4x7Itp9oGPG3jbxxf3ZniTNbKp/OwZivn7LK+2/PbWZRkMngBzQpVQFDdZEg3sx42GA0aSpj8ZnqW7GA8NxgP1krtnA8COGioOExJ2roSkrn43Ow34FzzRqu54gBg5PRg6bW6XjPNSni899s7VJbzyzZDtWL6TVdswzoKLUJ+YF4jIcEM9MKMSHCSpsAoc/euXcif3nztg2NOpZofBL0n63n/dOYjlrp5Ond5hOQKi46EuYPmFz+NrLv7zz8xefJ1Fko/j3msyZmc6NLY/FQQiOPZXEVugTmxM1HqDn5XufLD974+vf7N91xLQw+BvwZOjr/Kt203aixwkY5jp4DPQy3a2DrZZq7B+MDa5DewsAAA=="
}
}

