package scalan.examples

import scala.reflect.runtime.universe._
import scalan._
import scalan.monads._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait AuthenticationsAbs extends Authentications with scalan.Scalan {
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
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA))
    }
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
        case e => !!!(s"Expected $x to have AuthElem[_, _], but got $e")
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
  implicit def proxyAuthCompanion(p: Rep[AuthCompanion]): AuthCompanion =
    proxyOps[AuthCompanion](p)

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
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }

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
    extends Iso[LoginData, Login]()(pairElement(implicitly[Elem[String]], implicitly[Elem[String]])) {
    override def from(p: Rep[Login]) =
      (p.user, p.password)
    override def to(p: Rep[(String, String)]) = {
      val Pair(user, password) = p
      Login(user, password)
    }
    lazy val eTo = new LoginElem(this)
  }
  // 4) constructor and deconstructor
  class LoginCompanionAbs extends CompanionDef[LoginCompanionAbs] with LoginCompanion {
    def selfType = LoginCompanionElem
    override def toString = "Login"
    def apply(p: Rep[LoginData]): Rep[Login] =
      isoLogin.to(p)
    def apply(user: Rep[String], password: Rep[String]): Rep[Login] =
      mkLogin(user, password)
  }
  object LoginMatcher {
    def unapply(p: Rep[Auth[SOption[String]]]) = unmkLogin(p)
  }
  lazy val Login: Rep[LoginCompanionAbs] = new LoginCompanionAbs
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
    cachedIso[LoginIso]()

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
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }

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
    extends Iso[HasPermissionData, HasPermission]()(pairElement(implicitly[Elem[String]], implicitly[Elem[String]])) {
    override def from(p: Rep[HasPermission]) =
      (p.user, p.password)
    override def to(p: Rep[(String, String)]) = {
      val Pair(user, password) = p
      HasPermission(user, password)
    }
    lazy val eTo = new HasPermissionElem(this)
  }
  // 4) constructor and deconstructor
  class HasPermissionCompanionAbs extends CompanionDef[HasPermissionCompanionAbs] with HasPermissionCompanion {
    def selfType = HasPermissionCompanionElem
    override def toString = "HasPermission"
    def apply(p: Rep[HasPermissionData]): Rep[HasPermission] =
      isoHasPermission.to(p)
    def apply(user: Rep[String], password: Rep[String]): Rep[HasPermission] =
      mkHasPermission(user, password)
  }
  object HasPermissionMatcher {
    def unapply(p: Rep[Auth[Boolean]]) = unmkHasPermission(p)
  }
  lazy val HasPermission: Rep[HasPermissionCompanionAbs] = new HasPermissionCompanionAbs
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
    cachedIso[HasPermissionIso]()

  // 6) smart constructor and deconstructor
  def mkHasPermission(user: Rep[String], password: Rep[String]): Rep[HasPermission]
  def unmkHasPermission(p: Rep[Auth[Boolean]]): Option[(Rep[String], Rep[String])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(Authentications_Module.dump))
}

// Seq -----------------------------------
trait AuthenticationsSeq extends AuthenticationsDsl with scalan.ScalanSeq {
  self: AuthenticationsDslSeq =>
  lazy val Auth: Rep[AuthCompanionAbs] = new AuthCompanionAbs {
  }

  case class SeqLogin
      (override val user: Rep[String], override val password: Rep[String])
    extends AbsLogin(user, password) {
  }

  def mkLogin
    (user: Rep[String], password: Rep[String]): Rep[Login] =
    new SeqLogin(user, password)
  def unmkLogin(p: Rep[Auth[SOption[String]]]) = p match {
    case p: Login @unchecked =>
      Some((p.user, p.password))
    case _ => None
  }

  case class SeqHasPermission
      (override val user: Rep[String], override val password: Rep[String])
    extends AbsHasPermission(user, password) {
  }

  def mkHasPermission
    (user: Rep[String], password: Rep[String]): Rep[HasPermission] =
    new SeqHasPermission(user, password)
  def unmkHasPermission(p: Rep[Auth[Boolean]]) = p match {
    case p: HasPermission @unchecked =>
      Some((p.user, p.password))
    case _ => None
  }
}

// Exp -----------------------------------
trait AuthenticationsExp extends AuthenticationsDsl with scalan.ScalanExp {
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

object Authentications_Module {
  val packageName = "scalan.examples"
  val name = "Authentications"
  val dump = "H4sIAAAAAAAAAL1WTWwbRRQer+M4ttOmRCioSBWpMSBQsSMk1EMOlZO6/Mh1rGxBlamQxuuxM2V2ZjMzDmsOPXCEG+KKUO+9cUFC6gUhIQ6cECBx5lSKUAX0BOqb2R//JE5zgT2MZt++eT/f997buXMf5ZREzysPM8yrPtG46tp9XemK2+Ca6tFV0Rsycpn0P1z70rvKt5SDVjpocQ+ry4p1UCHaNMIg3btkv4kKmHtEaSGVRueb1kPNE4wRT1PBa9T3hxp3Gak1qdKbTbTQFb3RPrqFMk10xhPck0QTd5thpYiK5UvERETT94J9H+0EYx+8ZrKoTWRxTWKqIXzwcSbS3yWBO+KCj3yNTseh7QQmLNDJUz8QUicu8mBuT/SS1wWOQYBWmzfxAa6Bi0HN1ZLyAZwsBdh7Dw9IC1SM+gIErAjrXxsF9j3bREVF9gGgN/yAWUkYIISAgVdsENUxPtUUn6rBp+ISSTGjH2DzsS1FOELRk8kiFAZg4sJjTCQWSIP3Kh/d8N556JZ8xxwOTSh5m+EiGHpmTjVYKgDHb3c/UQ9eu33RQcUOKlJV7yotsacnKY/RKmHOhbYxpwBiOQC2yvPYsl7qoDNTEgVP+AHmYCmGchl4YtSj2igb2XLMzhzo8zogiWomDDJpvutz8rV1s40Za987+/JzvzWuO8iZdlEAky4UvkyMarRQH+q92LRZVzTKu1FtpQ6fnecwIG1JfSjwA/Lq11+99cfdVs76XO2RPh4y/TZmQxKVWxzBOBrj3CmXNVocKxTC2TV/TL4p8i/c+733zQa64aR8xemdrETARE79/GPphxcvOWipYxvqCsODDlCmGoz4O3JbcN1BS+KAyOhL/gAzszuyZPJx+jGRkwxkgQGN1ue2fkAMPZu2zTIJAKWoU1qCk8qVduVv97tP75hGkGg5+hLx9S+9+M8vp/va9ggwO1REJpxmYYREaJjlqQhgKziXejIL8LEUACbvC9k79uw0RcUoDlf45InyA/ru7Y+1JSMTTo+ene5N6PVNe+78MbwkI/Cvzobz59mfPndQAeDvUu3joLJxwsb9D5sRpYCNl3JghrMYUL496aw8HlZP261GOas187GUme7C2cZcg3N9zBSUT35LCEYwP8yE9TJx6BC5/19RmPWCXWvz0Fp7Has2kT5VCsB6HGqnprTHShNhL8b+ppHMQiEdjy2IMvUJY4eyOXFKp4zlIzJJJq2VpGPp3PzBCsW2ttt8kt2/dNdBuTeBe5g2qolyXTHkvaSK4dqhSai3EllmuoqharHEflq19llH47Dm5t2axhcUV0z45jriRV0Gkjh6EmJoJqJifCQkeHRabtxTwMeth5+1Xvr+i1/t76JouhOGG08vMZO/iWmeVmfCgMvJRAKAsmlcG/wjz6+u9iwKAAA="
}
}

