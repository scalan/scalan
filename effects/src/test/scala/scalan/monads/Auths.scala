package scalan.monads

import scala.reflect.runtime.universe._
import scalan._
import scalan.monads._

trait Authentications { self: AuthenticationsDsl =>
  lazy val OperM: Monad[Oper] = Monad[Oper]
  import OperM.toMonadic

  type RepAuth[A] = Rep[Auth[A]]
  trait Auth[A] extends Def[Auth[A]] { self =>
    implicit def eA: Elem[A]
    def toOper: Rep[Oper[A]]
  }
  trait AuthCompanion

  abstract class Login(val user: Rep[String], val password: Rep[String]) extends Auth[SOption[String]] {
    val eA = element[SOption[String]]

    def toOper = {
      for {
        cond <- eval(user === "john.snow" && password === "Ghost")
      } yield
        IF (cond) {
          SOption.some("john.snow")
        } ELSE {
          SOption.none[String]
        }
    }
  }
  trait LoginCompanion

  abstract class HasPermission(val user: Rep[String], val password: Rep[String]) extends Auth[Boolean] {
    def eA = element[Boolean]
    def toOper = eval(user === "john.snow")
  }
  trait HasPermissionCompanion
}


trait AuthenticationsDsl extends ScalanDsl with impl.AuthenticationsAbs with Authentications
    with MonadsDsl {

  implicit val AuthCont: Cont[Auth] = new Cont[Auth] {
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[Auth[T]]
    def lift[T](implicit eT: Elem[T]) = element[Auth[T]]
    def unlift[T](implicit eFT: Elem[Auth[T]]) = eFT.asInstanceOf[AuthElem[T,_]].eA
    def getElem[T](fa: Rep[Auth[T]]) = fa.selfType1
    def unapply[T](e: Elem[_]) = e match {
      case e: AuthElem[_,_] => Some(e.asElem[Auth[T]])
      case _ => None
    }
  }

  class Auths[F[_]:Cont](implicit I: Inject[Auth,F]) {
    def login(u: Rep[String], p: Rep[String]): Rep[Free[F, SOption[String]]] = lift(Login(u, p))
    def hasPermission(u: Rep[String], p: Rep[String]): Rep[Free[F,Boolean]] = lift(HasPermission(u, p))
  }
  object Auths {
    implicit def instance[F[_]:Cont](implicit I: Inject[Auth,F]): Auths[F] = new Auths[F]
  }

  object AuthOper extends (Auth ~> Oper) {
    def cIn = container[Auth]
    def cOut = container[Oper]
    def apply[A:Elem](i: Rep[Auth[A]]): Rep[Oper[A]] = i.toOper
  }
}

trait AuthenticationsDslSeq extends ScalanDslSeq with impl.AuthenticationsSeq with MonadsDslSeq {
}

trait AuthenticationsDslExp extends ScalanDslExp with impl.AuthenticationsExp with MonadsDslExp {
}