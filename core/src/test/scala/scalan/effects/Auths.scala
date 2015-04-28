package scalan.examples

import scala.reflect.runtime.universe._
import scalan._
import scalan.monads._

trait Authentications { self: AuthenticationsDsl =>
  val OperM: Monad[Oper]
  import OperM.toMonadic

  type RepAuth[A] = Rep[Auth[A]]
  trait Auth[A] extends Reifiable[Auth[A]] { self =>
    implicit def eA: Elem[A]
    def toOper: Rep[Oper[A]]
  }
  trait AuthCompanion

  abstract class Login(val user: Rep[String], val password: Rep[String]) extends Auth[Unit | String] {
    val eA = element[Unit | String]

    def toOper = {
      for {
        cond <- eval(user === "john.snow" && password === "Ghost")
      } yield
        IF (cond) {
          toRight("john.snow")
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

  implicit def AuthCont: Cont[Auth] = new Container[Auth] {
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[Auth[T]]
    def lift[T](implicit eT: Elem[T]) = element[Auth[T]]
  }

  class Auths[F[_]:Cont](implicit I: Inject[Auth,F]) {
    def login(u: Rep[String], p: Rep[String]): Rep[Free[F,Unit | String]] = lift(Login(u, p))
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

trait AuthenticationsDslSeq extends AuthenticationsDsl with impl.AuthenticationsSeq with ScalanCtxSeq with MonadsDslSeq {
}

trait AuthenticationsDslExp extends AuthenticationsDsl with impl.AuthenticationsExp with ScalanExp with MonadsDslExp {
}