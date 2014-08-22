package scalan.monads

import scalan.common.Default
import scalan._

/**
 * User: Alexander Slesarenko   
 * Date: 8/2/14
 * The code is taken from https://gist.github.com/runarorama/a8fab38e473fafa0921d
 */
trait Algebraic {

  case class Coproduct[F[_],G[_],A](run: Either[F[A],G[A]])

  sealed trait ~>[F[_],G[_]] { self =>
    def apply[A](f: F[A]): G[A]

    def or[H[_]](f: H ~> G): ({ type f[x] = Coproduct[F, H, x]})#f ~> G =
      new (({type f[x] = Coproduct[F,H,x]})#f ~> G) {
        def apply[A](c: Coproduct[F,H,A]): G[A] = c.run match {
          case Left(fa) => self(fa)
          case Right(ha) => f(ha)
        }
      }
  }

  sealed trait Inject[F[_],G[_]] {
    def inj[A](sub: F[A]): G[A]
    def prj[A](sup: G[A]): Option[F[A]]
  }

  object Inject {
    implicit def injRefl[F[_]] = new Inject[F,F] {
      def inj[A](sub: F[A]) = sub
      def prj[A](sup: F[A]) = Some(sup)
    }

    implicit def injLeft[F[_],G[_]] = new Inject[F,({type λ[α] = Coproduct[F,G,α]})#λ] {
      def inj[A](sub: F[A]) = Coproduct(Left(sub))
      def prj[A](sup: Coproduct[F,G,A]) = sup.run match {
        case Left(fa) => Some(fa)
        case Right(_) => None
      }
    }

    implicit def injRight[F[_],G[_],H[_]](implicit I: Inject[F,G]) =
      new Inject[F,({type f[x] = Coproduct[H,G,x]})#f] {
        def inj[A](sub: F[A]) = Coproduct(Right(I.inj(sub)))
        def prj[A](sup: Coproduct[H,G,A]) = sup.run match {
          case Left(_) => None
          case Right(x) => I.prj(x)
        }
      }
  }

  // --------------------- Monads --------------------------
  trait Monad[M[_]] {
    def pure[A](a: A): M[A]
    def flatMap[A,B](a: M[A])(f: A => M[B]): M[B]
  }

  object Monad {
    def apply[F[_]:Monad]: Monad[F] = implicitly[Monad[F]]
  }

  trait Free[F[_],A] {
    def flatMap[B](f: A => Free[F,B]): Free[F,B] =
      this match {
        case Return(a) => f(a)
        case Bind(fx, g) =>
          Bind(fx, g andThen (_ flatMap f))
      }

    def map[B](f: A => B): Free[F,B] =
      flatMap(a => Return(f(a)))

    def foldMap[G[_]:Monad](f: F ~> G): G[A] =
      this match {
        case Return(a) => Monad[G].pure(a)
        case Bind(fx, g) =>
          Monad[G].flatMap(f(fx)) { a =>
            g(a).foldMap(f)
          }
      }
  }

  case class Return[F[_],A](a: A) extends Free[F,A]

  case class Bind[F[_],A,B]( a: F[A], f: A => Free[F,B] ) extends Free[F,B]

  def lift[F[_],G[_],A](f: F[A])(implicit I: Inject[F,G]): Free[G,A] =
    Bind(I.inj(f), Return(_:A))

  type Id[A] = A

  implicit val identityMonad: Monad[Id] = new Monad[Id] {
    def pure[A](a: A) = a
    def flatMap[A,B](a: A)(f: A => B) = f(a)
  }

  trait Interact[A] {}

  case class Ask(prompt: String)
    extends Interact[String]

  case class Tell(msg: String)
    extends Interact[Unit]

  object Console extends (Interact ~> Id) {
    def apply[A](i: Interact[A]): Id[A] = i match {
      case Ask(prompt) =>
        println(prompt)
        readLine()
      case Tell(msg) =>
        println(msg)
    }
  }

  type Tester[A] = Map[String, String] => (List[String], A)

  object TestConsole extends (Interact ~> Tester) {
    def apply[A](i: Interact[A]) = i match {
      case Ask(prompt) => m => (List(), m(prompt))
      case Tell(msg) => _ => (List(msg), ())
    }
  }

  implicit val testerMonad = new Monad[Tester] {
    def pure[A](a: A) = _ => (List(), a)
    def flatMap[A,B](t: Tester[A])(f: A => Tester[B]) =
      m => {
        val (o1, a) = t(m)
        val (o2, b) = f(a)(m)
        (o1 ++ o2, b)
      }
  }

  type UserID = String
  type Password = String
  type Permission = String
  case class User(id: String)

  sealed trait Auth[A]

  case class Login(u: UserID, p: Password) extends Auth[Option[User]]

  case class HasPermission(u: User, p: Permission) extends Auth[Boolean]

  val TestAuth: Auth ~> Id = new (Auth ~> Id) {
    def apply[A](a: Auth[A]) = a match {
      case Login(uid, pwd) =>
        if (uid == "john.snow" && pwd == "Ghost")
          Some(User("john.snow"))
        else None
      case HasPermission(u, _) =>
        u.id == "john.snow"
    }
  }

  class Interacts[F[_]](implicit I: Inject[Interact,F]) {
    def tell(msg: String): Free[F,Unit] = lift(Tell(msg))
    def ask(prompt: String): Free[F,String] = lift(Ask(prompt))
  }

  class Auths[F[_]](implicit I: Inject[Auth,F]) {
    def login(id: UserID, pwd: Password): Free[F,Option[User]] =
      lift(Login(id, pwd))
    def hasPermission(u: User, p: Permission): Free[F,Boolean] =
      lift(HasPermission(u, p))
  }

  object Auths {
    implicit def instance[F[_]](implicit I: Inject[Auth,F]): Auths[F] = new Auths[F]
  }

  object Interacts {
    implicit def instance[F[_]](implicit I: Inject[Interact,F]): Interacts[F] = new Interacts[F]
  }

  val KnowSecret = "KnowSecret"

  def prg[F[_]](implicit I: Interacts[F], A: Auths[F]) = {
    import I._; import A._
    for {
      uid <- ask("What's your user ID?")
      pwd <- ask("Password, please.")
      u <- login(uid, pwd)
      b <- u.map(hasPermission(_, KnowSecret)).getOrElse(Return(false))
      _ <- if (b) tell("UUDDLRLRBA") else tell("Go away!")
    } yield ()
  }

  type App[A] = Coproduct[Auth, Interact, A]

  val app: Free[App, Unit] = prg[App]

  def runApp() = app.foldMap(TestAuth or Console)
}

//trait MonadsDsl extends ScalanDsl with impl.MonadsAbs with Monads
//
//trait MonadsDslSeq extends MonadsDsl with impl.MonadsSeq with ScalanCtxSeq
//
//trait MonadsDslExp extends MonadsDsl with impl.MonadsExp with ScalanStaged