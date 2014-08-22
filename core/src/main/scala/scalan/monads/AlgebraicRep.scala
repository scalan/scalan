package scalan.monads

import scalan.common.Default
import scalan._
import scalan.common.Default._
import scala.reflect.runtime.universe._
//import scala.reflect.ClassTag

/**
 * User: Alexander Slesarenko   
 * Date: 8/2/14
 */
trait AlgebraicRep { self: Scalan =>


  trait ElemLifter[F[_]] {
    def lift[A:Elem]: Elem[F[A]]
  }
  object ElemLifter {
    class CoproductElem[F[_],G[_], A](eF: Elem[F[A]], eG: Elem[G[A]], eA: Elem[A]) extends Element[Coproduct[F,G,A]] {
      override def isEntityType = eF.isEntityType || eG.isEntityType || eA.isEntityType
      lazy val tag = {
//        implicit val tA = eA.tag
//        implicit val tF = eF.tag
//        implicit val tG = eG.tag
//        typeTag[Coproduct[F,G[_],A]]
        ???
      }
      lazy val defaultRep = ???
    }

    implicit def liftCoproduct[F[_],G[_]](implicit lF: ElemLifter[F], lG: ElemLifter[G]) = new ElemLifter[({ type f[x] = Coproduct[F, G, x]})#f] {
      override def lift[A: Elem] = new CoproductElem(lF.lift, lG.lift, element[A])
    }
    def apply[F[_]:ElemLifter]: ElemLifter[F] = implicitly[ElemLifter[F]]
  }

  case class Coproduct[F[_],G[_],A](run: Either[F[A],G[A]])

  sealed trait ~>[F[_],G[_]] { self =>
    def apply[A](f: F[A]): G[A]

//    def or[H[_]](f: H ~> G): ({ type f[x] = Coproduct[F, H, x]})#f ~> G =
//      new (({type f[x] = Coproduct[F,H,x]})#f ~> G) {
//        def apply[A](c: Rep[Coproduct[F,H,A]]): Rep[G[A]] = c.run match {
//          case Left(fa) => self(fa)
//          case Right(ha) => f(ha)
//        }
//      }
  }

  //def apply_coproduct[F[_],H[_],G[_],A](c: Rep[Coproduct[F,H,A]], f: F ~> G): Rep[G[A]]

  sealed trait Inject[F[_],G[_]] {
    def inj[A](sub: F[A]): G[A]
    def prj[A](sup: G[A]): Option[F[A]]
    def eGA[A:Elem]: Elem[G[A]]
  }

  object Inject {
    implicit def injRefl[F[_]](implicit lF: ElemLifter[F]) = new Inject[F,F] {
      def inj[A](sub: F[A]) = sub
      def prj[A](sup: F[A]) = Some(sup)
      def eGA[A: Elem] = lF.lift
    }

    implicit def injLeft[F[_],G[_]](implicit lF: ElemLifter[F], lG: ElemLifter[G]) = new Inject[F,({type λ[α] = Coproduct[F,G,α]})#λ] {
      def inj[A](sub: F[A]) = Coproduct(Left(sub))
      def prj[A](sup: Coproduct[F,G,A]) = sup.run match {
        case Left(fa) => Some(fa)
        case Right(_) => None
      }
      override def eGA[A: Elem] = ElemLifter[({type λ[α] = Coproduct[F,G,α]})#λ].lift
    }

    implicit def injRight[F[_],G[_],H[_]](implicit I: Inject[F,G], lG: ElemLifter[G], lH: ElemLifter[H]) =
      new Inject[F,({type λ[α] = Coproduct[H,G,α]})#λ] {
        def inj[A](sub: F[A]) = Coproduct(Right(I.inj(sub)))
        def prj[A](sup: Coproduct[H,G,A]) = sup.run match {
          case Left(_) => None
          case Right(x) => I.prj(x)
        }
        override def eGA[A: Elem] = ElemLifter[({type λ[α] = Coproduct[H,G,α]})#λ].lift
      }
  }

  sealed trait Free[F[_], A] {
    def flatMap[B](f: Rep[A] => Free[F,B]): Free[F,B] =
      this match {
        case Return(a) => f(a)
        case Bind(fx, g) =>
          Bind(fx, g andThen (_ flatMap f))
      }

    def map[B](f: Rep[A] => Rep[B]): Free[F,B] =
      flatMap(a => Return(f(a)))

    def foldMap(f: ({type λ[α] = Rep[F[α]]})#λ ~> Rep)/*(implicit resolver: Rep[F[_]] => F[_])*/: Rep[A] =
      this match {
        case Return(a) => a
        case Bind(fx, g) =>
          val a = f(fx)
          g(a).foldMap(f)
      }
  }
  case class Return[F[_],A](a: Rep[A]) extends Free[F,A]
  case class Bind[F[_],A,B]( a: Rep[F[A]], f: Rep[A] => Free[F,B] ) extends Free[F,B]

  def lift[F[_],G[_],A](f: F[A])(implicit I: Inject[F,G], eA: Elem[A]): Free[G,A] = {
    implicit val eGA: Elem[G[A]] = I.eGA
    Bind(I.inj(f), Return(_: Rep[A]))
  }


  // language of Interactions
  type RInteract[A] = Rep[Interact[A]]
  trait Interact[A] {}
  case class Ask(prompt: Rep[String]) extends Interact[String]
  case class Tell(msg: Rep[String]) extends Interact[Unit]

  class Interacts[F[_]](implicit I: Inject[Interact,F]) {
    def tell(msg: Rep[String]): Free[F, Unit] = lift(Tell(msg))
    def ask(prompt: Rep[String]): Free[F, String] = lift(Ask(prompt))
  }
  object Interacts {
    implicit def instance[F[_]](implicit I: Inject[Interact,F]): Interacts[F] = new Interacts[F]
  }
  def doAsk(prompt: Rep[String]): Rep[String]
  def doTell(msg: Rep[String]): Rep[Unit]
  val Console: RInteract ~> Rep
  case class InteractElem[A](eA: Elem[A]) extends Element[Interact[A]] {
    override def isEntityType = eA.isEntityType
    override def tag = ???
    override def defaultRep = ???
  }
  implicit def liftInteract = new ElemLifter[Interact] {
    override def lift[A: Elem] = new InteractElem(element[A])
  }

  // language of Authentication
  type UserID = String
  type Password = String
  type Permission = String
  type User = String

  type RAuth[A] = Rep[Auth[A]]
  sealed trait Auth[A]
  case class Login(u: Rep[UserID], p: Rep[Password]) extends Auth[User]
  case class HasPermission(u: Rep[User], p: Rep[Permission]) extends Auth[Boolean]

  class Auths[F[_]](implicit I: Inject[Auth,F]) {
    def login(id: Rep[UserID], pwd: Rep[Password]): Free[F,User] =
      lift(Login(id, pwd))
    def hasPermission(u: Rep[User], p: Rep[Permission]): Free[F,Boolean] =
      lift(HasPermission(u, p))
  }
  object Auths {
    implicit def instance[F[_]](implicit I: Inject[Auth,F]): Auths[F] = new Auths[F]
  }
  def doLogin(u: Rep[UserID], p: Rep[Password]): Rep[User]
  def doHasPermission(u: Rep[User], p: Rep[Permission]): Rep[Boolean]
  val TestAuth: RAuth ~> Rep
  case class AuthElem[A](eA: Elem[A]) extends Element[Auth[A]] {
    override def isEntityType = eA.isEntityType
    override def tag = ???
    override def defaultRep = ???
  }
  implicit def liftAuth = new ElemLifter[Auth] {
    override def lift[A: Elem] = new AuthElem(element[A])
  }

  val KnowSecret = "KnowSecret"

  def prg[F[_]](implicit I: Interacts[F]/*, A: Auths[F]*/): Free[F,User] = {
    /*import I._; import A._*/
    for {
      uid <- I.ask("What's your user ID?")
      pwd <- I.ask("Password, please.")
      /*u <- login(uid, pwd)*/
      _ <- I.tell(uid)
      //b <- u.map(hasPermission(_, KnowSecret)).getOrElse(Return(false))
      //_ <- if (b) tell("UUDDLRLRBA") else tell("Go away!")
    } yield uid
  }

  type App[A] = Interact[A] //Coproduct[Auth, Interact, A]

  def app: Free[App, User] = prg[App]
  def runApp = {
    val a = app
    a.foldMap(Console)/*(x => algebraResolver(x))*/
  }

  def algebraResolver[F[_]]: Rep[F[_]] => F[_]
}

trait ApplicativeSeq extends AlgebraicRep with ScalanCtxSeq {

//  def apply_~>[F[_],G[_],A](f: F ~> G, x: Rep[F[A]]): Rep[G[A]] = f.run match {
//    case Left(fa) => self(fa)
//    case Right(ha) => f(ha)
//  }
  override def doLogin(uid: Rep[UserID], pwd: Rep[Password]) =
    if (uid == "john.snow" && pwd == "Ghost")
      "john.snow"
    else "wrong"

  override def doHasPermission(u: Rep[User], p: Rep[Permission]) =
    u == "john.snow"

  override def doAsk(prompt: Rep[String]) = {
    println(prompt)
    "john.snow"
  }

  override def doTell(msg: Rep[String]) = {
    println(msg)
  }

  override def algebraResolver[F[_]] = x => x

  val Console: RInteract ~> Rep = new (RInteract ~> Rep) {
    def apply[A](i: Rep[Interact[A]]): Rep[A] = i match {
      case Ask(prompt) => doAsk(prompt)
      case Tell(msg) =>  doTell(msg)
    }
  }
  val TestAuth: RAuth ~> Rep = new (RAuth ~> Rep) {
    def apply[A](a: RAuth[A]) = a match {
      case Login(uid, pwd) => doLogin(uid, pwd)
      case HasPermission(u, p) => doHasPermission(u, p)
    }
  }

}

trait ApplicativeExp extends AlgebraicRep with ScalanCtxExp {

  override def doLogin(uid: Rep[UserID], pwd: Rep[Password]) = ???

  override def doHasPermission(u: Rep[User], p: Rep[Permission]) = ???

  override def doAsk(prompt: Rep[String]) = {
    ???
  }

  override def doTell(msg: Rep[String]) = {
    ???
  }

  override def algebraResolver[F[_]] = x => x match {
    case Def(d) => d.asInstanceOf[F[_]]
    case _ => !!!(s"cannot resolve algebra $x")
  }

  val Console: RInteract ~> Rep = new (RInteract ~> Rep) {
    def apply[A](i: Rep[Interact[A]]): Rep[A] = ???
//      i match {
//      case Def(Ask(prompt)) => doAsk(prompt)
//      case Def(Tell(msg)) =>  doTell(msg)
//    }
  }
  val TestAuth: RAuth ~> Rep = new (RAuth ~> Rep) {
    def apply[A](a: RAuth[A]) = ???
//      a match {
//      case Def(Login(uid, pwd)) => doLogin(uid, pwd)
//      case Def(HasPermission(u, p)) => doHasPermission(u, p)
//    }
  }

}

//trait MonadsDsl extends ScalanDsl with impl.MonadsAbs with Monads
//
//trait MonadsDslSeq extends MonadsDsl with impl.MonadsSeq with ScalanCtxSeq
//
//trait MonadsDslExp extends MonadsDsl with impl.MonadsExp with ScalanStaged