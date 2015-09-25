package scalan.effects

import scalan.examples.{AuthenticationsDsl, InteractionsDsl}
import scala.language.reflectiveCalls

trait CrossDomainExample extends InteractionsDsl with AuthenticationsDsl {
  override lazy val OperM: Monad[Oper] = Monad[Oper]
  val KnowSecret = "KnowSecret"

  def prg[F[_]:Cont](implicit I: Interacts[F], A: Auths[F]) = {
    import I._, A._;
    for {
      uid <- ask("What's your user ID?")
      pwd <- ask("Password, please.")
      u <- login(uid, pwd)
      b <- u.map(hasPermission(_, KnowSecret)).getOrElse(Return[F,Boolean](false))
      res <- IF (b) {
        for { _ <- tell(toRep("Hi ") + uid); res <- tell("Ok!") } yield res
      } ELSE {
        for { _ <- tell(toRep("Sorry ") + uid); res <- tell("Go away!") } yield res
      }
    } yield res
  }

  type App[A] = Coproduct[Auth, Interact, A]

  lazy val app = prg[App]

  def runApp = app.run(AuthOper or InteractOper)

  lazy val runAppW = fun {in: Rep[Int] =>
    val app = runApp
    app(in)._2
  }
}
