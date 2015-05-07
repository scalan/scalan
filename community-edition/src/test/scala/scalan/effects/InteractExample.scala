package scalan.effects

import scalan.examples.InteractionsDsl

/**
 * Created by slesarenko on 25/04/15.
 */
trait InteractExample extends InteractionsDsl {
  val OperM = Monad[Oper]

  def prg[F[_]:Cont](implicit I: Interacts[F]) = { import I._;
    for {
      uid <- ask("What's your user ID?")
      pwd <- ask("Password, please.")
      res <- tell(uid)
    } yield res
  }

  def prg2[F[_]:Cont](implicit I: Interacts[F]) = { import I._;
    for {
      uid <- ask("What's your user ID?")
      res <- IF (uid.contains("admin")) THEN { tell("Ok") } ELSE { tell("Forbidden") }
    } yield res
  }

  type App[A] = Interact[A]

  lazy val app: Rep[Free[App, Unit]] = prg[App]
  def runApp = app.run(InteractOper)

  lazy val app2: Rep[Free[App, Unit]] = prg2[App]
  def runApp2 = app2.run(InteractOper)
}
