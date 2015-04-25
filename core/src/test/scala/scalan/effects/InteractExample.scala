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

  type App[A] = Interact[A]

  lazy val app: Rep[Free[App, Unit]] = prg[App]

  def runApp = app.run(InteractOper)
}
