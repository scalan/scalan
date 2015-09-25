package scalan.effects

import scalan.examples.InteractionsDsl

trait InteractExample extends InteractionsDsl {
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
      res <- IF ((uid !== (null: String))) {
        IF (uid.contains("admin")) THEN { tell("Ok") } ELSE { tell("Forbidden") }
      } ELSE {
        tell("enter again")
      }
    } yield res
  }

  type App[A] = Interact[A]

  lazy val app: Rep[Free[App, Unit]] = prg[App]
  def runApp = app.run(InteractOper)

  lazy val app2: Rep[Free[App, Unit]] = prg2[App]
  def runApp2 = app2.run(InteractOper)

  lazy val runAppW = fun {in: Rep[Int] =>
    val app = runApp
    app(in)._2
  }
  lazy val runApp2W = fun {in: Rep[Int] =>
    val app = runApp2
    app(in)._2
  }

}
