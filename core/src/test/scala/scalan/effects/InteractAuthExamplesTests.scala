package scalan.examples

import java.io.File
import java.lang.reflect.Method

import scala.language.reflectiveCalls
import scalan.{TestContext, BaseTests, ScalanCtxExp}

class InteractAuthExamplesTests extends BaseTests {

  trait MyProg extends InteractionsDsl {
    val OperM = Monad[Oper]

    def prg[F[_]:Cont](implicit I: Interacts[F]) = {
      import I._;
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

  test("interactsStaged") {
    val ctx = new TestContext(this, "interactsStaged") with InteractionsDslExp with MyProg {
      override def isInvokeEnabled(d: Def[_], m: Method) = true
    }
    ctx.emit("app", ctx.app)
    ctx.emit("runApp", ctx.runApp)
  }

  trait MyCrossDomainProg extends InteractionsDsl with AuthenticationsDsl {
    val OperM = Monad[Oper]
    val KnowSecret = "KnowSecret"

    def prg[F[_]:Cont](implicit I: Interacts[F], A: Auths[F]) = {
      import A._
      import I._;
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
  }

  test("crossDomainStaged") {
    pending
    val ctx = new TestContext(this, "crossDomainStaged") with MyCrossDomainProg with InteractionsDslExp with AuthenticationsDslExp {
      override def isInvokeEnabled(d: Def[_], m: Method) = true
    }
    ctx.emit("app", ctx.app)
    ctx.emit("runApp", ctx.runApp)
  }

}
