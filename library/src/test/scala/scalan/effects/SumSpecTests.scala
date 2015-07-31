package scalan.monads

import java.io.File
import java.lang.reflect.Method

import scala.language.reflectiveCalls
import scalan.{BaseCtxTests, ScalanCtxExp}

class SumSpecTests extends BaseCtxTests {

  trait MyProg extends MonadsDsl {
      def sum[F[_]:Cont](F: Monad[F])(n: Rep[Int])(f: Rep[F[Int]] => Rep[Int]): Rep[Int] = {
        import F.toMonadic
        f {
          val is = SList.rangeFrom0(n).map(i => F.unit(i))

          is.foldLeft[F[Int]](F.unit(0)) { (in: Rep[(F[Int],F[Int])]) =>
            val Pair(facc,fi) = in
            for {
              acc <- facc
              i <- fi
            } yield {
              (acc + i)
            }
          }
        }
      }

    def sum2[F[_]:Cont](F: Monad[F])(n: Rep[Int])(f: Rep[F[Int]] => Rep[Int]): Rep[Int] = {
      import F.toMonadic
      f {
        val is = SList.rangeFrom0(n).map(i => F.unit(i))
        for {
          is <- F.sequence(is)
        } yield is.length
      }
    }

  }

  trait MyProgExp extends ScalanCtxExp with MonadsDslExp with MyProg {
    override def isInvokeEnabled(d: Def[_], m: Method) = true
    override def shouldUnpack(e: Elem[_]) = true
  }

  test("identityMonad") {
    val ctx = new TestContext("identityMonad") with MyProgExp {
      lazy val runSum = fun {(n: Rep[Int]) =>
        sum(Monad[Id])(n)(r => r)
      }
    }
    ctx.emit("runSum", ctx.runSum)
  }

  test("readerMonad") {
    val ctx = new TestContext("readerMonad") with MyProgExp with MonadsDslExp {
      override def rewriteDef[T](d: Def[T]) = d match {
        //TODO this rule works only for this particular tests, but can be generalized
        case ListFoldLeft(
        Def(ListMap(xs, Def(h: Lambda[a,_]))),
        Def(f: Lambda[e,Int]@unchecked),
        Def(g: Lambda[_,_])) => {
          val source = xs.asRep[List[a]]
          val h1 = h.self.asRep[a => (e => Int)]
          implicit val eA = source.elem.eItem
          implicit val eE = f.eA
          //implicit val eC = f.eB

          fun { (env: Rep[e]) => {
            val ys: Rep[List[Int]] = source.map(x => h1(x)(env))
            val res = ys.foldLeft(f.self(env)){ (p: Rep[(Int, Int)]) =>
              val Pair(acc, y) = p
              acc + y
            }
            res
          }}
        }
        case _ => super.rewriteDef(d)
      }

      type Env = (Int,String)
      type Read[A] = Reader[Env, A]
      val M = Monad[Read]
      lazy val runSum = fun {(in: Rep[(Env,Int)]) =>
        val Pair(env, n) = in
        sum(M)(n)(r => r.run(env))
      }
    }
    ctx.emit("runSum", ctx.runSum)
  }

  test("operMonad") {
    val ctx = new TestContext("operMonad") with MyProgExp {
      lazy val runSum = fun {(n: Rep[Int]) =>
        sum(Monad[Oper])(n)(r => r(0)._2)
      }
    }
    ctx.emit("runSum", ctx.runSum)
  }

  test("freeIdMonad") {
    val ctx = new TestContext("freeIdMonad") with MyProgExp {
      val idFreeM = freeMonad[Id]
      type IdFree[A] = Free[Id,A]
      lazy val runSum = fun {(n: Rep[Int]) =>
        sum[IdFree](idFreeM)(n)((r: RFree[Id,Int]) => {
          emit("runSum-r", r)
          r.run[Oper](IdOper)(operationMonad)(0)._2
        })(freeMonad[Id])
      }
      lazy val runSum2 = fun {(n: Rep[Int]) =>
        sum2[IdFree](idFreeM)(n)((r: RFree[Id,Int]) => {
          emit("runSum2-r", r)
          r.run[Oper](IdOper)(operationMonad)(0)._2
        })(freeMonad[Id])
      }
    }
    ctx.emit("runSum", ctx.runSum)
    ctx.emit("runSum2", ctx.runSum2)
  }


  test("freeOperMonad") {
    val ctx = new TestContext("freeOperMonad") with MyProgExp {
      val operFreeM = freeMonad[Oper]
      type OperFree[A] = Free[Oper,A]
      lazy val runSum = fun {(n: Rep[Int]) =>
        sum[OperFree](operFreeM)(n)((r: RFree[Oper,Int]) => {
          emit("runSum", r)
          r.run[Oper](OperOper)(operationMonad)(0)._2
        })(freeMonad[Oper])
      }
      lazy val runSum2 = fun {(n: Rep[Int]) =>
        sum2[OperFree](operFreeM)(n)((r: RFree[Oper,Int]) => {
          emit("runSum-r", r)
          r.run[Oper](OperOper)(operationMonad)(0)._2
        })(freeMonad[Oper])
      }
    }
    ctx.emit("runSum", ctx.runSum)
    ctx.emit("runSum2", ctx.runSum2)
  }
}
