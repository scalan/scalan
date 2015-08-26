package scalan.monads

import java.io.File
import java.lang.reflect.Method

import scala.language.reflectiveCalls
import scalan.{BaseCtxTests, ScalanCtxExp}

/**
 * User: Alexander Slesarenko   
 * Date: 8/3/14
 */
class ReaderTests extends BaseCtxTests {

  trait MyProg extends MonadsDsl { self =>
    type Env = (Int,String)
    type Read[A] = Reader[Env, A]
    implicit val M = Monad[Read]
    import M.toMonadic
    def ask: Rep[Read[Env]] = Reader.ask[Env].asRep[Read[Env]]

    lazy val prg = fun { (r: Rep[Env]) =>
      //val ask: Rep[Read[Int]] = Reader.ask[Int]
      val read = for {
        p <- ask
      } yield {
        val Pair(i,s) = p
        (i + 1, s.contains("abc"))
      }
      read.run(r)
    }

    lazy val seq = fun { (in: Rep[(List[Int],(Int, String))]) =>
      val Pair(xs, start) = in
      val read = M.sequence(xs.map(x => M.unit(x)))
      read.run(start)
    }

    lazy val f1 = fun[Int, Read[Int]]{(x: Rep[Int]) =>
      for { e <- ask } yield (x + e._1)
    }
    lazy val f2 = fun[Int, Read[Int]]{(x: Rep[Int]) =>
      val list = SList.rangeFrom0(x).map(i => f1(i))
      val listM = M.sequence(list)
      val res =  listM.map(l => l.reduce)
      res
    }
    lazy val f3 = fun[Int, Read[Int]]{(x: Rep[Int]) =>
      val list = M.replicateM(x)(f1(x))
      val res =  list.map(l => l.reduce)
      res
    }

    lazy val mapped = fun { (in: Rep[(Int => Read[Int], (List[Int],(Int, String)))]) =>
      val Pair(f, Pair(xs, start)) = in
      val read = M.sequence(xs.map(x => f(x)))
      read
    }
    lazy val mappedRun1 = fun { (in: Rep[(List[Int],(Int, String))]) =>
      val Pair(_, start) = in
      mapped((f1,in)).run(start)
    }
    lazy val mappedRun2 = fun { (in: Rep[(List[Int],(Int, String))]) =>
      val Pair(_, start) = in
      mapped((f2,in)).run(start)
    }
    lazy val mappedRun3 = fun { (in: Rep[(List[Int],(Int, String))]) =>
      val Pair(_, start) = in
      mapped((f3,in)).run(start)
    }
  }

  trait OptRules extends ScalanCtxExp with MonadsDslExp {
    override def isInvokeEnabled(d: Def[_], m: Method) = true
    override def shouldUnpack(e: Elem[_]) = true

    override def rewriteDef[T](d: Def[T]) = d match {
      //TODO this rule works only for this particular tests, but can be generalized
      case ListFoldRight(
              Def(ListMap(xs, Def(h: Lambda[a,_]))),
              Def(f: Lambda[e,List[c]]@unchecked),
              Def(g: Lambda[_,_])) if h.eB.isInstanceOf[FuncElem[_,_]] => {
        val source = xs.asRep[List[a]]
        val h1 = h.self.asRep[a => (e => c)]
        implicit val eA = source.elem.eItem
        implicit val eE = f.eA
        implicit val eC = f.eB.eItem

        fun { (env: Rep[e]) => {
          val ys: Rep[List[c]] = source.map(x => h1(x)(env))
          ys.foldRight(f.self(env)){ (p: Rep[(c, List[c])]) =>
            val Pair(y, acc) = p
            y :: acc
          }
        }}
      }
      case _ => super.rewriteDef(d)
    }
  }

  test("readerStaged") {
    val ctx = new TestContext with MonadsDslExp with MyProg {
      override def isInvokeEnabled(d: Def[_], m: Method) = true
    }
    ctx.emit("prg", ctx.prg)
  }

  test("readerSequence") {
    val ctx = new TestContext with OptRules with MyProg
    ctx.emit("seq", ctx.seq)
    ctx.emit("mapped", ctx.mapped)
    ctx.emit("mappedRun1", ctx.mappedRun1)
    ctx.emit("mappedRun2", ctx.mappedRun2)
  }

  test("readerReplicateM") {
    val ctx = new TestContext with OptRules with MyProg
    ctx.emit("mappedRun3", ctx.mappedRun3)
  }

}
