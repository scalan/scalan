package scalan.collections

import scala.collection.immutable.HashSet
import scala.language.reflectiveCalls
import scalan._

class HashSetTests extends BaseCtxTests {
  trait HashSetSimple extends HashSetsDsl {
    lazy val tElem = element[HashSet[Int]]
    lazy val empty = SHashSet.empty[Int]

    lazy val t1 = fun { (t: Rep[SHashSet[Int]]) => t }
    lazy val t2 = fun { (in: Rep[(SHashSet[Int],Int)]) => val Pair(t, i) = in; t + i }
    lazy val t3 = fun { (e: Rep[Int]) => SHashSet.empty[Int] + e }
    lazy val t4 = fun { (t: Rep[SHashSet[Int]]) => t.map(fun { x => x + 1 }) }
    lazy val t5 = fun { (in: Rep[(SHashSet[Int],Int)]) => val Pair(t, i) = in; t + i }
    lazy val t6 = fun { (t: Rep[(SHashSet[Int],Int)]) => {
      t._1.map(fun { x => x + t._2 })
    }}
    lazy val t7 = fun { (t: Rep[(SHashSet[Int],Int)]) => {
      t._1.fold(t._2)(fun { x => x._1 + x._2 })
    }}

  }

  test("simpleHashsetStaged") {
    val ctx = new TestContext with HashSetSimple with HashSetsDslExp {
      def test() = {
        //assert(!isInlineThunksOnForce, "precondition for tests")
        {
//TODO make this work (recognizer should deal with BaseTypeElem)
//          val Def(Lambda(_, _, x, SThrowableMethods.getMessage(obj))) = t1
//          assert(x == obj)
        }
      }
    }

    ctx.test
    ctx.emit("empty", ctx.empty)
    ctx.emit("t1", ctx.t1)
    ctx.emit("t2", ctx.t2)
    ctx.emit("t3", ctx.t3)
    ctx.emit("t4", ctx.t4)
    ctx.emit("t5", ctx.t5)
    ctx.emit("t6", ctx.t6)
    ctx.emit("t7", ctx.t7)
  }

  test("simpleHashsetSeq") {
    val ctx = new ScalanDslSeq with HashSetSimple with HashSetsDslSeq {
      def test() = {
        //assert(!isInlineThunksOnForce, "precondition for tests")

      }
    }
    import ctx._
    ctx.test

    {
      val res = ctx.t2((SHashSet.empty[Int], 10))
      assertResult(SHashSetImpl(HashSet(10)))(res)
    }
    {
      val res = ctx.t3(10)
      assertResult(SHashSetImpl(HashSet(10)))(res)
    }
    {
      val res = ctx.t4(SHashSetImpl(HashSet(10, 20, 30)))
      assertResult(SHashSetImpl(HashSet(11, 21, 31)))(res)
    }
    {
      val res = ctx.t7((SHashSetImpl(HashSet(10, 20, 30)),0))
      assertResult(60)(res)
    }
  }
}
