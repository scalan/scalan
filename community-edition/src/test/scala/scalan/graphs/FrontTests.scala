package scalan.graphs

import scala.language.reflectiveCalls
import scalan._

class FrontTests extends BaseTests { suite =>

  trait FrontProg extends Scalan with GraphsDsl {
    lazy val example = fun { (in: Rep[(Int, Front)]) =>
      val Pair(v, f) = in;
      f.append(v)
    }

    lazy val t1 = fun { (in: Rep[(Int,(Array[Int],Array[Boolean]))]) =>
      val Pair(v, Pair(xs, bits)) = in
      val c = Collection.fromArray(xs)
      val f = example(Pair(v, CollectionFront(c, PBitSet(bits))))
      f.set.arr
    }
    lazy val t2 = fun { (in: Rep[(Int,(List[Int],Array[Boolean]))]) =>
      val Pair(v, Pair(xs, bits)) = in
      val c = Collection.fromList(xs)
      val f = example(Pair(v, CollectionFront(c, PBitSet(bits))))
      f.set.arr
    }

    lazy val t3 = fun { (in: Rep[List[Int]]) => ListCollection(in).asRep[BaseCollection[Int]].arr }
  }

  class FrontProgStaged(testName: String) extends TestContext(this, testName) with  FrontProg with GraphsDslExp {
  }
  class FrontProgSeq(testName: String) extends ScalanCtxSeq with  FrontProg with GraphsDslSeq {
  }

  test("deepSpec") {
    val ctx = new FrontProgStaged("deepSpec")
    ctx.emit("t1", ctx.t1)
    ctx.emit("t2", ctx.t2)
  }

  test("autoSuper") {
    val ctx = new FrontProgStaged("autoSuper")
    ctx.emit("t3", ctx.t3)
  }



}
