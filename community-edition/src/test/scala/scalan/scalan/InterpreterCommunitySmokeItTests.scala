package scalan.scalan

import scalan.arrays.{PArraysDslSeq, PArraysDslExp}
import scalan.{Interpreter, ScalanCtxSeq, ScalanCtxExp}
import scalan.codegen.{GraphVizExport}
import scalan.linalgebra.{VectorsDslSeq, VectorsDslExp}
import scalan.scalan.arrays.PArrayExamples
import scalan.it.{InterpreterSmokeItTests, ItTests}
import scalan.community.{InterpreterCommunity, ScalanCommunitySeq, ScalanCommunityExp, ScalanCommunity}

class InterpreterCommunitySmokeItTests extends InterpreterSmokeItTests {
  import scala.Array

  trait ProgCommunity extends Prog with ScalanCommunity with PArrayExamples {
    lazy val simpleConst = fun {x: PA[Int] =>
      PArray.singleton(1)
    }
    lazy val simpleArrGet = fun {in: Rep[(Array[Int], Int)] =>
      val arr = in._1
      val ind = in._2
      arr(ind)
    }
    lazy val simpleMap = fun {x: Rep[Array[Int]] =>
      val x1 = x.map {y:Rep[Int] => y+1}
      x1
    }
    lazy val simpleMapNested = fun {x: Rep[(Array[Array[Double]], Int)] =>
      val x1 = x._1.map {y:Rep[Array[Double]] => y(x._2)}
      x1
    }
    lazy val simpleZip = fun {x: Rep[Array[Int]] =>
      val x1 = x.map {y:Rep[Int] => y+2}
      x1 zip x
    }
    lazy val simpleZipWith = fun {x: Rep[Array[Int]] =>
      val x1 = x.map {y:Rep[Int] => y+3}
      val x2 = x1 zip x
      val x3 = x2.map {y:Rep[(Int,Int)] => y._1 * y._2}
      x3
    }

    lazy val simpleReduce = fun {x: Rep[Array[Int]] =>
      val curMonoid: RepMonoid[Int] = IntRepPlusMonoid
      val x1 = x reduce(curMonoid)
      x1
    }
    lazy val mvMul = fun { in:Rep[(Array[Array[Int]], Array[Int])] =>
      val mat = in._1
      val vec = in._2
      val res = mat map {row: Rep[Array[Int]] =>
        val x1 = row zip vec
        val x2 = x1.map {y:Rep[(Int,Int)] => y._1 * y._2}
        x2 reduce (IntRepPlusMonoid)
      }
      res
    }
    lazy val expBaseArrays = fun { xss:Arr[Array[Int]] =>
      val pss1:Arr[PArray[Int]] = xss.map { xs: Rep[Array[Int]] => PArray(xs)}
      val res = pss1.map { ps: PA[Int] =>
        ps.arr
      }
      res
    }
    lazy val indexRangeLength = fun { in:Rep[Array[Int]] =>
      val res = in.map { xs: Rep[Int] => array_rangeFrom0(in.length + xs)}
      res
    }
    lazy val filterArray = fun { in:Rep[Array[Int]] =>
      val res = in.filter { x => x !== 0 }
      res
    }
    lazy val gatherArray = fun { in: Rep[(Array[Int], Array[Int])] =>
      val res = in._1.apply(in._2)
      res
    }
  }

  // FIXME make front/back parameter types into type members to avoid cast
  override def createInterpreter(front: ScalanCtxSeq, back: ScalanCtxExp with GraphVizExport): Interpreter = new InterpreterCommunity {
    val seq = front.asInstanceOf[ScalanCommunitySeq]
    val staged = back.asInstanceOf[ScalanCommunityExp]
  }

  class ProgSeq extends ProgCommunity with PArraysDslSeq with ScalanCommunitySeq with VectorsDslSeq
  override val progSeq = new ProgSeq()
  import progSeq._

  // TODO remove VectorsDslExp!
  class ProgExp extends ProgCommunity with PArraysDslExp with ScalanCommunityExp with GraphVizExport with VectorsDslExp

  override val progStaged = new ProgExp() {
    this.invokeEnabled = true
  }

  test("test2simpleArrGet") {
    val (in, out) = (Array(2,3),1) -> 3
    val seqRes = progSeq.simpleArrGet(in)
    seqRes should be(out)
    val res = interpreterTestRun(progSeq, progStaged)(progSeq.simpleArrGet, progStaged.simpleArrGet)("simpleArrGet", in)
    res should be(seqRes)
  }
  test("test3simpleMap") {
    val (in, out) = Array(2,3) -> Array(3,4)
    val seqRes = progSeq.simpleMap(in)
    seqRes should be(out)
    val res = interpreterTestRun(progSeq, progStaged)(progSeq.simpleMap, progStaged.simpleMap)("simpleMap", in)
    //println(res.mkString(" "))
    res should be(seqRes)
  }
  test("test4simpleMapNested") {
    val (in, out) = (Array(Array(2.0,3.0), Array(3.0,4.0)), 1) -> Array(3.0,4.0)
    val seqRes = progSeq.simpleMapNested(in)
    seqRes should be(out)
    val res = interpreterTestRun(progSeq, progStaged)(progSeq.simpleMapNested, progStaged.simpleMapNested)("simpleMapNested", in)
    //println(res.mkString(" "))
    res should be(seqRes)
  }
  test("test5simpleZip") {
    val (in, out) = Array(2,3) -> Array((4,2), (5,3))
    val seqRes = progSeq.simpleZip(in)
    seqRes should be(out)
    val res = interpreterTestRun(progSeq, progStaged)(progSeq.simpleZip, progStaged.simpleZip)("simpleZip", in)
    //println(res.mkString(" "))
    res should be(seqRes)
  }
  test("test6simpleZipWith") {
    val (in, out) = Array(2,3) -> Array(10,18)
    val seqRes = progSeq.simpleZipWith(in)
    seqRes should be(out)
    val res = interpreterTestRun(progSeq, progStaged)(progSeq.simpleZipWith, progStaged.simpleZipWith)("simpleZipWith", in)
    //println(res.mkString(" "))
    res should be(seqRes)
  }

  test("test7simpleReduce") {
    val (in, out) = Array(2,3) -> 5
    val seqRes = progSeq.simpleReduce(in)
    seqRes should be(out)
    val res = interpreterTestRun(progSeq, progStaged)(progSeq.simpleReduce, progStaged.simpleReduce)("simpleReduce", in)
    //println(res)
    res should be(seqRes)
  }
  test("test8mvMul") {
    val (in, out) = (Array(Array(2,3), Array(4,5)), Array(6,7))  -> Array(33,59)
    val seqRes = progSeq.mvMul(in)
    seqRes should be(out)
    val res = interpreterTestRun(progSeq, progStaged)(progSeq.mvMul, progStaged.mvMul)("mvMul", in)
    //println(res.mkString(" "))
    res should be(seqRes)

  }
  test("test9expBaseArrays") {
    val (in, out) = Array(Array(2,3), Array(4,5)) ->  Array(Array(2,3), Array(4,5))
    val seqRes = progSeq.expBaseArrays(in)
    seqRes should be(out)
    val res = interpreterTestRun(progSeq, progStaged)(progSeq.expBaseArrays, progStaged.expBaseArrays)("expBaseArrays", in)
    //println(res.mkString(" "))
    res should be(seqRes)
  }
  test("test10indexRangeLength") {
    val (in, out) = Array(2,3) ->  Array(Array(0,1,2,3), Array(0,1,2,3,4))
    val seqRes = progSeq.indexRangeLength(in)
    seqRes should be(out)
    val res = interpreterTestRun(progSeq, progStaged)(progSeq.indexRangeLength, progStaged.indexRangeLength)("indxRangeLength", in)
    //println(res.mkString(" "))
    res should be(seqRes)
  }
  test("test11filter") {
    val (in, out) = Array(2,0,4,0,5,0) ->  Array(2,4,5)
    val seqRes = progSeq.filterArray(in)
    seqRes should be(out)
    val res = interpreterTestRun(progSeq, progStaged)(progSeq.filterArray, progStaged.filterArray)("filterArray", in)
    //println(res.mkString(" "))
    res should be(seqRes)
  }
  test("test12gather") {
    val (in, out) = (Array(2,0,4,0,5,0), Array(0,2,4)) ->  Array(2,4,5)
    val seqRes = progSeq.gatherArray(in)
    seqRes should be(out)
    val res = interpreterTestRun(progSeq, progStaged)(progSeq.gatherArray, progStaged.gatherArray)("filterArray", in)
    //println(res.mkString(" "))
    res should be(seqRes)
  }
}
