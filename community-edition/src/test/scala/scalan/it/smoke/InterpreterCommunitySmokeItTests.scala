package scalan.it.smoke

import java.io.File
import java.lang.reflect.Method

import scalan.community._
import scalan.compilation.GraphVizExport
import scalan.it.BaseItTests
import scalan.linalgebra.{MatricesDslExp, MatricesDslSeq}
import scalan.parrays.{PArrayExamples, PArraysDslExp, PArraysDslSeq}
import scalan.{Interpreter, ScalanCtxExp, ScalanCtxSeq}

class InterpreterCommunitySmokeItTests extends BaseItTests {

  trait ProgCommunity extends ScalanCommunityDsl with PArrayExamples {
    lazy val simpleConst = fun {x: PA[Int] =>
      PArray.singleton(1)
    }
    lazy val expBaseArrays = fun { xss:Arr[Array[Int]] =>
      val pss1:Arr[PArray[Int]] = xss.map { xs: Rep[Array[Int]] => PArray(xs)}
      val res = pss1.map { ps: PA[Int] =>
        ps.arr
      }
      res
    }
  }

  def createInterpreter(front: ScalanCtxSeq, back: ScalanCtxExp with GraphVizExport): Interpreter = new Interpreter {
    val seq = front
    val staged = back
  }

  // TODO Create abstract class to eliminate duplicate code from InterpreterSmokeItTests
  def interpreterTestRun[A,B](front: ScalanCtxSeq /*with VectorsDslSeq*/, back: ScalanCtxExp with GraphVizExport /*with VectorsDslExp*/)
                             (fseq: front.Rep[A=>B], f: back.Exp[A=>B])
                             (name: String, input: front.Rep[A])
                             (implicit eA: front.Elem[A], eB: front.Elem[B]) : front.Rep[B] = {
    val dir = "it-out/" + prefix + "/" + name

    new File(dir).mkdirs()

    val interp = createInterpreter(front, back)

    interp.run[A,B](f.asInstanceOf[interp.staged.Exp[A=>B]])(dir, name, input, emitGraphs)(eA.asInstanceOf[interp.seq.Elem[A]], eB.asInstanceOf[interp.seq.Elem[B]])
  }

  class ProgSeq extends ProgCommunity with PArraysDslSeq with ScalanCommunitySeq with MatricesDslSeq
  val progSeq = new ProgSeq()

  // TODO remove VectorsDslExp!
  class ProgExp extends ProgCommunity with PArraysDslExp with ScalanCommunityExp with GraphVizExport with MatricesDslExp {
    // FIXME should be handled in the interpreter
    override def isInvokeEnabled(d: Def[_], m: Method) = true
  }

  val progStaged = new ProgExp()

  import progSeq._
  import scala.Array

  test("test9expBaseArrays") {
    val (in, out) = Array(Array(2,3), Array(4,5)) ->  Array(Array(2,3), Array(4,5))
    val seqRes = progSeq.expBaseArrays(in)
    seqRes should be(out)
    val res = interpreterTestRun(progSeq, progStaged)(progSeq.expBaseArrays, progStaged.expBaseArrays)("expBaseArrays", in)
    //println(res.mkString(" "))
    res should be(seqRes)
  }
}
