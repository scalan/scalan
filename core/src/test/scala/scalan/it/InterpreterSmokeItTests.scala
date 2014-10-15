package scalan.it

import java.lang.reflect.Method

import scalan.{ScalanDsl, ScalanCtxSeq, ScalanCtxExp, Interpreter}
import scalan.compilation.{GraphVizExport}
import java.io.File

/**
 * Created with IntelliJ IDEA.
 * User: afilippov
 * Date: 7/3/14
 * Time: 3:25 PM
 * To change this template use File | Settings | File Templates.
 */
class InterpreterSmokeItTests extends BaseItTests {
  def createInterpreter(front: ScalanCtxSeq, back: ScalanCtxExp with GraphVizExport): Interpreter = new Interpreter {
    val seq = front
    val staged = back
  }

  def interpreterTestRun[A,B](front: ScalanCtxSeq /*with VectorsDslSeq*/, back: ScalanCtxExp with GraphVizExport /*with VectorsDslExp*/)
                             (fseq: front.Rep[A=>B], f: back.Exp[A=>B])
                             (name: String, input: front.Rep[A])
                             (implicit eA: front.Elem[A], eB: front.Elem[B]) : front.Rep[B] =
  {
    val dir = "it-out/" + prefix + "/" + name

    import scalan.Interpreter

    new File(dir).mkdirs()

    val interp = createInterpreter(front, back)

    interp.run[A,B](f.asInstanceOf[interp.staged.Exp[A=>B]])(dir, name, input, emitGraphs)(eA.asInstanceOf[interp.seq.Elem[A]], eB.asInstanceOf[interp.seq.Elem[B]])
  }

  /*def intepreterCheckRun[A,B](front: ScalanSeq, back: LangBackend)
                      (fseq: front.Rep[A=>B], f: back.Exp[A=>B])
                      (name: String, input: front.Rep[A], expOutput: front.Rep[B])
                      (implicit eA: front.Elem[A], eB: front.Elem[B]) {
    val output = interpreterTestRun(front, back)(fseq, f)(name, input)
    //output.toString should be (expOutput.toString)
  } */

  trait Prog extends ScalanDsl {
    lazy val simpleArith = fun {x: Rep[Int] => x*x + 2}
    lazy val simpleFirst = fun {x: Rep[(Int,Int)] => x._1}
  }

  class ProgSeq extends Prog with ScalanCtxSeq
  val progSeq: Prog with ScalanCtxSeq = new ProgSeq()

  class ProgExp extends Prog with ScalanCtxExp with GraphVizExport

  val progStaged: Prog with ScalanCtxExp = new ProgExp() {
    // FIXME should be handled in the interpreter
    override def isInvokeEnabled(d: Def[_], m: Method) = true
  }

  import progSeq._

  test("test0simpleArith") {
    val (in, out) = 2 -> 6
    val seqRes = progSeq.simpleArith(in)
    seqRes should be(out)
    val res = interpreterTestRun(progSeq, progStaged)(progSeq.simpleArith, progStaged.simpleArith)("simpleArith", in)
    res should be(seqRes)
  }
  test("test1simpleFirst") {
    val (in, out) = (2,3) -> 2
    val seqRes = progSeq.simpleFirst(in)
    seqRes should be(out)
    val res = interpreterTestRun(progSeq, progStaged)(progSeq.simpleFirst, progStaged.simpleFirst)("simpleFirst", in)
    res should be(seqRes)
  }
}
