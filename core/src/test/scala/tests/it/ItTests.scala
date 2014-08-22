package tests.it

import java.io.{File, PrintWriter}
import scalan.{ScalanCtxExp, ScalanCtxSeq, ScalanExp, ScalanSeq}
import scalan.codegen.{GraphVizExport, LangBackend}
import tests.BaseTests
import scala.language.postfixOps
import scala.reflect.runtime.universe._
import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers

trait ItTests extends BaseTests {

  lazy val prefix: String = getClass.getSimpleName
  def emitGraphs: Boolean = true

  def readFile(name: String): String =
    Source.fromFile(name).getLines().toIterator.mkString("\n")

  def assertFileContentCheck(name: String): Unit = {
    readFile(name + ".check") should be (readFile(name))
  }

  def lmsTestRun[A,B](front: ScalanSeq, back: LangBackend)
                  (fseq: front.Rep[A=>B], f: back.Exp[A=>B])
                  (name: String, input: front.Rep[A])
                  (implicit eA: front.Elem[A], eB: front.Elem[B]) /*: front.Rep[B]*/ =
  {
    val dir = "it-out/" + prefix + "/" + name
    val outFile = dir + "/out.txt"
    val inFile = dir + "/in.txt"

    new File(dir).mkdirs()

    back.run(dir, name, f, emitGraphs)
  }

  def lmsCheckRun[A,B](front: ScalanSeq, back: LangBackend)
                   (fseq: front.Rep[A=>B], f: back.Exp[A=>B])
                   (name: String, input: front.Rep[A], expOutput: front.Rep[B])
                   (implicit eA: front.Elem[A], eB: front.Elem[B]) {
    val output = lmsTestRun(front, back)(fseq, f)(name, input)
    //output.toString should be (expOutput.toString)
  }

  def testRun[A,B](front: ScalanSeq, back: LangBackend)
                  (fseq: front.Rep[A=>B], f: back.Exp[A=>B])
                  (name: String, input: front.Rep[A])
                  (implicit eA: front.Elem[A], eB: front.Elem[B]) /*: front.Rep[B]*/ =
  {
    val dir = "it-out/" + prefix + "/" + name
    val outFile = dir + "/out.txt"
    val inFile = dir + "/in.txt"

    new File(dir).mkdirs()

    // preparing input
    {
      val p = new PrintWriter(inFile)
      p.print(serialize(front)(input)(eA))
      p.close()
    }


    back.run(dir, name, f, emitGraphs)

    val outputS = readFile(outFile)
    val output = deserialize[B](front)(outputS)

    output
  }

  def checkRun[A,B](front: ScalanSeq, back: LangBackend)
                   (fseq: front.Rep[A=>B], f: back.Exp[A=>B])
                   (name: String, input: front.Rep[A], expOutput: front.Rep[B])
                   (implicit eA: front.Elem[A], eB: front.Elem[B]) {
    val output = testRun(front, back)(fseq, f)(name, input)
    output.toString should be (expOutput.toString)
  }

  def serialize(a: Array[Int]) =
    a.length + " " + a.mkString("[", " ", "]")

  def deserialize(s: String) = reader.readIntArray(s)

  def serialize[A](front: ScalanSeq)(a: front.Rep[A])(implicit eA: front.Elem[A]): String = {
    import front._
    eA match {
      case el: front.BaseElem[_] =>
        a.toString
      case el: front.PairElem[pa, pb] =>
        val (x1, x2) = a
        "(" + serialize(front)(x1)(el.eFst) + ", " + serialize(front)(x2)(el.eSnd) + ")"
    }
  }

  def deserialize[A](front: ScalanSeq)(s: String)(implicit eA: front.Elem[A]): front.Rep[A] =
    reader.read(front)(s)(eA)

  object reader extends JavaTokenParsers {

    def read[A](front: ScalanSeq)(s: String)(implicit eA: front.Elem[A]): front.Rep[A] =
      parse(repParser(front)(eA), s).get

    def readIntArray(s: String) = parse(arrayParser, s).get
    lazy val arrayParser =
      (wholeNumber ^^ {_.toInt}) ~ "[" ~ ((wholeNumber^^{_.toInt})*) ~ "]" ^^ {
        case size ~ _ ~ xs ~ _ =>
          assert(size == xs.length)
          xs.toArray
      }

    def repParser[A](front: ScalanSeq)(implicit e: front.Elem[A]): Parser[front.Rep[A]] = {
      import front._
      e match {
        case eb: BaseElem[pa] =>
          baseRepParser(front)(eb)
        case ep: PairElem[pa, pb] =>
          "(" ~ repParser(front)(ep.eFst) ~ "," ~ repParser(front)(ep.eSnd) ~ ")" ^^
            {case _ ~ p1 ~ _ ~ p2 ~ _ => Pair(p1, p2)}
      }
    }

    def baseRepParser[A](front: ScalanSeq)(implicit e: front.BaseElem[A]): Parser[front.Rep[A]] = {
      (e.tag match {
        case TypeTag.Boolean =>
          "true" ^^^ {true} | "false" ^^^ {false}
        case TypeTag.Int =>
          wholeNumber ^^ {_.toInt}
        case TypeTag.Float =>
          floatingPointNumber ^^ {_.toFloat}
        case TypeTag.Double =>
          floatingPointNumber ^^ {_.toDouble}
      }) ^^ {_.asInstanceOf[front.Rep[A]]}
    }
  }
}
