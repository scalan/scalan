// important: this example is in README. If you change it, please also change README.md!

import java.io.File

import scalan.{ScalanCommunityDslExp, ScalanCommunityDslSeq, ScalanCommunityDsl}
import scalan.compilation.GraphVizConfig
import scalan.compilation.lms.{CommunityLmsBackend, CommunityBridge}
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.it.BaseItTests

// ScalanCommunityDsl includes ScalanCommunity and all DSLs defined in that project
trait HelloScalan extends ScalanCommunityDsl {
  lazy val run = fun { p: Rep[(Array[Array[Double]], Array[Double])] =>
    val Pair(m, v) = p
    val width = m(0).length
    val matrix: Matrix[Double] = CompoundMatrix(Collection(m.map { r: Arr[Double] => DenseVector(Collection(r))}), width)
    val vector: Vector[Double] = DenseVector(Collection(v))
    (matrix * vector).items.arr
  }
  // example input
  val matrix = Array(Array(1.0, 2.0), Array(3.0, 5.0))
  val vector = Array(2.0, 3.0)
  val input = (matrix, vector)
}

// to run: scalan-lms-backend/test:runMain HelloScalanSeq
object HelloScalanSeq extends HelloScalan with ScalanCommunityDslSeq {
  def result = run(input)

  def main(args: Array[String]) = {
    println(result.mkString(","))
  }
}

// to run: scalan-lms-backend/test:runMain HelloScalanExp
object HelloScalanExp {
  // allows use of standard Scala library, commented out to make tests faster
  // override val defaultCompilerConfig = CompilerConfig(Some("2.10.4"), Seq.empty)

  val program = new HelloScalan with ScalanCommunityDslExp

  val compiler = new CommunityLmsCompilerScala(program)
  import compiler._
  import compiler.scalan._

  def result = {
    // output directory
    val dir = new File("it-out")
    val compiled = compiler.buildExecutable(
      dir,
      // generated class name
      "HelloScalan1",
      // function to compile
      run,
      // write .dot files containing graph IR with default settings
      GraphVizConfig.default)
    // not necessary if you just want to generate
    // and compile the program
    execute(compiled, input)
  }

  def main(args: Array[String]): Unit = {
    println(result.mkString(","))
  }
}

class ReadmeExampleItTests extends BaseItTests {
  test("Examples from README run") {
    HelloScalanExp.result shouldEqual HelloScalanSeq.result
  }
}
