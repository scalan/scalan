// important: this example is in README. If you change it, please also change README.md!

import java.io.File

import scalan.compilation.GraphVizConfig
import scalan.compilation.lms.linalgebra.LinAlgLmsCompilerScala
import scalan.it.BaseItTests
import scalan.linalgebra.{MatricesDsl, MatricesDslExp, MatricesDslSeq}

trait HelloScalan extends MatricesDsl {
  lazy val run = fun { p: Rep[(Array[Array[Double]], Array[Double])] =>
    val Pair(m, v) = p
    val width = m(0).length
    val matrix: Matrix[Double] = CompoundMatrix(Collection(m.map { r: Arr[Double] => DenseVector(Collection(r))}), width)
    val vector: Vec[Double] = DenseVector(Collection(v))
    (matrix * vector).items.arr
  }
  // example input
  val matrix = Array(Array(1.0, 2.0), Array(3.0, 5.0))
  val vector = Array(2.0, 3.0)
  val input = (matrix, vector)
}

// to run: scalan-lms-backend/test:runMain HelloScalanSeq
object HelloScalanSeq extends MatricesDslSeq with HelloScalan {
  def result = run(input)

  def main(args: Array[String]) = {
    println(result.mkString(","))
  }
}

// to run: scalan-lms-backend/it:runMain HelloScalanExp
object HelloScalanExp {
  // allows use of standard Scala library, commented out to make tests faster
  // override val defaultCompilerConfig = CompilerConfig(Some("2.11.7"), Seq.empty)

  val program = new MatricesDslExp with HelloScalan

  val compiler = new LinAlgLmsCompilerScala(program)
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

class ReadmeExampleItTests extends BaseItTests[HelloScalan](HelloScalanSeq) {
  val defaultCompilers = compilers(HelloScalanExp.compiler)

  test("Examples from README run") {
    HelloScalanExp.result shouldEqual HelloScalanSeq.result
  }
}
