package benchmark
package jni

import java.io.File
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import scalan.ScalanCommunityDslExp
import scalan.compilation.GraphVizConfig
import scalan.compilation.lms.CommunityLmsBackend
import scalan.compilation.lms.scalac.LmsCompilerScala
import scalan.graphs.MST_example
import scalan.util.FileUtil

object JNITestBenchmark {
  @State(Scope.Benchmark)
  @volatile
  class ExtractAndPackState {
//    val in: NativeMethods#TT = ( Array((Array(1,2,3),(Array(11.0,22.0,3.0),3))
//      ,(Array(1,2,3),(Array(1.0,2.0,3.0),3))
//      ,(Array(1,2,3),(Array(1.0,2.0,3.0),3)))
//      , Array(1.0,222.0,3.0))
    val in: NativeMethods#TT = Array(1,2)
    var nativeMethods = new NativeMethods
    var res: NativeMethods#TT = null;

    @TearDown
    def check(): Unit = {
//      require(res == in, s"error!")
      require(res.deep == in.deep, s"error!")
    }
  }
}

class JNITestBenchmark {
  System.loadLibrary("jniTest")

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def extractAndPack( state: JNITestBenchmark.ExtractAndPackState ): NativeMethods#TT = {
    val res = state.nativeMethods.extractAndPack( state.in )
    state.res = res
    res
  }
}

@State(Scope.Benchmark)
class JNIMSTBenchmark {
  System.loadLibrary("jniMST")

  val graph = Array(
    Array(1, 8),
    Array(0, 2, 8),
    Array(1, 3, 5, 7),
    Array(2, 4, 5),
    Array(3, 5),
    Array(2, 3, 4, 6),
    Array(5, 7, 8),
    Array(2, 6, 8),
    Array(0, 1, 6, 7),
    Array(10,11),
    Array(9,11),
    Array(9,10)
  )
  val graphValues = Array(
    Array(4.0, 8.0),
    Array(4.0, 8.0, 11.0),
    Array(8.0, 7.0, 4.0, 2.0),
    Array(7.0, 9.0, 14.0),
    Array(9.0, 10.0),
    Array(4.0, 14.0, 10.0, 2.0),
    Array(2.0, 6.0, 1.0),
    Array(2.0, 6.0, 7.0),
    Array(8.0, 11.0, 1.0, 7.0),
    Array(1.0, 2.0),
    Array(1.0, 3.0),
    Array(2.0, 3.0)
  )
  val links = graph.flatMap( i=> i)
  val edgeVals = graphValues.flatMap(i => i)
  val lens = graph.map(i => i.length)
  val offs = Array(0,2,5,9,12,14,18,21,24,28,30,32) //(Array(0) :+ lens.scan.slice(lens.length-1)
  val input = (links, (edgeVals, (offs, lens)))
  val right = Array(-1 , 0 , 1 , 2 , 3 , 2 , 5 , 2 , 6 , -2 , -2 , -2) //epends on the order of operations and algorithm behaviour

  val vertexNum = graph.length
  val incMatrix = (graph zip graphValues).flatMap({ in =>
    val row = in._1
    val vals = in._2
    val zero = scala.Array.fill(vertexNum)(0.0)
    for (i <- 0 to row.length-1) { zero(row(i)) = vals(i) }
    zero
  })
  val inputM = (incMatrix, vertexNum)

  val nativeMethods = new NativeMethods

  var res: Array[Int] = null

  class ProgExp extends MST_example with ScalanCommunityDslExp with LmsCompilerScala { self =>
    val lms = new CommunityLmsBackend
  }

  protected val ctx = new ProgExp
  protected val baseDir = FileUtil.file("MST-staged")
  protected implicit val cfg = ctx.defaultCompilerConfig.copy(scalaVersion = Some("2.10.2"))

  protected def loadMethod[A, B](prog: ProgExp)(baseDir: File, functionName: String, f: prog.Exp[A => B] )
                                (implicit compilerConfig: prog.CompilerConfig) =
  {
    val funcDir = FileUtil.file(baseDir, functionName)

    val compilerOutput = prog.buildExecutable(funcDir, functionName, f, GraphVizConfig.none)
    val (cls, method) = prog.loadMethod(compilerOutput)
    val instance = cls.newInstance()
    (method.invoke(instance, _: AnyRef)).asInstanceOf[A => B]
  }

  val MST_adjlist_method = loadMethod(ctx)(baseDir, "MST_adjlist", ctx.MST_adjlist)
  val MST_adjmatrix_method = loadMethod(ctx)(baseDir, "MST_adjmatrix", ctx.MST_adjmatrix)
  val MSF_adjlist_method = loadMethod(ctx)(baseDir, "MSF_adjlist", ctx.MSF_adjlist)
  val MSF_adjmatrix_method = loadMethod(ctx)(baseDir, "MSF_adjmatrix", ctx.MSF_adjmatrix)

//  @TearDown
//  def check(): Unit = {
//    //      require(res == in, s"error!")
//    require(res.deep == right.deep, s"res.deep == right.deep")
//  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def MST_adjlist_cxx( state: JNIMSTBenchmark ): Array[Int] = {
    val res = state.nativeMethods.MSTadjlist( state.input )
    state.res = res
    res
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def MST_adjmatrix_cxx( state: JNIMSTBenchmark ): Array[Int] = {
    val res = state.nativeMethods.MSTadjmatrix( state.inputM )
    state.res = res
    res
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def MST_adjlist_staged( state: JNIMSTBenchmark ): Array[Int] = {
    val res = state.MST_adjlist_method(state.input)
    state.res = res
    res
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def MST_adjmatrix_staged( state: JNIMSTBenchmark ): Array[Int] = {
    val res = state.MST_adjmatrix_method(state.inputM)
    state.res = res
    res
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def MSF_adjlist_cxx( state: JNIMSTBenchmark ): Array[Int] = {
    val res = state.nativeMethods.MSFadjlist( state.input )
    state.res = res
    res
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def MSF_adjmatrix_cxx( state: JNIMSTBenchmark ): Array[Int] = {
    val res = state.nativeMethods.MSFadjmatrix( state.inputM )
    state.res = res
    res
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def MSF_adjlist_staged( state: JNIMSTBenchmark ): Array[Int] = {
    val res = state.MSF_adjlist_method(state.input)
    state.res = res
    res
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def MSF_adjmatrix_staged( state: JNIMSTBenchmark ): Array[Int] = {
    val res = state.MSF_adjmatrix_method(state.inputM)
    state.res = res
    res
  }
}
