package benchmark.jni

import java.io.File
import java.nio.file.Paths
import java.util.concurrent.TimeUnit

import benchmark.common.GraphUtilsSSCA
import org.openjdk.jmh.annotations._

import scalan.compilation.GraphVizConfig
import scalan.compilation.lms.CommunityLmsBackend
import scalan.compilation.lms.scalac.LmsCompilerScala
import scalan.graphs.MST_example
import scalan.util.FileUtil
import scalan.{ScalanCommunityDslExp, ScalanCtxSeq}

object JNIMSTBenchmarkSSCA {

  case class InputDataSSCA(inFileName: String) {
    val g = GraphUtilsSSCA(Paths.get(inFileName))
    val input: (Array[Int], (Array[Double], (Array[Int], Array[Int]))) = (g.endV.toArray, (g.weights.toArray, (g.rowInd.dropRight(1).toArray, GraphUtilsSSCA.calcSegLens(g.rowInd.toArray))))
    val inputM: (Array[Double], Int) = (GraphUtilsSSCA.toDenseWeights(g).toArray, g.n)
  }


  @State(Scope.Benchmark)
  trait MST_StateBase {

    @Param(Array("ssca2-3", "ssca2-10"))
    var inFileName: String = _

    var inputData: InputDataSSCA = _
    var input: (Array[Int],(Array[Double], (Array[Int], Array[Int]))) = _
    var inputM: (Array[Double], Int) = _

    @Setup
    def prepare(): Unit = {
      inputData = InputDataSSCA(inFileName)
      input = inputData.input
      inputM = inputData.inputM
    }

    class ProgSeq extends MST_example with ScalanCtxSeq
    protected val progSeq = new ProgSeq

    var res: Array[Int] = Array.empty
    def resSeq: Array[Int]

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

    val nativeMethods = new NativeMethods

    @TearDown
    def check(): Unit = {
//      println(resSeq.mkString(","))
      assert(resSeq.sameElements(res), s"resSeq.sameElements(res)")
    }
  }

  @State(Scope.Benchmark)
  class MST_adjlist_State extends MST_StateBase {
    val MST_adjlist = loadMethod(ctx)(baseDir, "MST_adjlist", ctx.MST_adjlist)
    lazy val resSeq = progSeq.MST_adjlist(input)
  }

  @State(Scope.Benchmark)
  class MST_adjmatrix_State extends MST_StateBase {
    val MST_adjmatrix = loadMethod(ctx)(baseDir, "MST_adjmatrix", ctx.MST_adjmatrix)
    lazy val resSeq = progSeq.MST_adjmatrix(inputM)
  }

  @State(Scope.Benchmark)
  class MSF_adjlist_State extends MST_StateBase {
    val MSF_adjlist = loadMethod(ctx)(baseDir, "MSF_adjlist", ctx.MSF_adjlist)
    lazy val resSeq = progSeq.MSF_adjlist(input)
  }

  @State(Scope.Benchmark)
  class MSF_adjmatrix_State extends MST_StateBase {
    val MSF_adjmatrix = loadMethod(ctx)(baseDir, "MSF_adjmatrix", ctx.MSF_adjmatrix)
    lazy val resSeq = progSeq.MSF_adjmatrix(inputM)
  }


} //object JNIMSTBenchmarkSSCA

class JNIMSTBenchmarkSSCA {
  System.loadLibrary("jniMST")

  import benchmark.jni.JNIMSTBenchmarkSSCA._

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Warmup(iterations = 1)
  @Measurement(iterations = 10)
  def MST_adjlist_cxx( state: MST_adjlist_State ): Array[Int] = {
    val res = state.nativeMethods.MSTadjlist( state.input )
    state.res = res
    res
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Warmup(iterations = 1)
  @Measurement(iterations = 10)
  def MST_adjmatrix_cxx( state: MST_adjmatrix_State ): Array[Int] = {
    val res = state.nativeMethods.MSTadjmatrix( state.inputM )
    state.res = res
    res
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Warmup(iterations = 10)
  @Measurement(iterations = 10)
  def MST_adjlist_staged( state: MSF_adjlist_State ): Array[Int] = {
    val res = state.MSF_adjlist(state.input)
    state.res = res
    res
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Warmup(iterations = 10)
  @Measurement(iterations = 10)
  def MST_adjmatrix_staged( state: MST_adjmatrix_State ): Array[Int] = {
    val res = state.MST_adjmatrix(state.inputM)
    state.res = res
    res
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Warmup(iterations = 1)
  @Measurement(iterations = 10)
  def MSF_adjlist_cxx( state: MSF_adjlist_State ): Array[Int] = {
    val res = state.nativeMethods.MSFadjlist( state.input )
    state.res = res
    res
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Warmup(iterations = 1)
  @Measurement(iterations = 10)
  def MSF_adjmatrix_cxx( state: MSF_adjmatrix_State ): Array[Int] = {
    val res = state.nativeMethods.MSFadjmatrix( state.inputM )
    state.res = res
    res
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Warmup(iterations = 10)
  @Measurement(iterations = 10)
  def MSF_adjlist_staged( state: MSF_adjlist_State ): Array[Int] = {
    val res = state.MSF_adjlist(state.input)
    state.res = res
    res
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @Warmup(iterations = 10)
  @Measurement(iterations = 10)
  def MSF_adjmatrix_staged( state: MSF_adjmatrix_State ): Array[Int] = {
    val res = state.MSF_adjmatrix(state.inputM)
    state.res = res
    res
  }
} //class JNIMSTBenchmarkSSCA
