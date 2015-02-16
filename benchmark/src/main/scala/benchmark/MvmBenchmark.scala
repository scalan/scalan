package benchmark

import java.io.File
import java.net.URLClassLoader
import java.util.concurrent.TimeUnit

import benchmark.jni.NativeMethods
import org.openjdk.jmh.annotations._

import scalan.community.{ScalanCommunityExp, ScalanCommunityDslExp, ScalanCommunitySeq}
import scalan.compilation.GraphVizConfig
import scalan.compilation.lms.cxx.{CommunityCXXLmsBackend, LmsCompilerCXX}
import scalan.compilation.lms.scalac.LmsCompilerScala
import scalan.compilation.lms.{CommunityLmsBackend, CommunityBridge}
import scalan.linalgebra.{MatricesDslSeq, LinearAlgebraExamples}
import scalan.util.FileUtil

/**
 * Created by zotov on 2/3/15.
 */
object MvmBenchmark {
  @volatile
  class MvmStateBase {
    // all declared vars to make them volatile
    var height = 10000
    var width = 10000
    var matSparse = 0.9
    var vecSparse = 0.1
    var max = 10

    def genRandVec(len: Int, sp: Double) = {
      val dense = (for (i <- 0 until len) yield   {
        val e = scala.util.Random.nextDouble()
        if (e >= sp) e*max else 0.0
      }).toArray
      //dense
      val (spInd, spVals) = ((0 until len).toArray zip dense).filter( x => x._2 != 0).unzip
      (dense, (spInd.toArray, (spVals.toArray, width)))
    }

    def genRandMat(h: Int, w: Int, sp: Double) = {
      (for (i <- 0 until h) yield genRandVec(w, sp)).toArray
      val (dmat, smat) = (for (i <- 0 until h) yield genRandVec(w, sp)).unzip
      (dmat.toArray, smat.toArray)
    }

    def printSparseVec[T]( sv: (Array[Int], Array[T] ), prefix: String = "") = {
      println(prefix + (sv._1 zip sv._2).mkString(" "))
    }
    def printDenseVec[T](dv: Array[T], prefix: String = "") = {
      println(prefix + dv.mkString(" "))
    }
    def printDenseMat[T](m: Array[Array[T]], prefix: String = "") = {
      println(prefix)
      for (i <- m) printDenseVec(i)
      println("")
    }
    def printSparseMat[T](m: Array[(Array[Int], Array[T])], prefix: String = "") = {
      println(prefix)
      for (i <- m) printSparseVec(i)
      println("")
    }


    var (dvec, svec) = genRandVec(width, vecSparse)
    var (dmat, smat) = genRandMat(height, width, matSparse)
    var fmat = (dmat.flatten, width)

    //printDenseVec(dvec, "Dense Vector:")
    //printSparseVec(svec, "Sparse Vector:")
    //printDenseMat(dmat, "Dense Matrix:")
    //printSparseMat(smat, "Sparse Matrix:")

    //val dmdv_res = (new dmdvm())(dmat, dvec)
    //printDenseVec(dmdv_res, "Dense x Dense: ")

    //val dmsv_res = (new dmsvm())(dmat, svec)
    //printDenseVec(dmsv_res, "Dense x Sparse: ")

    //val smdv_res = (new smdvm())(smat, dvec)
    //printDenseVec(smdv_res, "Sparse x Dense: ")

    //val smsv_res = (new smsvm())(smat, svec)
    //printDenseVec(smsv_res, " Sparse x Sparse: ")
  }

  @State(Scope.Benchmark)
  @volatile
  class MvmState extends MvmStateBase {
    val ctx = new ScalanCommunitySeq with LinearAlgebraExamples with MatricesDslSeq
  }

  @volatile
  abstract class MvmStateStagedAbs extends MvmStateBase {

    class ProgExp extends LinearAlgebraExamples with ScalanCommunityDslExp with LmsCompilerScala with CommunityBridge {
      val lms = new CommunityLmsBackend
    }


    protected val ctx = new ProgExp

    protected val baseDir = FileUtil.file("mvm-staged")

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
  }

  @State(Scope.Benchmark)
  @volatile
  class MvmStateStaged_ddmvm extends MvmStateStagedAbs {
    val ddmvm = loadMethod(ctx)(baseDir, "ddmvm", ctx.ddmvm)
  }

  @State(Scope.Benchmark)
  @volatile
  class MvmStateStaged_dsmvm extends MvmStateStagedAbs {
    val dsmvm = loadMethod(ctx)(baseDir, "dsmvm", ctx.dsmvm)
  }

  @State(Scope.Benchmark)
  @volatile
  class MvmStateStaged_sdmvm extends MvmStateStagedAbs {
    val sdmvm = loadMethod(ctx)(baseDir, "sdmvm", ctx.sdmvm)
  }

  @State(Scope.Benchmark)
  @volatile
  class MvmStateStaged_ssmvm extends MvmStateStagedAbs {
    val ssmvm = loadMethod(ctx)(baseDir, "ssmvm", ctx.ssmvm)
  }

  @State(Scope.Benchmark)
  @volatile
  class MvmStateStaged_fdmvm extends MvmStateStagedAbs {
    val fdmvm = loadMethod(ctx)(baseDir, "fdmvm", ctx.fdmvm)
  }

  @State(Scope.Benchmark)
  @volatile
  class MvmStateStaged_fsmvm extends MvmStateStagedAbs {
    val fsmvm = loadMethod(ctx)(baseDir, "fsmvm", ctx.fsmvm)
  }

  @State(Scope.Benchmark)
  @volatile
  class MvmStateCpp extends MvmStateBase {

//    class ProgExp extends LinearAlgebraExamples with ScalanCommunityDslExp with ScalanCommunityExp with LmsCompilerCXX {
//      self =>
//      def makeBridge[A, B] = new CommunityBridge[A, B] {
//        val scalan = self
//        val lms = new CommunityCXXLmsBackend
//      }
//    }
//
//
//    protected val ctx = new ProgExp
//
//    protected val baseDir = FileUtil.file("mvm-staged-cxx")
//
//    protected implicit val cfg = ctx.defaultCompilerConfig

    val nm = new NativeMethods
    var res: Array[Double] = null;

    @TearDown
    def check(): Unit = {
//      require(res.deep == dvec.deep, "bad!")
    }
  }

}

class MvmBenchmark {
  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def dmdv_seq(state: MvmBenchmark.MvmState): Array[Double] = {
    state.ctx.ddmvm( (state.dmat, state.dvec) )
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def dmdv_cpp(state: MvmBenchmark.MvmStateCpp): Array[Double] = {
    val res = state.nm.ddmvm( (state.dmat, state.dvec) )
    state.res = res
    res
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def dmsv_seq(state: MvmBenchmark.MvmState): Array[Double] = {
    state.ctx.dsmvm( (state.dmat, state.svec) )
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def smdv_seq(state: MvmBenchmark.MvmState): Array[Double] = {
    state.ctx.sdmvm( (state.smat, state.dvec) )
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def smsv_seq(state: MvmBenchmark.MvmState): Array[Double] = {
    state.ctx.ssmvm( (state.smat, state.svec) )
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def fmdv_seq(state: MvmBenchmark.MvmState): Array[Double] = {
    state.ctx.fdmvm( (state.fmat, state.dvec) )
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def fmsv_seq(state: MvmBenchmark.MvmState): Array[Double] = {
    state.ctx.fsmvm( (state.fmat, state.svec) )
  }


  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def dmdv_staged(state: MvmBenchmark.MvmStateStaged_ddmvm): Array[Double] = {
    state.ddmvm( (state.dmat, state.dvec) )
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def dmsv_staged(state: MvmBenchmark.MvmStateStaged_dsmvm): Array[Double] = {
    state.dsmvm( (state.dmat, state.svec) )
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def smdv_staged(state: MvmBenchmark.MvmStateStaged_sdmvm): Array[Double] = {
    state.sdmvm( (state.smat, state.dvec) )
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def smsv_staged(state: MvmBenchmark.MvmStateStaged_ssmvm): Array[Double] = {
    state.ssmvm( (state.smat, state.svec) )
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def fmdv_staged(state: MvmBenchmark.MvmStateStaged_fdmvm): Array[Double] = {
    state.fdmvm( (state.fmat, state.dvec) )
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def fmsv_staged(state: MvmBenchmark.MvmStateStaged_fsmvm): Array[Double] = {
    state.fsmvm( (state.fmat, state.svec) )
  }
}
