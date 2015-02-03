package scalan.benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import scalan.community.ScalanCommunitySeq
import scalan.linalgebra.{MatricesDslSeq, LinearAlgebraExamples}

/**
 * Created by zotov on 2/3/15.
 */
object MvmBenchmark {
  @State(Scope.Benchmark)
  @volatile
  class MvmState {
    // all declared vars to make them volatile
    var height = 1000
    var width = 1000
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

    var ctx = new ScalanCommunitySeq with LinearAlgebraExamples with MatricesDslSeq

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
}

class MvmBenchmark {
  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def seqdmdv(state: MvmBenchmark.MvmState): Array[Double] = {
    state.ctx.ddmvm( (state.dmat, state.dvec) )
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def seqdmsv(state: MvmBenchmark.MvmState): Array[Double] = {
    state.ctx.dsmvm( (state.dmat, state.svec) )
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def seqsmdv(state: MvmBenchmark.MvmState): Array[Double] = {
    state.ctx.sdmvm( (state.smat, state.dvec) )
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def seqsmsv(state: MvmBenchmark.MvmState): Array[Double] = {
    state.ctx.ssmvm( (state.smat, state.svec) )
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def seqfmdv(state: MvmBenchmark.MvmState): Array[Double] = {
    state.ctx.fdmvm( (state.fmat, state.dvec) )
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def seqfmsv(state: MvmBenchmark.MvmState): Array[Double] = {
    state.ctx.fsmvm( (state.fmat, state.svec) )
  }
}
