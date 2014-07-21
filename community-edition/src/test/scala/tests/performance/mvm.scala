package tests.performance

import org.scalameter.api._
import tests.scalan.linalgebra.LinearAlgebraExamples
import scalan.community.ScalanCommunitySeq
import scalan.linalgebra.MatricesDslSeq

class mvm extends BaseBenchmark {
  //def main(args:Array[String]) {
    val height = 10000
    val width = 10000
    val matSparse = 0.9
    val vecSparse = 0.1
    val max = 10

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

    val (dvec, svec) = genRandVec(width, vecSparse)
    val (dmat, smat) = genRandMat(height, width, matSparse)
    val fmat = (dmat.flatten, width)

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
    val inDD = Gen.single("dmdv")((dmat,dvec))
    val inDS = Gen.single("dmsv")((dmat,svec))
    val inSD = Gen.single("smdv")((smat,dvec))
    val inSS = Gen.single("smsv")((smat,svec))
    val inFD = Gen.single("fmdv")((fmat,dvec))
    val inFS = Gen.single("fmsv")((fmat,svec))

    val ctx = new ScalanCommunitySeq with LinearAlgebraExamples with MatricesDslSeq

    performance of "mvm" config (
      exec.minWarmupRuns -> 5,
      exec.maxWarmupRuns -> 20,
      exec.warmupCovThreshold -> 0.1
    ) in {
      measure method "seqdmdv" in {
        using(inDD) in {
          a => ctx.ddmvm((a._1, a._2))
        }
      }
    }
      measure method "seqdmsv" in {
        using(inDS) in {
          a => ctx.dsmvm(a)
        }
      }
      measure method "seqsmdv" in {
        using(inSD) in {
          a => ctx.sdmvm(a)
        }
      }
      measure method "seqsmsv" in {
        using(inSS) in {
          a => ctx.ssmvm(a)
        }
      }
//      measure method "seqfmdv" in {
//        using(inFD) in {
//          a => ctx.fdmvm((a._1, a._2))
//        }
//      }
//      measure method "seqfmsv" in {
//        using(inFS) in {
//          a => ctx.fsmvm(a)
//        }
//      }
//      measure method "dmdv" in {
//        using(inDD) in {
//          a => (new ddmvm())(a._1, a._2)
//        }
//      }
//      measure method "dmsv" in {
//        using(inDS) in {
//          a => (new dsmvm())(a)
//        }
//      }
//      measure method "smdv" in {
//        using(inSD) in {
//          a => (new sdmvm())(a)
//        }
//      }
//      measure method "smsv" in {
//        using(inSS) in {
//          a => (new ssmvm())(a)
//        }
//      }
//      measure method "fmdv" in {
//        using(inFD) in {
//          a => (new fdmvm())(a._1, a._2)
//        }
//      }
//      measure method "fmsv" in {
//        using(inFS) in {
//          a => (new fsmvm())(a)
//        }
//      }
//    }

  //}
}
