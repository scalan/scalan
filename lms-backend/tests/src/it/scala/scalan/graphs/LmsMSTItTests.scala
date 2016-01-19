package scalan.graphs

import scalan.compilation.lms.cxx.LmsCompilerCxx
import scalan.compilation.lms.scalac.LmsCompilerScala
import scalan.it.BaseItTests
import scalan.{ScalanDslExp, ScalanDslStd}

abstract class LmsMstItTests extends BaseItTests[MST_example](new ScalanDslStd with MST_example) {

  class ProgExp extends ScalanDslExp with MST_example

  val progStaged = new LmsCompilerScala(new ProgExp)

  val progStagedCxx = new LmsCompilerCxx(new ProgExp)

  def sparseVectorData(arr: Array[Double]) = (arr.indices.toArray, (arr, arr.length))

  // LmsCompilerCxx doesn't produce output yet
  val defaultCompilers = compilers(progStaged/*, progStagedCxx*/)

  val progStagedCxxOnly = compilers(progStagedCxx)
}

class LmsMstPrimeItTests extends LmsMstItTests with GraphTestInputs {

  test("MST_adjList") {
    val res = progSeq.MST_adjlist(listInput)
    println(res.mkString(", "))
    compareOutputWithSequential(_.MST_adjlist)(listInput)
    compileSource(_.MST_adjlist, progStagedCxxOnly)
  }

  test("MSF_adjList") {
    val res = progSeq.MSF_adjlist(listInput)
    println(res.mkString(", "))
    //compareOutputWithSequential(progStaged)(progSeq.MST, progStaged.scalan.MST, "MST_adjList", input)
    compileSource(_.MSF_adjlist, progStagedCxxOnly)
  }

  test("MST_adjMatrix") {
    compareOutputWithSequential(_.MST_adjmatrix)(matrixInput)
    compileSource(_.MST_adjmatrix, progStagedCxxOnly)
  }
  test("MSF_adjMatrix") {
    val res = progSeq.MSF_adjmatrix(matrixInput)
    println(res.mkString(", "))
    //compareOutputWithSequential(progStaged)(progSeq.MST, progStaged.scalan.MST, "MST_adjMatrix", input)
    compileSource(_.MSF_adjmatrix, progStagedCxxOnly)
  }

  test("MSF_adjListMap") {
    val resSeq = progSeq.MSF_adjlistMap(listInput)
    println(resSeq.mkString(", "))
    val Seq(Seq(resStaged)) = getStagedOutput(_.MSF_adjlistMap)(listInput)
    println("Staged: " + resStaged.mkString(", "))
    // compileSource(_.MSF_adjlistMap, progStagedCxxOnly)
  }

  test("MSF_adjMatrixMap") {
    val resSeq = progSeq.MSF_adjmatrixMap(matrixInput)
    println(resSeq.mkString(", "))
    val Seq(Seq(resStaged)) = getStagedOutput(_.MSF_adjmatrixMap)(matrixInput)
    println("Staged: " + resStaged.mkString(", "))
  }
  test("MST_adjListMap") {
    val resSeq = progSeq.MST_adjlistMap(listInput)
    println(resSeq.mkString(", "))
    val Seq(Seq(resStaged)) = getStagedOutput(_.MST_adjlistMap)(listInput)
    println("Staged: " + resStaged.mkString(", "))
//    compileSource(_.MST_adjlistMap, progStagedCxxOnly)
  }
  test("MST_adjMatrixMap") {
    val resSeq = progSeq.MST_adjmatrixMap(matrixInput)
    println(resSeq.mkString(", "))
    val Seq(Seq(resStaged)) = getStagedOutput(_.MST_adjmatrixMap)(matrixInput)
    println("Staged: " + resStaged.mkString(", "))
  }

  test("MSF_adjListList") {
    val resSeq = progSeq.MSF_adjlistList(listInput)
    println(resSeq.mkString(", "))
    val Seq(Seq(resStaged)) = getStagedOutput(_.MSF_adjlistList)(listInput)
    println("Staged: " + resStaged.mkString(", "))
    compileSource(_.MST_adjlistList, progStagedCxxOnly)
  }

  test("MSF_adjMatrixList") {
    val resSeq = progSeq.MSF_adjmatrixList(matrixInput)
    println(resSeq.mkString(", "))
    val Seq(Seq(resStaged)) = getStagedOutput(_.MSF_adjmatrixList)(matrixInput)
    println("Staged: " + resStaged.mkString(", "))
    compileSource(_.MST_adjmatrixList, progStagedCxxOnly)
  }

  test("MST_adjMatrixList") {
    val resSeq = progSeq.MST_adjmatrixList(matrixInput)
    println(resSeq.mkString(", "))
    val Seq(Seq(resStaged)) = getStagedOutput(_.MST_adjmatrixList)(matrixInput)
    println("Staged: " + resStaged.mkString(", "))
  }

}
