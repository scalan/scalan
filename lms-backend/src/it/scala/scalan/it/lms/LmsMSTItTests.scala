package scalan.it.lms

import scalan.compilation.lms.cxx.sharedptr.CoreCxxShptrLmsBackend
import scalan.{ScalanCommunityDslExp, ScalanCtxSeq}
import scalan.compilation.GraphVizConfig
import scalan.compilation.lms._
import scalan.compilation.lms.cxx.LmsCompilerCxx
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.graphs.{GraphExamples, GraphsDslExp, GraphsDslSeq, MST_example}
import scalan.it.BaseItTests
import scalan.util.FileUtil


abstract class LmsMstItTests extends BaseItTests {

  type Prog = MST_example

  class ProgSeq extends ScalanCtxSeq with MST_example
  class ProgExp extends ScalanCommunityDslExp with MST_example

  val progStaged = new CommunityLmsCompilerScala(new ProgExp) with CommunityBridge

  val progStagedCxx = new LmsCompilerCxx(new ProgExp) with CoreBridge

  val progSeq = new ProgSeq

  def sparseVectorData(arr: Array[Double]) = (0.until(arr.length).toArray, (arr, arr.length))

  // LmsCompilerCxx doesn't produce output yet
  val defaultCompilers = compilers(progStaged/*, progStagedCxx*/)
}

class LmsMstPrimeItTests extends LmsMstItTests {
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
    Array(1.0, 0.5),
    Array(2.0, 0.5)
  )

  test("MST_adjList") {
    val links = graph.flatMap( i=> i)
    val edgeVals = graphValues.flatMap(i => i)
    val lens = graph.map(i => i.length)
    val offs = Array(0,2,5,9,12,14,18,21,24,28,30,32) //(Array(0) :+ lens.scan.slice(lens.length-1)
    val input = (links, (edgeVals, (offs, lens)))
    val res = progSeq.MST_adjlist(input)
    compareOutputWithSequential(_.MST_adjlist, "MST_adjList")(input)
    val dir = FileUtil.file(prefix, "MST_adjList")
    progStagedCxx.buildExecutable(dir,dir,"MST_adjList", progStagedCxx.scalan.MST_adjlist, GraphVizConfig.default)(progStagedCxx.defaultCompilerConfig)
    println(res.mkString(" , "))
  }

  test("MSF_adjList") {
    val links = graph.flatMap( i=> i)
    val edgeVals = graphValues.flatMap(i => i)
    val lens = graph.map(i => i.length)
    val offs = Array(0,2,5,9,12,14,18,21,24,28,30,32) //(Array(0) :+ lens.scan.slice(lens.length-1)
    val input = (links, (edgeVals, (offs, lens)))
    val res = progSeq.MSF_adjlist(input)
    //compareOutputWithSequential(progStaged)(progSeq.MST, progStaged.scalan.MST, "MST_adjList", input)
    val dir = FileUtil.file(prefix, "MSF_adjlist")
    progStagedCxx.buildExecutable(dir,dir,"MSF_adjlist", progStagedCxx.scalan.MSF_adjlist, GraphVizConfig.default)(progStagedCxx.defaultCompilerConfig)
    println(res.mkString(" , "))
  }

  test("MST_adjMatrix") {
    val vertexNum = graph.length
    val incMatrix = (graph zip graphValues).flatMap({ in =>
      val row = in._1
      val vals = in._2
      val zero = scala.Array.fill(vertexNum)(0.0)
      for (i <- 0 to row.length-1) { zero(row(i)) = vals(i) }
      zero
    })
    val input = (incMatrix, vertexNum)
    compareOutputWithSequential(_.MST_adjmatrix, "MST_adjMatrix")(input)
    val dir = FileUtil.file(prefix, "MST_adjMatrix")
    progStagedCxx.buildExecutable(dir,dir,"MST_adjMatrix", progStagedCxx.scalan.MST_adjmatrix, GraphVizConfig.default)(progStagedCxx.defaultCompilerConfig)
  }
  test("MSF_adjMatrix") {
    val vertexNum = graph.length
    val incMatrix = (graph zip graphValues).flatMap({ in =>
      val row = in._1
      val vals = in._2
      val zero = scala.Array.fill(vertexNum)(0.0)
      for (i <- 0 to row.length-1) { zero(row(i)) = vals(i) }
      zero
    })
    val input = (incMatrix, vertexNum)
    val res = progSeq.MSF_adjmatrix(input)
    //compareOutputWithSequential(progStaged)(progSeq.MST, progStaged.scalan.MST, "MST_adjMatrix", input)
    val dir = FileUtil.file(prefix, "MSF_adjmatrix")
    progStagedCxx.buildExecutable(dir,dir,"MSF_adjmatrix", progStagedCxx.scalan.MSF_adjmatrix, GraphVizConfig.default)(progStagedCxx.defaultCompilerConfig)
    println(res.mkString(" , "))
  }

  test("MSF_adjListMap") {
    val links = graph.flatMap( i=> i)
    val edgeVals = graphValues.flatMap(i => i)
    val lens = graph.map(i => i.length)
    val offs = Array(0,2,5,9,12,14,18,21,24,28,30,32) //(Array(0) :+ lens.scan.slice(lens.length-1)
    val input = (links, (edgeVals, (offs, lens)))
    val resSeq = progSeq.MSF_adjlistMap(input)
    println(resSeq.mkString(" , "))
    val resStaged = getStagedOutput(progStaged)((_: ProgExp).MSF_adjlistMap, "MSF_adjlistMap", input)
    println("Staged: " + resStaged.mkString(","))
//    val dir = FileUtil.file(prefix, "MSF_adjlistMap")
//    progStagedCxx.buildExecutable(dir,dir,"MSF_adjlistMap", progStagedCxx.scalan.MSF_adjlistMap, GraphVizConfig.default)(progStagedCxx.defaultCompilerConfig)
  }

  test("MSF_adjMatrixMap") {
    val vertexNum = graph.length
    val incMatrix = (graph zip graphValues).flatMap({ in =>
      val row = in._1
      val vals = in._2
      val zero = scala.Array.fill(vertexNum)(0.0)
      for (i <- 0 to row.length-1) { zero(row(i)) = vals(i) }
      zero
    })
    val input = (incMatrix, vertexNum)
    val resSeq = progSeq.MSF_adjmatrixMap(input)
    println(resSeq.mkString(" , "))
    val resStaged = getStagedOutput(progStaged)((_: ProgExp).MSF_adjmatrixMap, "MSF_adjmatrixMap", input)
    println("Staged: " + resStaged.mkString(","))
  }
  test("MST_adjListMap") {
    val links = graph.flatMap( i=> i)
    val edgeVals = graphValues.flatMap(i => i)
    val lens = graph.map(i => i.length)
    val offs = Array(0,2,5,9,12,14,18,21,24,28,30,32) //(Array(0) :+ lens.scan.slice(lens.length-1)
    val input = (links, (edgeVals, (offs, lens)))
    val resSeq = progSeq.MST_adjlistMap(input)
    println(resSeq.mkString(" , "))
    val resStaged = getStagedOutput(progStaged)((_: ProgExp).MST_adjlistMap, "MST_adjlistMap", input)
    println("Staged: " + resStaged.mkString(","))
//    val dir = FileUtil.file(prefix, "MST_adjlistMap")
//    progStagedCxx.buildExecutable(dir,dir,"MST_adjlistMap", progStagedCxx.scalan.MST_adjlistMap, GraphVizConfig.default)(progStagedCxx.defaultCompilerConfig)
  }
  test("MST_adjMatrixMap") {
    val vertexNum = graph.length
    val incMatrix = (graph zip graphValues).flatMap({ in =>
      val row = in._1
      val vals = in._2
      val zero = scala.Array.fill(vertexNum)(0.0)
      for (i <- 0 to row.length-1) { zero(row(i)) = vals(i) }
      zero
    })
    val input = (incMatrix, vertexNum)
    val resSeq = progSeq.MST_adjmatrixMap(input)
    println(resSeq.mkString(" , "))
    val resStaged = getStagedOutput(progStaged)((_: ProgExp).MST_adjmatrixMap, "MST_adjmatrixMap", input)
    println("Staged: " + resStaged.mkString(","))
  }

  test("MSF_adjListList") {
    val links = graph.flatMap( i=> i)
    val edgeVals = graphValues.flatMap(i => i)
    val lens = graph.map(i => i.length)
    val offs = Array(0,2,5,9,12,14,18,21,24,28,30,32) //(Array(0) :+ lens.scan.slice(lens.length-1)
    val input = (links, (edgeVals, (offs, lens)))
    val resSeq = progSeq.MSF_adjlistList(input)
    println(resSeq.mkString(" , "))
    val resStaged = getStagedOutput(progStaged)((_: ProgExp).MSF_adjlistList, "MSF_adjlistList", input)
    println("Staged: " + resStaged.mkString(","))
    val dir = FileUtil.file(prefix, "MSF_adjlistList")
    progStagedCxx.buildExecutable(dir,dir,"MSF_adjlistList", progStagedCxx.scalan.MSF_adjlistList, GraphVizConfig.default)(progStagedCxx.defaultCompilerConfig)
  }

  test("MSF_adjMatrixList") {
    val vertexNum = graph.length
    val incMatrix = (graph zip graphValues).flatMap({ in =>
      val row = in._1
      val vals = in._2
      val zero = scala.Array.fill(vertexNum)(0.0)
      for (i <- 0 to row.length-1) { zero(row(i)) = vals(i) }
      zero
    })
    val input = (incMatrix, vertexNum)
    val resSeq = progSeq.MSF_adjmatrixList(input)
    println(resSeq.mkString(" , "))
    val resStaged = getStagedOutput(progStaged)((_: ProgExp).MSF_adjmatrixList, "MSF_adjmatrixList", input)
    println("Staged: " + resStaged.mkString(","))
    val dir = FileUtil.file(prefix, "MSF_adjmatrixList")
    progStagedCxx.buildExecutable(dir,dir,"MSF_adjmatrixList", progStagedCxx.scalan.MSF_adjmatrixList, GraphVizConfig.default)(progStagedCxx.defaultCompilerConfig)
  }

  test("MST_adjMatrixList") {
    val vertexNum = graph.length
    val incMatrix = (graph zip graphValues).flatMap({ in =>
      val row = in._1
      val vals = in._2
      val zero = scala.Array.fill(vertexNum)(0.0)
      for (i <- 0 to row.length-1) { zero(row(i)) = vals(i) }
      zero
    })
    val input = (incMatrix, vertexNum)
    val resSeq = progSeq.MST_adjmatrixList(input)
    println(resSeq.mkString(" , "))
    val resStaged = getStagedOutput(progStaged)((_: ProgExp).MST_adjmatrixList, "MST_adjmatrixList", input)
    println("Staged: " + resStaged.mkString(","))
  }

}
