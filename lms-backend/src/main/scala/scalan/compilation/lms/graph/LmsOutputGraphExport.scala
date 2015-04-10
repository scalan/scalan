package scalan.compilation.lms.graph

import java.io.{PrintWriter, File}

import scalan.compilation.GraphVizConfig
import scalan.compilation.lms.{BaseCodegen, LmsBackend, LmsBackendFacade}
import scalan.util.{StringUtil, FileUtil}

/**
 * this is GrafWithExport-like class, but for lms-based nodes (and with *FatBlock support)
 */

class LmsOutputGraphExport [BackendCake <: LmsBackend] (backend:BackendCake) extends GraphCodegen[BackendCake](backend){

  /*
  //private val GCG = backend.grapfCodegen
  //import GCG._
  //override val IR: BackendCake = backend
  import IR._


  private var _counter = -1
  def counter = { _counter = _counter + 1; _counter}

  def exportToGraphVis(file: File, config:GraphVizConfig) {
    if (!config.emitGraphs)
      return
    FileUtil.withFile(file) { stream =>

      stream.println( s"""digraph "${file.getName}" {""")

      stream.println("compound=true concentrate=true " + config.orientationString)

      for(root <- grapfCodegen.graphStream.roots)
        exportBlock(root)(stream)

      stream.println("}")

    }
  }


  def exportBlock(block: ControlGraphBlock)(implicit stream: PrintWriter) = {
    block match {
      case block: FuncBlock =>
        stream.println(s"subgraph cluster_${block.name} {")

        for (stm <- block.statements)
          exportNode(stm)

      case block: IfBlock =>
        stream.println(s"subgraph cluster_if${counter} {")
    }
    stream.println("}")

  }

  def exportNode[T](node: Exp[T])(implicit stream: PrintWriter): Unit = {
    node match {
      case x : BlockExp => exportBlock(x.block)
      case x : Sym[_] =>
        stream.println(s"x${x.id}")
      case x =>
        stream.println(s"exp${counter}")
        //stream.println(StringUtil.quote(x.toString))
    }
  }
*/
}
