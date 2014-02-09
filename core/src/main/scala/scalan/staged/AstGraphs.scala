package scalan.staged

import scalan.Base
import scalan.ScalanStaged

trait AstGraphs extends Transforming { self: ScalanStaged => 
  
  /**
   * AstNode is created for each symbol of the AstGraph and represents graph linking structure
   */
  abstract class AstNode(val graph: AstGraph) 
  {
    def sym: Exp[Any]
    def inputSyms: List[Exp[Any]]
    def outSyms: List[Exp[Any]]
  }

  abstract class AstGraph(val roots: List[Exp[Any]])
      extends PartialFunction[Exp[Any], AstNode] {
    
  }
  
  implicit class AstGraphOps(graph: AstGraph) {
    def startsWith(other: AstGraph): Boolean = {
      true
    }
  }

  
}

