package scalan.common

trait GraphOperations[Node] extends InductiveGraphs[Node] {
  type Subst = Map[Node, Node]
  type NodeCompare = (Node, Node) => Boolean
  /**
   *  Search subgraph in a Graph considered as DAG along input Edges (as defined by Context)
   *  @param graph Graph where to search
   *  @param graphNode node to start search in the graph
   *  @param prefix subgraph to search starting from its root
   *  @param prefixNode node to start search in the prefix
   *  @note Comparison of graphs is made respecting order of nodes in NodeContext.in sequences
   */
  case class SearchPrefix[A, B](graph: Graph[A, B], graphNode: Node, prefix: Graph[A,B], prefixNode: Node, compare: NodeCompare)
  object FoundPrefix {
    def unapply[A, B](q: SearchPrefix[A, B]): Option[Subst] = {
      var toSubst = Map[Node, Node]()
      var fromSubst = Map[Node, Node]()
      def bind(n1: Node, n2: Node) = {
        toSubst += (n1 -> n2)
        fromSubst += (n1 -> n2)
      }
      def extractPrefixRec[A,B](p: Graph[A,B], pn: Node, g: Graph[A,B], gn: Node): Boolean = {
        (p.search(pn), g.search(gn)) match {
          case (FoundNode(Context(inEdges1, n1, v1, _), g1), FoundNode(Context(inEdges2, n2, v2, _), g2)) =>
            if (q.compare(n1, n2) && (inEdges1.length == inEdges2.length || inEdges1.isEmpty || inEdges2.isEmpty))
            {
              bind(n1, n2)
              for ((Edge(_, in1), Edge(_, in2)) <- inEdges1 zip inEdges2) {
                (toSubst.get(in1), fromSubst.get(in2)) match {
                  case (None, None) => {
                    val res = extractPrefixRec(g1, in1, g2, in2)
                    if (!res) return false
                    //else just continue
                  }
                  case (Some(_), None) | (None, Some(_)) =>
                    // this is not a prefix
                    return false
                  case (Some(_), Some(_)) =>
                  // has already been in this child, just skip it
                }
              }
              true
            }
            else
              false
          case (FoundNode(_, _), gsearch) =>
            if (gsearch.graph.isEmpty) false
            else {
              sys.error(s"cannot complete PrefixSearch $q; caused by search in graph unsuccessful ${gsearch}")
            }

          case (psearch, FoundNode(_, _)) =>
            if (psearch.graph.isEmpty) true
            else {
              sys.error(s"cannot complete PrefixSearch $q; caused by search in prefix unsuccessful ${psearch}")
            }
        }
      }

      val res = extractPrefixRec(q.prefix, q.prefixNode, q.graph, q.graphNode)
      if (res) Some(toSubst) else None
    }

  }

}
