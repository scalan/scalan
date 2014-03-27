package scalan.common

trait GraphOperations[Node] extends InductiveGraphs[Node] {
  type Subst = Map[Node, Node]
  type NodeCompare = (Node, Node) => Boolean
  type LeafPredicate = Node => Boolean
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
        fromSubst += (n2 -> n1)
      }
      def extractPrefixRec(p: Graph[A,B], pn: Node, g: Graph[A,B], gn: Node): Boolean = {
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
  trait SimilarityResult
  case object SimilarityEqual extends SimilarityResult
  case object SimilarityEmbeded extends SimilarityResult
  case object SimilarityFailed extends SimilarityResult

  type ContextCompare[A,B] = (NodeContext[A,B], NodeContext[A,B]) => SimilarityResult

  case class BisimulatorState[A, B](
       leftGraph: Graph[A, B],
       leftStack: List[Node],
       rightGraph: Graph[A,B],
       rightStack: List[Node],
       toSubst: Subst,
       fromSubst: Subst,
       kind: SimilarityResult) {
    def isFailed = kind == SimilarityFailed || leftStack.size != rightStack.size
  }

  def defaultContextCompare[A,B](compare: NodeCompare, isLeaf: LeafPredicate)(lctx: NodeContext[A,B], rctx: NodeContext[A,B]): SimilarityResult = {
    val ln = lctx.node
    val rn = rctx.node
    val leftIns = lctx.in
    val rightIns = rctx.in
    if (compare(ln, rn))
      SimilarityEqual
    else if (isLeaf(ln) || isLeaf(rn))
      SimilarityEmbeded
    else
      SimilarityFailed
  }

  class Bisimulator[A,B](lg: Graph[A,B], rg: Graph[A,B], compare: ContextCompare[A,B]) {

    def genStates(leftStart: List[Node], rightStart: List[Node]) = {
      val initialState = BisimulatorState(lg, leftStart, rg, rightStart, Map(), Map(), SimilarityEqual)

      new Iterator[BisimulatorState[A,B]] {
        private var currState = initialState
        private def isBisimilar(ln: Node, rn: Node) = currState.toSubst.get(ln).exists(_ == rn)

        def hasNext = !currState.leftStack.isEmpty && !currState.rightStack.isEmpty && !currState.isFailed

        def next() = {
          val current = currState; import current._
          val lnode :: ls = leftStack
          val rnode :: rs = rightStack
          if (isBisimilar(lnode, rnode))
            currState = currState.copy(leftStack = ls, rightStack = rs)
          else
            (leftGraph.search(lnode), rightGraph.search(rnode)) match {
              case (FoundNode(lc@Context(leftIns, _, _, _), leftRest), FoundNode(rc@Context(rightIns, _, _, _), rightRest)) =>
                compare(lc, rc) match {
                  case SimilarityEqual =>
                    currState = BisimulatorState(
                      leftRest, leftIns.nodesList ::: ls,
                      rightRest, rightIns.nodesList ::: rs,
                      toSubst + (lnode -> rnode), fromSubst + (rnode -> lnode),
                      SimilarityEqual
                    )
                  case SimilarityEmbeded =>
                    currState = BisimulatorState(
                      leftRest, ls,
                      rightRest, rs,
                      toSubst + (lnode -> rnode), fromSubst + (rnode -> lnode),
                      SimilarityEmbeded
                    )
                  case SimilarityFailed =>
                    currState = currState.copy(kind = SimilarityFailed)
                }
              case (_,_) =>
                currState = currState.copy(kind = SimilarityFailed)
            }
          currState
        }
      }
    }
  }
}
