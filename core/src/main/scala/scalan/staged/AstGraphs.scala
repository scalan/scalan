package scalan.staged

import scalan.Base
import scalan.ScalanStaged
import scalan.common.GraphUtil

trait AstGraphs extends Transforming { self: ScalanStaged => 
  
  /**
   * AstNode is created for each symbol of the AstGraph and represents graph linking structure
   */
  abstract class AstNode(val graph: AstGraph) 
  {
    def sym: Exp[_]
    def inputSyms: List[Exp[_]]
    def outSyms: List[Exp[_]]
  }
  case class GraphNode(
        override val graph: AstGraph,
        sym: Exp[_],                 // this symbol
        definition: Option[Def[_]],  // definition
        usages: List[Exp[_]]) extends AstNode(graph)
  {
    def inputSyms: List[Exp[_]] = definition.toList.flatMap(_.getDeps)
    def outSyms = usages
    def addUsage(usage: Exp[_]) = copy(usages = usage :: this.usages)
  }

  trait AstGraph {
    def roots: List[Exp[_]]
    lazy val schedule = buildScheduleForResult(roots, _.getDeps)
    lazy val scheduleSyms = schedule map { _.sym }

    lazy val scheduleFromProjections = buildScheduleForResult(roots, s => {
      if (isLambdaBoundProjection(s)) Nil
      else
        s.getDeps
    })

    lazy val scheduleAll: Seq[TableEntry[_]] = {
      schedule flatMap (tp  => tp match {
        case TableEntry(s, lam: Lambda[_, _]) => lam.bodyScheduleAll :+ tp
        case _ => List(tp)
      })
    }

    lazy val lambdaBoundSyms: Set[Exp[_]] = {
      schedule.foldLeft(Set.empty[Exp[_]]) { (acc, tp) =>
        val deps = nodes(tp.sym).inputSyms
        val bound = deps.filter(_.isVar)
        acc ++ bound
      }
    }

    /** Symbol Usage information for this graph
        also contains lambda vars with definition = None
      */
    lazy val nodes: Map[Exp[_], GraphNode] = {
      var defMap: Map[Exp[_], GraphNode] = (schedule map {
        case TableEntry(s, d) => (s, GraphNode(this, s, Some(d), List.empty[Exp[_]]))
      }).toMap

      def addUsage(usedSym: Exp[_], referencingSym: Exp[_]) = defMap.get(usedSym) match {
        case Some(node) =>
          defMap += usedSym -> node.copy(usages = referencingSym :: node.usages)
        case None =>
          defMap += usedSym -> GraphNode(this, usedSym, None, List(referencingSym))
      }

      for (TableEntry(s,d) <- schedule) {
        val usedSymbols = d.getDeps
        usedSymbols.foreach(us => addUsage(us, s))
      }
      defMap
    }

    lazy val domain: Set[Exp[_]] = (nodes collect {
      case (s, GraphNode(_, _, Some(_), _)) => s
    }).toSet

    def node(s: Exp[_]): Option[AstNode] = nodes.get(s)

    def usagesOf(s: Exp[_]) = nodes.get(s) match {
      case Some(GraphNode(_,_,_,usages)) => usages
      case None => List()
    }

    def hasManyUsages(s: Exp[_]): Boolean = usagesOf(s).lengthCompare(1) > 0

    def scheduleFrom(x: Exp[_]): Seq[TableEntry[_]] = {
      val locals = GraphUtil.depthFirstSetFrom[Exp[_]](x)(sym => usagesOf(sym).filter(domain.contains))
      schedule.filter(locals contains _.sym)
    }

    def projectionTreeFrom(root: Exp[_]): ProjectionTree = {
      ProjectionTree(root, s => {
        val usages = usagesOf(s).collect { case u@TupleProjection(i) => (i,u) }
        usages.sortBy(_._1).map(_._2)
      })
    }

    lazy val lambdaBoundProjections: Set[ExpAny] = {
      val leaves = for {
            v <- lambdaBoundSyms
            t = projectionTreeFrom(v)
            (p, s) <- t.paths
          } yield s
      leaves
    }

    def isLambdaBoundProjection(s: ExpAny): Boolean = lambdaBoundProjections contains s
  }
  
  implicit class AstGraphOps(graph: AstGraph) {
    def startsWith(other: AstGraph): Boolean = {
      true
    }
  }

  
}

