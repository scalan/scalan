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
    def sym: Exp[Any]
    def inputSyms: List[Exp[Any]]
    def outSyms: List[Exp[Any]]
  }
  case class GraphNode(
        override val graph: AstGraph,
        sym: Exp[Any],                 // this symbol
        definition: Option[Def[Any]],  // definition
        usages: List[Exp[Any]]) extends AstNode(graph)
  {
    def inputSyms: List[Exp[Any]] = definition.toList.flatMap(_.getDeps)
    def outSyms = usages
  }

  trait AstGraph extends PartialFunction[Exp[Any], AstNode] {
    def roots: List[Exp[Any]]
    lazy val schedule = buildScheduleForResult(roots, _.getDeps)
    lazy val scheduleSyms = schedule map { _.sym }

    lazy val scheduleFromProjections = buildScheduleForResult(roots, s => {
      if (isLambdaBoundProjection(s)) Nil
      else
        s.getDeps
    })

    def scheduleAll: List[TableEntry[_]] = {
      schedule flatMap (tp  => tp match {
        case TableEntry(s, lam: Lambda[_, _]) => lam.scheduleAll :+ tp
        case _ => List(tp)
      })
    }

    lazy val lambdaBoundSyms: Set[AnyExp] = {
      schedule.foldLeft(Set[AnyExp]()) { (acc, tp) =>
        val deps = nodes(tp.sym).inputSyms
        val bound = deps.filter(_.isVar)
        acc ++ bound
      }
    }

    /** Symbol Usage information for this graph
        also contains lambda vars with definition = None
      */
    lazy val nodes: Map[Exp[Any], GraphNode] = {
      var defMap: Map[Exp[Any], GraphNode] = (schedule map {
        case TableEntry(s, d) => (s, GraphNode(this, s, Some(d), List.empty[Exp[Any]]))
      }).toMap

      def addUsage(usedSym: Exp[Any], referencingSym: Exp[Any]) = defMap.get(usedSym) match {
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

    lazy val domain: Set[Exp[Any]] = (nodes collect {
      case (s, GraphNode(_, _, Some(_), _)) => s
    }).toSet

    def isDefinedAt(s: AnyExp) = domain.contains(s)

    def apply(s: AnyExp) = nodes(s)

    def usagesOf(s: AnyExp) = nodes.get(s) match {
      case Some(GraphNode(_,_,_,usages)) => usages
      case None => List()
    }

    def hasManyUsages(s: AnyExp): Boolean = usagesOf(s).length > 1

    def scheduleFrom(x: AnyExp): List[TableEntry[_]] = {
      val locals = GraphUtil.depthFirstSetFrom(x)(sym => usagesOf(sym).filter(domain contains _))
      schedule filter ( locals contains _.sym )
    }

    def projectionTreeFrom(root: AnyExp): ProjectionTree = {
      ProjectionTree(root, s => {
        val usages = usagesOf(s).collect { case u@TupleProjection(i) => (i,u) }
        usages.sortBy(_._1).map(_._2)
      })
    }

    lazy val lambdaBoundProjections: Set[AnyExp] = {
      val leaves = for {
            v <- lambdaBoundSyms
            t = projectionTreeFrom(v)
            (p, s) <- t.paths
          } yield s
      leaves
    }

    def isLambdaBoundProjection(s: AnyExp): Boolean = lambdaBoundProjections contains s
  }
  
  implicit class AstGraphOps(graph: AstGraph) {
    def startsWith(other: AstGraph): Boolean = {
      true
    }
  }

  
}

