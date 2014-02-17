package scalan.staged

import scalan.ScalanStaged
import scalan.common.GraphUtil

trait ProgramGraphs extends Scheduling with Transforming with AstGraphs { self: ScalanStaged =>

  case class ProgramGraphNode(
      override val graph: AstGraph,
      sym: Exp[Any],                 // this symbol
      definition: Option[Def[Any]],  // definition 
      usages: List[Exp[Any]]) extends AstNode(graph) 
  {
    def inputSyms: List[Exp[Any]] = definition.toList.flatMap(_.getDeps)
    def outSyms = usages
  }

  class PGraph(roots: List[Exp[Any]],
               mapping: MapTransformer = new MapTransformer()) extends ProgramGraph[MapTransformer](roots, mapping) {
    def this(root: Exp[Any]) = this(List(root))
  }

  // immutable program graph
  class ProgramGraph[Ctx <: Transformer : TransformerOps](override val roots: List[Exp[Any]], val mapping: Ctx) 
  	  extends AstGraph(roots) 
  {
    val schedule = buildScheduleForResult(roots /*map { _.asSymbol }*/, _.getDeps)

    def scheduleAll = {
      schedule flatMap (tp  => tp match {
        case TableEntry(s, lam: Lambda[_, _]) => lam.scheduleAll :+ tp
        case _ => List(tp)
      })
    }

    /** Symbol Usage information for this graph
        also contains lambda vars with definition = None
     */
    val nodes: Map[Exp[Any], ProgramGraphNode] = {
      var defMap: Map[Exp[Any], ProgramGraphNode] = (schedule map {
        case TableEntry(s, d) => (s, ProgramGraphNode(this, s, Some(d), List.empty[Exp[Any]]))
      }).toMap

      def addUsage(usedSym: Exp[Any], referencingSym: Exp[Any]) = defMap.get(usedSym) match {
        case Some(node) =>
          defMap += usedSym -> node.copy(usages = referencingSym :: node.usages)
        case None =>
          defMap += usedSym -> ProgramGraphNode(this, usedSym, None, List(referencingSym))
      }

      for (TableEntry(s,d) <- schedule) {
        val usedSymbols = d.getDeps
        usedSymbols.foreach(us => addUsage(us, s))
      }
      defMap
    }

    lazy val domain: Set[Exp[Any]] = (nodes collect {
      case (s, ProgramGraphNode(_, _, Some(_), _)) => s
    }).toSet
    def isDefinedAt(s: Exp[Any]) = domain.contains(s)
    def apply(s: Exp[Any]) = nodes(s)
    def usagesOf(s: Exp[Any]) = nodes.get(s) match {
      case Some(ProgramGraphNode(_,_,_,usages)) => usages
      case None => List()
    }
    def hasManyUsages(s: Exp[Any]): Boolean = usagesOf(s).length > 1

    def scheduleFrom(x: Exp[Any]): List[TableEntry[_]] = {
      val locals = GraphUtil.depthFirstSetFrom(x)(sym => usagesOf(sym).filter(domain contains _))
      schedule filter ( locals contains _.sym )
    }

    def transform(m: Mirror[Ctx], rw: Rewriter, t: Ctx): ProgramGraph[Ctx] = {
      val t0 = t merge mapping
      val (t1, _) = m.mirrorSymbols(t0, rw, schedule map { _.sym })
      val newRoots = roots map { t1(_) }
      new ProgramGraph(newRoots, t1)
    }
  }


  def mkTuple(xs: List[Rep[_]]): Rep[_] = xs.reverse match {
    case x1 :: (tail@(x2 :: _)) => (x1 /: tail) ((s, x) => Pair(x, s))
    case x :: _ => x
    case _ => ()
  }

}
