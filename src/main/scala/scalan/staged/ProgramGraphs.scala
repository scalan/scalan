package scalan.staged

import scalan.ScalanStaged

trait ProgramGraphs extends Scheduling with Transforming { self: ScalanStaged =>

  case class Node(usages: List[Exp[Any]], definition: Option[Def[Any]])

  class PGraph(roots: List[Exp[Any]],
               mapping: Transformer = new MapTransformer()) extends ProgramGraph[Transformer](roots, mapping) {
    def this(root: Exp[Any]) = this(List(root))
  }

  // immutable program graph
  class ProgramGraph[Ctx <: Transformer](val roots: List[Exp[Any]], val mapping: Ctx) extends PartialFunction[Exp[Any], Node]
  {
    val schedule = buildScheduleForResult(roots /*map { _.asSymbol }*/, _.getDeps)

    def scheduleAll = {
      schedule flatMap (tp  => tp match {
        case TP(s, Lambda(lam,_,_,_)) => lam.scheduleAll :+ tp
        case _ => List(tp)
      })
    }

    /** Symbol Usage information for this graph
        also contains lambda vars with definition = None
     */
    val nodes: Map[Exp[Any], Node] = {
      var defMap: Map[Exp[Any], Node] = (schedule map {
        case TP(s, d) => (s, Node(List.empty[Exp[Any]], Some(d)))
      }).toMap

      def addUsage(usedSym: Exp[Any], referencingSym: Exp[Any]) = defMap.get(usedSym) match {
        case Some(node) =>
          defMap += usedSym -> node.copy(usages = referencingSym :: node.usages)
        case None =>
          defMap += usedSym -> Node(List(referencingSym), None)
      }

      for (TP(s,d) <- schedule) {
        val usedSymbols = d.getDeps
        usedSymbols.foreach(us => addUsage(us, s))
      }
      defMap
    }

    lazy val domain: Set[Exp[Any]] = (nodes collect {
      case (s, Node(_, Some(_))) => s
    }).toSet
    def isDefinedAt(s: Exp[Any]) = domain.contains(s)
    def apply(s: Exp[Any]) = nodes(s)
    def usagesOf(s: Exp[Any]) = nodes.get(s) match {
      case Some(Node(usages, _)) => usages
      case None => List()
    }
    def hasManyUsages(s: Exp[Any]): Boolean = usagesOf(s).length > 1

    def scheduleFrom(x: Exp[Any]): List[TP[_]] = {
      val locals = depthFirstSetFrom(x)(sym => usagesOf(sym).filter(domain contains _))
      schedule filter ( locals contains _.sym )
    }


    def transform(m: Mirror[Ctx], rw: Rewriter, t: Ctx): ProgramGraph[Ctx] = {
      val t0 = t merge (mapping.asInstanceOf[t.Self])
      val (t1, _) = m.mirrorSymbols(t0, rw, schedule map { _.sym })
      val newRoots = roots map { t1(_) }
      new ProgramGraph(newRoots, t1)
    }
  }

  def depthFirstSetFrom[A](start: A)(neighbours: A =>List[A]): Set[A] = {
    var visited = Set[A]()

    def visit(s: A): Unit = {
      (!(visited contains s)) match {
        case true =>
          visited += s
          neighbours(s) foreach visit
        case _ =>
      }
    }
    visit(start)
    visited
  }

  def mkTuple(xs: List[Rep[_]]): Rep[_] = xs.reverse match {
    case x1 :: (tail@(x2 :: _)) => (x1 /: tail) ((s, x) => Pair(x, s))
    case x :: _ => x
    case _ => ()
  }

}
