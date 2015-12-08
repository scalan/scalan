package scalan.staged

import scala.collection._
import scalan.ScalanExp
import scalan.common.GraphUtil

trait AstGraphs extends TransformingExp { self: ScalanExp =>

  /**
   * AstNode is created for each symbol of the AstGraph and represents graph linking structure
   */
  abstract class AstNode(val graph: AstGraph) {
    def sym: Exp[_]
    def inputSyms: List[Exp[_]]
    def outSyms: List[Exp[_]]
  }

  case class GraphNode(
    override val graph: AstGraph,
    sym: Exp[_], // this symbol
    definition: Option[Def[_]], // definition
    usages: List[Exp[_]]) extends AstNode(graph) {
    def inputSyms: List[Exp[_]] = definition.toList.flatMap(_.getDeps)
    def outSyms = usages
    def addUsage(usage: Exp[_]) = copy(usages = usage :: this.usages)
  }

  type Schedule = Seq[TableEntry[_]]

  implicit class ScheduleOps(sch: Schedule) {
    def symbols = sch.map(_.sym)
    def getEffectfulSyms: Set[Exp[_]] =
      sch.flatMap(tp => effectSyms(tp.rhs)).toSet
  }

  def getScope(g: PGraph, vars: List[Exp[_]]): Schedule = {
    def loop(currScope: immutable.Set[Exp[_]]): Schedule = {
      val sch = g.scheduleFrom(currScope)
      val currSch = g.getRootsIfEmpty(sch)
      val es = currSch.getEffectfulSyms
      val newEffects = es -- currScope
      if (newEffects.isEmpty)
        currSch
      else
        loop(currScope ++ newEffects)
    }
    loop(vars.toSet)
  }

  trait AstGraph { thisGraph =>
    def boundVars: List[Exp[_]]
    def roots: List[Exp[_]]

    def freeVars: Set[Exp[_]] = {
      val alldeps = schedule.flatMap { tp => tp.rhs.getDeps }.toSet
      val free = alldeps filter { s => !(isLocalDef(s) || boundVars.contains(s)) }
      free
    }

    def getRootsIfEmpty(sch: Schedule) =
      if (sch.isEmpty) {
        val consts = roots.collect { case DefTableEntry(tp) => tp }
        consts  // the case when body is consists of consts
      }
      else sch

    def schedule: Schedule = {
      if (boundVars.isEmpty)
        buildScheduleForResult(roots, _.getDeps)
      else
      if (isIdentity) Nil
      else {
        val g = new PGraph(roots)
        val scope = getScope(g, boundVars)
        scope
      }
    }

    def scheduleFrom(x: immutable.Set[Exp[_]]): Schedule = {
      val locals = GraphUtil.depthFirstSetFrom[Exp[_]](x)(sym => usagesOf(sym).filter(domain.contains))
      schedule.filter(locals contains _.sym)
    }

    lazy val scheduleSyms = schedule.map { _.sym }

    def iterateIfs = schedule.iterator.filter(_.isIfThenElse)

    def isIdentity: Boolean = boundVars == roots
    def isLocalDef(s: Exp[_]): Boolean = scheduleSyms contains s
    def isLocalDef[T](tp: TableEntry[T]): Boolean = isLocalDef(tp.sym)
    def isRoot(s: Exp[_]): Boolean = roots contains s

    lazy val scheduleAll: Schedule =
      schedule.flatMap {
        case tp @ TableEntry(_, subgraph: AstGraph) => subgraph.scheduleAll :+ tp
        case tp => List(tp)
      }

    /**
     * Returns definitions which are not assigned to sub-branches
     */
    def scheduleSingleLevel = schedule.filter(tp => !isAssignedToIfBranch(tp.sym))

    def filterReifyRoots(schedule: Schedule): Schedule = {
      val filtered = schedule.filter(tp => tp.rhs match {
        case Reify(_,_,_) if isRoot(tp.sym) =>
          false
        case _ => true
      })
      filtered
    }

    /**
     * Symbol Usage information for this graph
     * also contains lambda vars with definition = None
     */
    lazy val nodes: Map[Exp[_], GraphNode] = {
      var defMap: Map[Exp[_], GraphNode] = (schedule.map {
        case TableEntry(s, d) => (s, GraphNode(this, s, Some(d), List.empty[Exp[_]]))
      }).toMap

      def addUsage(usedSym: Exp[_], referencingSym: Exp[_]) = {
        val newNode = defMap.getOrElse(usedSym, GraphNode(this, usedSym, None, List.empty)).addUsage(referencingSym)
        defMap += usedSym -> newNode
      }

      for (TableEntry(s, d) <- schedule) {
        val usedSymbols = d.getDeps
        usedSymbols.foreach(us => addUsage(us, s))
      }
      defMap
    }

    lazy val allNodes: Map[Exp[_], GraphNode] = {
      var defMap: Map[Exp[_], GraphNode] = (scheduleAll.map {
        case TableEntry(s, d) => (s, GraphNode(this, s, Some(d), List.empty[Exp[_]]))
      }).toMap

      def addUsage(usedSym: Exp[_], referencingSym: Exp[_]) = {
        val newNode = defMap.getOrElse(usedSym, GraphNode(this, usedSym, None, List.empty)).addUsage(referencingSym)
        defMap += usedSym -> newNode
      }

      for (TableEntry(s, d) <- scheduleAll) {
        val usedSymbols = d.getDeps
        usedSymbols.foreach(us => addUsage(us, s))
      }
      defMap
    }

    lazy val domain: Set[Exp[_]] = scheduleSyms.toSet

    def node(s: Exp[_]): Option[AstNode] = nodes.get(s)

    def globalUsagesOf(s: Exp[_]) = allNodes.get(s) match {
      case Some(node) => node.outSyms
      case None => List()
    }

    def hasManyUsagesGlobal(s: Exp[_]): Boolean = globalUsagesOf(s).lengthCompare(1) > 0

    def usagesOf(s: Exp[_]) = node(s) match {
      case Some(node) => node.outSyms
      case None => List()
    }

    def hasManyUsages(s: Exp[_]): Boolean = usagesOf(s).lengthCompare(1) > 0

    /** Builds a schedule starting from symbol `sym`  which consists only of local definitions.
      *  @param syms   the roots of the schedule, it can be non-local itself
      *  @param deps  dependence relation between a definition and symbols
      *  @return      a `Seq` of local definitions on which `sym` depends or empty if `sym` is itself non-local
      */
    def buildLocalScheduleFrom(syms: Seq[ExpAny], deps: ExpAny => List[ExpAny]): Schedule =
      for {
        s <- syms if isLocalDef(s)
        tp <- buildScheduleForResult(List(s), deps(_).filter(isLocalDef))
      }
      yield tp

    def buildLocalScheduleFrom(sym: ExpAny, deps: ExpAny => List[ExpAny]): Schedule =
      buildLocalScheduleFrom(List(sym), deps)

    def buildLocalScheduleFrom(sym: ExpAny): Schedule = buildLocalScheduleFrom(sym, (_: ExpAny).getDeps)

    def projectionTreeFrom(root: Exp[_]): ProjectionTree = {
      ProjectionTree(root, s => {
        val usages = usagesOf(s).collect { case u @ TupleProjection(i) => (i, u) }
        usages.sortBy(_._1).map(_._2)
      })
    }

    /** Keeps immutable maps describing branching structure of this lambda
      */
    lazy val branches = {
      // traverse the lambda body from the results to the arguments
      // during the loop below, keep track of all the defs that `are used` below the current position in the `schedule`
      val usedSet = mutable.Set.empty[Exp[_]]

      def isUsed(sym: Exp[_]) = usedSet.contains(sym)

      /** Keep the assignments of symbols to the branches of IfThenElse
        if a definition is assigned to IF statement then it will be in either THEN or ELSE branch, according to flag
        */
      val assignments = mutable.Map.empty[Exp[_], BranchPath]
      def isAssigned(sym: Exp[_]) = assignments.contains(sym)

      val ifBranches = mutable.Map.empty[Exp[_], IfBranches]

      // should return definitions that are not in usedSet
      def getLocalUnusedSchedule(s: Exp[_]): Schedule = {
        if (usedSet.contains(s)) Seq()
        else {
          val sch = buildLocalScheduleFrom(s, (_: ExpAny).getDeps.filterNot(usedSet.contains))
          sch
        }
      }

      /** Builds a schedule according to the current usedSet
        * @param syms starting symbols
        * @return sequence of symbols that 1) local 2) in shallow dependence relation 3) not yet marked
        */
      def getLocalUnusedShallowSchedule(syms: Seq[ExpAny]): Schedule = {
        val sch = buildLocalScheduleFrom(syms, (_: ExpAny).getShallowDeps.filterNot(usedSet.contains))
        sch
      }


      def getTransitiveUsageInRevSchedule(revSchedule: List[TableEntry[_]]): mutable.Set[Exp[_]] = {
        val used = mutable.Set.empty[Exp[_]]
        for (TableEntry(s, _) <- revSchedule) {
          if (isUsed(s))
            used += s

          if (used.contains(s))
            used ++= s.getDeps
        }
        used
      }

      // builds branches for the `cte`
      def getIfBranches(ifSym: Exp[_], cte: IfThenElse[_], defsBeforeIfReversed: List[TableEntry[_]]) = {
        val IfThenElse(c, t, e) = cte

        val cs = buildLocalScheduleFrom(c)
        val ts = buildLocalScheduleFrom(t)
        val es = buildLocalScheduleFrom(e)

        val usedAfterIf = getTransitiveUsageInRevSchedule(defsBeforeIfReversed)
        def isUsedBeforeIf(s: Exp[_]) = usedAfterIf.contains(s)

        val cSet = cs.symbols.toSet
        val tSet = ts.symbols.toSet
        val eSet = es.symbols.toSet

        // a symbol can be in a branch if all is true:
        // 1) the branch root depends on it
        // 2) the other branch doesn't depends on it
        // 3) the condition doesn't depend on it
        // 4) it is not marked as used after the If statement (transitively)
        val tbody = ts.filter(tp => !(eSet.contains(tp.sym) || cSet.contains(tp.sym) || isUsedBeforeIf(tp.sym)))
        val ebody = es.filter(tp => !(tSet.contains(tp.sym) || cSet.contains(tp.sym) || isUsedBeforeIf(tp.sym)))

        IfBranches(thisGraph, ifSym, ThunkDef(t, tbody), ThunkDef(e, ebody))
      }

      def assignBranch(sym: Exp[_], ifSym: Exp[_], thenOrElse: Boolean) = {
        assignments(sym) = BranchPath(thisGraph, ifSym, thenOrElse)
      }

      // traverse the lambda body from the results to the arguments
      var reversed = schedule.reverse.toList
      while (reversed.nonEmpty) {
        val TableEntry(s, d) = reversed.head
        if (!isAssigned(s)) {
          // process current definition
          d match {
            case cte@IfThenElse(c, t, e) => {
              val ifSym = s
              val bs = getIfBranches(ifSym, cte, reversed.tail)
              ifBranches(ifSym) = bs

              // assign symbols to this IF
              // put symbol to the current IF
              for (tp <- bs.thenBody.schedule) {
                assignBranch(tp.sym, ifSym, thenOrElse = true)
              }
              for (tp <- bs.elseBody.schedule) {
                assignBranch(tp.sym, ifSym, thenOrElse = false)
              }

              val tUsed = getLocalUnusedShallowSchedule(bs.thenBody.freeVars.toSeq)
              val eUsed = getLocalUnusedShallowSchedule(bs.elseBody.freeVars.toSeq)
              usedSet ++= tUsed.symbols
              usedSet ++= eUsed.symbols
            }
            case _ =>
          }
          val deps = s.getDeps       // for IfThenElse is gets the roots of each branch and condition
          val shallowDeps = getLocalUnusedShallowSchedule(deps)
          usedSet ++= shallowDeps.symbols
        }
        reversed = reversed.tail
      }


      // create resulting immutable structures
      val resAssignments = assignments.toMap
      val resBranches = ifBranches.toMap
      LambdaBranches(resBranches, resAssignments)
    }

    def isAssignedToIfBranch(sym: Exp[_]) = branches.assignments.contains(sym)
  }

  /** When stored in Map, describes for each key the branch of the symbol
    * @param ifSym      symbol of the related IfThenElse definition
    * @param thenOrElse true if the symbol is assigned to then branch, false if to the else branch
    */
  case class BranchPath(graph: AstGraph, ifSym: Exp[_], thenOrElse: Boolean) {
//    def parent: Option[BranchPath] = graph.assignments.get(ifSym)
//    def pathToRoot: Iterator[BranchPath] =
//      Iterator.iterate(Option(this))(p => p.flatMap { _.parent}).takeWhile(_.isDefined).map(_.get)
  }

  /** When stored in a Map, keeps for each IfThenElse schedule of the branches
    * @param graph     the graph this IF branches belong to
    * @param ifSym     symbol of the IfThenElse statement
    * @param thenBody  schedule of `then` branch
    * @param elseBody  schedule of `else` branch
    */
  case class IfBranches(graph: AstGraph, ifSym: Exp[_], thenBody: ThunkDef[_], elseBody: ThunkDef[_])
  {
    // filter out definitions from this branches that were reassigned to the deeper levels
    def cleanBranches(assignments: Map[Exp[_], BranchPath]) = {
      val thenClean = thenBody.schedule.filter(tp => assignments(tp.sym).ifSym == ifSym)
      val elseClean = elseBody.schedule.filter(tp => assignments(tp.sym).ifSym == ifSym)
      IfBranches(graph, ifSym,
        ThunkDef(thenBody.root, thenClean),
        ThunkDef(elseBody.root, elseClean))
    }
    override def toString = {
      val Def(IfThenElse(cond,_,_)) = ifSym
      s"""
         |${ifSym} = if (${cond}) then
         |  ${thenBody.schedule.map(tp => s"${tp.sym} -> ${tp.rhs}").mkString("\n")}
         |else
         |  ${elseBody.schedule.map(tp => s"${tp.sym} -> ${tp.rhs}").mkString("\n")}
       """.stripMargin
    }
  }

  /** Keeps a branching structure of the Lambda
    */
  case class LambdaBranches(ifBranches: Map[Exp[_], IfBranches], assignments: Map[Exp[_], BranchPath])

  def buildScheduleForResult(st: Seq[Exp[_]], neighbours: Exp[_] => Seq[Exp[_]]): Schedule = {
    val startNodes = st.flatMap(e => findDefinition(e).toList)
    val lambdas = startNodes.flatMap(_.lambda).toSet

    def succ(tp: TableEntry[_]): Schedule = {
      val ns = neighbours(tp.sym).filterNot(lambdas.contains)
      ns.flatMap { e =>
        findDefinition(e).toList
      }
    }

    val components = GraphUtil.stronglyConnectedComponents[TableEntry[_]](startNodes)(succ)
    components.flatten
  }
}
