package scalan.staged

import scalan.Base
import scalan.ScalanStaged
import scalan.common.GraphUtil

trait AstGraphs extends Transforming { self: ScalanStaged =>

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

  trait AstGraph { thisGraph =>
    def boundVars: List[Exp[_]]
    def roots: List[Exp[_]]

    def freeVars: Set[Exp[_]] = {
      val alldeps = schedule flatMap { tp => tp.rhs.getDeps }
      val external = alldeps filter { s => !(isLocalDef(s) || boundVars.contains(s))  }
      external.toSet
    }

    def schedule: Seq[TableEntry[_]] = {
      if (boundVars.isEmpty)
        buildScheduleForResult(roots, _.getDeps)
      else
      if (isIdentity) Nil
      else {
        val g = new PGraph(roots)
        val sh = g.scheduleFrom(boundVars)
        if (sh.isEmpty) {
          val consts = roots.collect { case DefTableEntry(tp) => tp }
          consts  // the case when body is consists of consts
        }
        else
          sh
      }
    }

    def scheduleFrom(x: List[Exp[_]]): Seq[TableEntry[_]] = {
      val locals = GraphUtil.depthFirstSetFrom[Exp[_]](x)(sym => usagesOf(sym).filter(domain.contains))
      schedule.filter(locals contains _.sym)
    }

    lazy val scheduleSyms = schedule.map { _.sym }

    def iterateIfs = schedule.iterator.filter(_.isIfThenElse)

    def isIdentity: Boolean = boundVars == roots
    def isLocalDef(s: Exp[_]): Boolean = scheduleSyms contains s
    def isLocalDef[T](tp: TableEntry[T]): Boolean = isLocalDef(tp.sym)

    lazy val scheduleFromProjections =
      buildScheduleForResult(roots, s => if (isLambdaBoundProjection(s)) Nil else s.getDeps)

    lazy val scheduleAll: Seq[TableEntry[_]] = {
      schedule.flatMap(tp => tp match {
        case TableEntry(s, lam: Lambda[_, _]) => lam.scheduleAll :+ tp
        case _ => List(tp)
      })
    }

    /**
     * Returns definitions which are not assigned to sub-branches
     */
    def scheduleSingleLevel = schedule.filter(tp => !isAssignedToIfBranch(tp.sym))

    lazy val lambdaBoundSyms: Set[Exp[_]] = {
      schedule.foldLeft(Set.empty[Exp[_]]) { (acc, tp) =>
        val deps = nodes(tp.sym).inputSyms
        val bound = deps.filter(_.isVar)
        acc ++ bound
      }
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

    lazy val domain: Set[Exp[_]] = scheduleSyms.toSet

    def node(s: Exp[_]): Option[AstNode] = nodes.get(s)

    def usagesOf(s: Exp[_]) = node(s) match {
      case Some(node) => node.outSyms
      case None => List()
    }

    def hasManyUsages(s: Exp[_]): Boolean = usagesOf(s).lengthCompare(1) > 0

    /** Builds a schedule starting from symbol `sym`  which consists only of local definitions.
      *  @param sym   the root of the schedule, it can be non-local itself
      *  @param deps  dependence relation between a definition and symbols
      *  @return      a `Seq` of local definitions on which `sym` depends or empty if `sym` is itself non-local
      */
    def buildLocalScheduleFrom(sym: ExpAny, deps: ExpAny => List[ExpAny]): Seq[TableEntry[_]] =
      if (isLocalDef(sym))
        buildScheduleForResult(List(sym), deps(_).filter(isLocalDef))
      else
        Seq.empty

    def buildLocalScheduleFrom(sym: ExpAny): Seq[TableEntry[_]] = buildLocalScheduleFrom(sym, _.getDeps)

    def projectionTreeFrom(root: Exp[_]): ProjectionTree = {
      ProjectionTree(root, s => {
        val usages = usagesOf(s).collect { case u @ TupleProjection(i) => (i, u) }
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

//    def filterBranch(resAssignments: Map[Exp[_], BranchPath], ifSym: Exp[_], thenOrElse: Boolean): Seq[TableEntry[_]] = {
//      schedule.filter(tp => {
//        val pathOpt = resAssignments.get(tp.sym)
//        pathOpt.exists(p => p.ifSym == ifSym && thenOrElse == p.thenOrElse)
//      })
//    }

    /** Keeps immutable maps describing branching structure of this lambda
      */
    lazy val branches = {
      // traverse the lambda body from the results to the arguments
      // during the loop below, keep track of all the defs that `are used` below the current position in the `schedule`
      val usedSet = scala.collection.mutable.Set.empty[Exp[_]]

      def isUsed(sym: Exp[_]) = usedSet.contains(sym)

      /** Keep the assignments of symbols to the branches of IfThenElse
        if a definition is assigned to IF statement then it will be in either THEN or ELSE branch, according to flag
        */
      val assignments = scala.collection.mutable.Map.empty[Exp[_], BranchPath]
      def isAssigned(sym: Exp[_]) = assignments.contains(sym)

      val ifBranches = scala.collection.mutable.Map.empty[Exp[_], IfBranches]

      // should return definitions that are not in usedSet
      def getLocalUnusedSchedule(s: Exp[_]): Seq[TableEntry[_]] = {
        if (usedSet.contains(s)) Seq()
        else {
          val sch = buildLocalScheduleFrom(s, _.getDeps.filter(!usedSet.contains(_)))
          sch
        }
      }

      // builds branches for the `cte`
      def getIfBranches(ifSym: Exp[_], cte: IfThenElse[_]) = {
        val IfThenElse(c, t, e) = cte
        val cs = buildLocalScheduleFrom(c)
        val ts = getLocalUnusedSchedule(t)
        val es = getLocalUnusedSchedule(e)

        val cSet = cs.map(_.sym).toSet
        val tSet = ts.map(_.sym).toSet
        val eSet = es.map(_.sym).toSet

        // a symbol can be in a branch if all is true:
        // 1) the branch root depends on it
        // 2) the other branch doesn't depends on it
        // 3) the condition doesn't depend on it
        // 4) it is not used below the If statement
        val tbody = ts.filter(tp => !(eSet.contains(tp.sym) || cSet.contains(tp.sym)))
        val ebody = es.filter(tp => !(tSet.contains(tp.sym) || cSet.contains(tp.sym)))

        IfBranches(thisGraph, ifSym, DefBlock(List(t), tbody), DefBlock(List(e), ebody))
      }

      def assignBranch(sym: Exp[_], ifSym: Exp[_], thenOrElse: Boolean) = {
        assignments(sym) = BranchPath(thisGraph, ifSym, thenOrElse)
      }

      // traverse the lambda body from the results to the arguments
      for (TableEntry(s, d) <- schedule.reverseIterator) {
        if (!isAssigned(s)) {
          // process current definition
          d match {
            case cte@IfThenElse(c, t, e) => {
              val ifSym = s
              val bs = getIfBranches(ifSym, cte)
              ifBranches(ifSym) = bs

              // assign symbols to this IF
              // put symbol to the current IF
              for (tp <- bs.thenBody.schedule) {
                assignBranch(tp.sym, ifSym, thenOrElse = true)
              }
              for (tp <- bs.elseBody.schedule) {
                assignBranch(tp.sym, ifSym, thenOrElse = false)
              }

              // mark shallow scope for each branch
              val tUsed = bs.thenBody.freeVars
              val eUsed = bs.elseBody.freeVars
              usedSet ++= tUsed
              usedSet ++= eUsed
            }
            case _ =>
          }
          val deps = s.getDeps       // for IfThenElse is gets the roots of each branch and condition
          usedSet ++= deps
        }
      }


      // create resulting immutable structures
      val resAssignments = assignments.toMap
      val resBranches = ifBranches.toMap
      LambdaBranches(resBranches, resAssignments)
    }

    def isAssignedToIfBranch(sym: Exp[_]) = branches.assignments.contains(sym)
  }

  case class DefBlock(val roots: List[Exp[_]], override val schedule: Seq[TableEntry[_]]) extends AstGraph {
    override def boundVars = Nil
    override lazy val freeVars = super.freeVars
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
  case class IfBranches(graph: AstGraph, ifSym: Exp[_], thenBody: DefBlock, elseBody: DefBlock)
  {
    // filter out definitions from this branches that were reassigned to the deeper levels
    def cleanBranches(assignments: Map[Exp[_], BranchPath]) = {
      val thenClean = thenBody.schedule.filter(tp => assignments(tp.sym).ifSym == ifSym)
      val elseClean = elseBody.schedule.filter(tp => assignments(tp.sym).ifSym == ifSym)
      IfBranches(graph, ifSym, thenBody.copy(schedule = thenClean), elseBody.copy(schedule = elseClean))
    }
    override def toString = {
      val Def(IfThenElse(cond,_,_)) = ifSym
      val msg =
        s"""
           |${ifSym} = if (${cond}) then
           |  ${thenBody.schedule.map(tp => s"${tp.sym} -> ${tp.rhs}" ).mkString("\n")}
           |else
           |  ${elseBody.schedule.map(tp => s"${tp.sym} -> ${tp.rhs}" ).mkString("\n")}
         """.stripMargin
      msg
    }
  }

  /** Keeps a branching structure of the Lambda
    */
  case class LambdaBranches(ifBranches: Map[Exp[_], IfBranches], assignments: Map[Exp[_], BranchPath])

  implicit class AstGraphOps(graph: AstGraph) {
    def startsWith(other: AstGraph): Boolean = {
      ???
    }
  }

  def buildScheduleForResult(st: Seq[Exp[_]], neighbours: Exp[_] => Seq[Exp[_]]): Seq[TableEntry[_]] = {
    val startNodes = st.flatMap(e => findDefinition(e).toList)

    def succ(tp: TableEntry[_]): Seq[TableEntry[_]] = {
      //println("dep"+d +"="+dep(d.rhs))
      val ns = neighbours(tp.sym)
      ns.flatMap { e =>
        //println(d + "->" + e)
        findDefinition(e).toList
      }
    }

    GraphUtil.stronglyConnectedComponents[TableEntry[_]](startNodes)(succ).flatten
  }
}
