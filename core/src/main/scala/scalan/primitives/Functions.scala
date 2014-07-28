/**
 * Author: Alexander Slesarenko
 * Date: 9/17/12
 */
package scalan.primitives

import scalan.staged.{ProgramGraphs, BaseExp}
import scalan.{ScalanStaged, ScalanSeq, Scalan}
import collection.mutable
import scala.language.{implicitConversions}
import scalan.common.Lazy

trait Functions { self: Scalan =>

  implicit class LambdaOps[A,B](f: Rep[A => B]) {
    def apply(x: Rep[A]): Rep[B] = mkApply(f, x)
  }

  def mkApply[A,B](f: Rep[A=>B], x: Rep[A]): Rep[B]
  implicit def fun[A,B](f: Rep[A] => Rep[B])(implicit eA: LElem[A]): Rep[A => B]
  //def fun[A,B,C]  (f: Rep[A] => Rep[B] => Rep[C])(implicit eA: Elem[A], eB: Elem[B]): Rep[A=>B=>C]
}

trait FunctionsSeq extends Functions { self: ScalanSeq =>
  def mkApply[A,B](f: Rep[A=>B], x: Rep[A]): Rep[B] = f(x)
  def fun[A,B]    (f: Rep[A] => Rep[B])(implicit eA: LElem[A]): Rep[A => B] = f
  //def fun[A,B,C]  (f: Rep[A] => Rep[B] => Rep[C])(implicit eA: Elem[A], eB: Elem[B]): Rep[A=>B=>C] = f
}

trait FunctionsExp extends Functions with BaseExp with ProgramGraphs { self: ScalanStaged =>

  class Lambda[A, B](val f: Option[Exp[A] => Exp[B]], val x: Exp[A], val y: Exp[B])
                    (implicit val eA: Elem[A] = x.elem, val eB: Elem[B] = y.elem)
    extends BaseDef[A => B] with AstGraph with Product
  { thisLambda =>
    lazy val uniqueOpId = s"Lambda[${eA.name},${eB.name}]"
    
    override def mirror(t: Transformer) = {
      val newSym = fresh[A=>B]
      val newLam = new LambdaWrapper(None, t(x), t(y), newSym)
      toExp(newLam, newSym)
    }

    // structural equality pattern implementation
    override def hashCode: Int = (41 * (41 + x.hashCode) + y.hashCode)
    override def equals(other: Any) =
      other match {
        case that: Lambda[_,_] =>
          (that canEqual this) &&
          (this.x equals that.x) &&
          (this.y equals that.y)
        case _ => false
      }
    def canEqual(other: Any) = other.isInstanceOf[Lambda[_,_]]

    // Product implementation
    val productElements = scala.Array[Any](f, x, y)
    def productElement(n: Int): Any = productElements(n)
    def productArity: Int = 3

    // AstGraph implementation
    def roots = List(y)
    lazy val bodySchedule: List[TableEntry[_]] = {
      if (isIdentity) Nil
      else {
        val g = new PGraph(y)
        val sh = g.scheduleFrom(x)
        (sh, y) match {
          case (Nil, DefTableEntry(tp)) => List(tp)  // the case when body is const
          case _ => sh
        }
      }
    }

    lazy val bodyScheduleSyms = bodySchedule map { _.sym }

    def bodyScheduleAll: List[TableEntry[_]] = {
      bodySchedule flatMap (tp  => tp match {
        case TableEntry(s, lam: Lambda[_, _]) => lam.bodyScheduleAll :+ tp
        case _ => List(tp)
      })
    }

    lazy val scheduleWithConsts: List[TableEntry[_]] =
      if (isIdentity) Nil
      else {
        val g = new PGraph(y)
        val sh = g.schedule
        (sh, y) match {
          case (Nil, DefTableEntry(tp)) => List(tp)  // the case when body is const
          case _ => sh
        }
      }

    def isIdentity: Boolean = y == x
    def isLocalDef[T](tp: TableEntry[T]): Boolean = isLocalDef(tp.sym)
    def isLocalDef(s: Exp[Any]): Boolean = bodyScheduleSyms contains s

    lazy val freeVars: Set[Exp[_]] = {
      val alldeps = bodySchedule flatMap { tp => tp.rhs.getDeps }
      val external = alldeps filter { s => !(isLocalDef(s) || s == x)  }
      external.toSet
    }

    def isGlobalLambda: Boolean = {
      val free = freeVars filterNot (_.isConst)
      free.isEmpty
    }

    override def isScalarOp: Boolean = {
      val allScalars = !(bodySchedule exists { tp => !tp.rhs.isScalarOp })
      allScalars
    }

    /** Builds a schedule starting from symbol `sym` which which consists only of local definitions.
      *  @param sym   the root of the schedule, it can be non-local itself
      *  @param deps  dependence relation between a definition and symbols
      *  @return      a `Seq` of local definitions on which `sym` depends or empty if `sym` is itself non-local
      */
    def buildLocalScheduleFrom(sym: AnyExp, deps: AnyExp => List[AnyExp]): Seq[TableEntry[_]] =
      if (isLocalDef(sym))
        buildScheduleForResult(List(sym), deps(_).filter(isLocalDef(_)))
      else
        Seq.empty

    def buildLocalScheduleFrom(sym: AnyExp): Seq[TableEntry[_]] = buildLocalScheduleFrom(sym, _.getDeps)

    /** Keeps immutable maps describing branching structure of this lambda
      */
    lazy val branches = {
      //Accumulates branches for each IfThenElse definition of this `Lambda` (at the end transformed to immutable)
      val ifBranches = scala.collection.mutable.Map.empty[Exp[_], IfBranches]

      // Keep the assignments of symbols to the branches of IfThenElse
      // if a definition is assigned to IF statement then it will be in either THEN or ELSE branch, according to flag
      val assignments = scala.collection.mutable.Map.empty[Exp[_], BranchPath]

      // during the loop below, keep track of all the defs that `are used` below the current position in the `schedule`
      // `are used` relation is transitive closure of getShallowDeps
      val usedSet = scala.collection.mutable.Set.empty[Exp[_]]

      /** Shallow dependencies don't look into branches of IfThenElse
        */
      def getShallowDeps(d: AnyExp): List[AnyExp] = d match {
        case Def(IfThenElse(c, _, _)) => List(c)
        case _ => d.getDeps
      }

      // traverse the lambda body from the results to the arguments
      for (TableEntry(s, d) <- schedule.reverseIterator) {

        /** Builds a schedule according to the current usedSet
          * @param s starting symbol
          * @return sequence of symbols that 1) local 2) in shallow dependence relation 3) not yet marked
          */
        def getLocalUnusedShallowSchedule(s: AnyExp): Seq[TableEntry[_]] = {
          val sch = buildLocalScheduleFrom(s, getShallowDeps(_).filter(!usedSet.contains(_)))
          sch
        }

        // should return definitions that are not in usedSet
        def getLocalUnusedSchedule(s: Exp[_]): Seq[TableEntry[_]] = {
          if (usedSet.contains(s)) Seq()
          else {
            val sch = buildLocalScheduleFrom(s, _.getDeps.filter(!usedSet.contains(_)))
            sch
          }
        }

        // builds branches for the `cte` with respect to usedSet
        def getIfBranches(ifSym: Exp[_], cte: IfThenElse[_]) = {
          val IfThenElse(_, t, e) = cte
          // current usedSet should be filtered out from the contents of the branches
          val ts = getLocalUnusedSchedule(t)   // NOTE: this is local, unused BUT DEEP dependencies
          val es = getLocalUnusedSchedule(e)

          val tSet = ts.map(_.sym).toSet
          val eSet = es.map(_.sym).toSet

          // a symbol can be in a branch if 1) the branch root depends on it and 2) the other branch doesn't depends on it
          // Lemma: a symbol is excluded from a branch together with all its local dependencies
          val tbody = ts.filter(tp => !eSet.contains(tp.sym))
          val ebody = es.filter(tp => !tSet.contains(tp.sym))

          IfBranches(thisLambda, ifSym, tbody, ebody)
        }

        def assignBranch(sym: Exp[_], ifSym: Exp[_], thenOrElse: Boolean) = {
          assignments(sym) = BranchPath(ifSym, thenOrElse)
        }

        // process current definition
        d match {
          case cte@IfThenElse(c, t, e) =>
            val ifSym = s
            if (!usedSet.contains(ifSym)) {
              // this may happen when ifSym is lambda result or is a root of an IfThenElse branch
              // mark all the symbols from the shallow schedule thus excluding them from branches
              val deps = getLocalUnusedShallowSchedule(ifSym).map(_.sym)
              usedSet ++= deps
            }

            // compute and store branches
            val bs = getIfBranches(ifSym, cte)
            ifBranches += ifSym -> bs

            // assign symbols to this IF
            // put symbol to the current IF (it can later be reassigned to nested IF)
            for (tp <- bs.thenBody) {
              assignBranch(tp.sym, ifSym, thenOrElse = true)
            }
            for (tp <- bs.elseBody) {
              assignBranch(tp.sym, ifSym, thenOrElse = false)
            }

            // mark shallow scope for each branch
            val tUsed = getLocalUnusedShallowSchedule(t).map(_.sym)
            val eUsed = getLocalUnusedShallowSchedule(e).map(_.sym)
            usedSet ++= tUsed
            usedSet ++= eUsed

          case _ =>
            if (!usedSet.contains(s)) {
              // mark all the symbols from the shallow schedule of the lambda scope
              val deps = getLocalUnusedShallowSchedule(s).map(_.sym)
              usedSet ++= deps
            }
        }
      }

      // create resulting immutable structures
      val resAssignments = assignments.toMap
      val resBranches = ifBranches.map { case (ifSym, bs) => (ifSym, bs.cleanBranches(resAssignments)) }.toMap
      LambdaBranches(resBranches, resAssignments)
    }

  }

  /** When stored in Map, describes for each key the branch of the symbol
    * @param ifSym      symbol of the related IfThenElse definition
    * @param thenOrElse true if the symbol is assigned to then branch, false if to the else branch
    */
  case class BranchPath(ifSym: Exp[_], thenOrElse: Boolean)

  /** When stored in a Map, keeps for each IfThenElse schedule of the branches
    * @param lam       owning lambda
    * @param ifSym     symbol of the IfThenElse statement
    * @param thenBody  schedule of `then` branch
    * @param elseBody  schedule of `else` branch
    */
  case class IfBranches(lam: Lambda[_,_], ifSym: AnyExp, thenBody: Seq[TableEntry[_]], elseBody: Seq[TableEntry[_]])
  {
    // filter out definitions from this branches that were reassigned to the deeper levels
    def cleanBranches(assignments: Map[Exp[_], BranchPath]) = {
      val thenClean = thenBody.filter(tp => assignments(tp.sym).ifSym == ifSym)
      val elseClean = elseBody.filter(tp => assignments(tp.sym).ifSym == ifSym)
      IfBranches(lam, ifSym, thenClean, elseClean)
    }
  }

  /** Keeps a branching structure of the Lambda
    */
  case class LambdaBranches(ifBranches: Map[Exp[_], IfBranches], assignments: Map[Exp[_], BranchPath])

  type LambdaData[A,B] = (Lambda[A,B], Option[Exp[A] => Exp[B]], Exp[A], Exp[B])
  object Lambda {
    def unapply[A,B](d: Def[A => B]): Option[LambdaData[A,B]] = d match {
      case l: Lambda[_,_] =>
        val lam = l.asInstanceOf[Lambda[A,B]]
        Some((lam, lam.f, lam.x, lam.y))
      case _ => None
    }
  }

  class LambdaWrapper[A,B](
    f: Option[Exp[A] => Exp[B]], x: Exp[A], y: Exp[B], self0: Rep[A=>B]) extends Lambda[A,B](f, x, y) {
    override lazy val self = self0
  }

  case class Apply[A,B]
    (f: Exp[A => B], arg: Exp[A])
    (implicit eB: LElem[B])   // enforce explicit laziness at call sites to tie recursive knot (see executeFunction)
      extends Def[B]
  {
    def selfType = eB.value
    lazy val self: Rep[B] = this
    lazy val uniqueOpId = name(arg.elem, selfType)
    override def mirror(t: Transformer): Rep[_] = Apply(t(f), t(arg))(eB)
  }

  implicit class LambdaExtensions[A, B](lam: Lambda[A,B]) {
    def argsTree: ProjectionTree = lam.projectionTreeFrom(lam.x)
  }

  implicit class FuncExtensions[A, B](f: Exp[A=>B]) {
    implicit def eA = f.elem.ea
    def getLambda: Lambda[A,B] = f match {
      case Def(lam: Lambda[_,_]) => lam.asInstanceOf[Lambda[A,B]]
      case _ => !!!(s"Expected symbol of Lambda node but was $f", f)
    }

    def zip[C](g: Rep[A=>C]): Rep[A=>(B,C)] =
      fun { (x: Rep[A]) => Pair(f(x), g(x)) }

    def argsTree = getLambda.argsTree
  }

  //=====================================================================================
  //   Function application

  def mkApply[A,B](f: Exp[A => B], x: Exp[A]): Exp[B] = {
    implicit val leB = Lazy(f.elem.eb)
    recursion.find(m => m._3 == f) match {
      case None =>  // not in recursion, so lookup definition
        f match {
          case Def(lam: Lambda[_,_]) if !f.isRecursive => // non-recursive Lambda node
            unfoldLambda(f, lam.asInstanceOf[Lambda[A,B]], x)

          case Def(Apply(_, _)) =>  // function that is a result of Apply (curried application)
            Apply(f, x)

          case _ => // unknown function
            Apply(f, x)
        }
      case Some(_) =>  // f is not in Defs table at this time, thus a special case here
        f.isRecursive = true
        Apply(f, x) // hit recursion call ! so just make an application
    }
  }

  def unfoldLambda[A,B](f: Exp[A=>B], lam: Lambda[A,B], x: Exp[A]): Exp[B] = {
    lam match {
      case Lambda(_,Some(g),_,_) => g(x) // unfold initial non-recursive function
      case Lambda(_,None,_,_) => mirrorApply(f, x)  // f is a result of mirroring, so unfold it by mirroring
    }
  }

  def unfoldLambda[A,B](f: Exp[A=>B], x: Exp[A]): Exp[B] = {
    val lam = f.getLambda
    unfoldLambda(f, lam, x)
  }

  def mirrorApply[A,B](f: Exp[A => B], s: Exp[A], subst: Map[Exp[Any], Exp[Any]] = Map()): Exp[B] = {
    val Def(Lambda(lam,_,x,y)) = f
    val body = lam.bodySchedule map { _.sym }
    val (t, _) = DefaultMirror.mirrorSymbols(new MapTransformer(subst ++ Map(x -> s)), NoRewriting, body)
    t(y).asRep[B]
  }

  //=====================================================================================
  //   Function reification

  implicit def fun[A,B](f: Exp[A] => Exp[B])(implicit eA: LElem[A]): Exp[A=>B] = {
    val x = fresh[A]
    lambda(x)(f)
  }

//  def mkLambda[A,B,C]
//              (fun: Rep[A]=>Rep[B]=>Rep[C])
//              (implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[A=>B=>C] = {
//    val y = fresh[B]
//    mkLambda((a: Rep[A]) => lambda(y)((b:Rep[B]) => fun(a)(b)))
//  }
//
//  def lambda2[A,B,C](x: Rep[A])(fun: Exp[A] => Exp[B] => Exp[C])(implicit eA: Elem[A], eb:Elem[B], ec:Elem[C]): Exp[A=>B=>C] = {
//    val y = fresh[B]
//    lambda(x)((a: Rep[A]) => lambda(y)((b:Rep[B]) => fun(a)(b)))
//  }

  def lambda[A,B](x: Rep[A])(fun: Exp[A] => Exp[B]): Exp[A=>B] = {
    val res = fresh[A => B](Lazy(
      !!!("should not be called: this symbol should have definition and element should be taken from corresponding lambda"))
    )
    reifyFunction(fun, x, res)
  }

  def reifyFunction[A, B](fun: Exp[A] => Exp[B], x: Exp[A], fSym: Exp[A=>B]): Exp[A=>B] = {
    val y = executeFunction(fun, x, fSym)
    val lam = new LambdaWrapper(Some(fun), x, y, fSym)
    findDefinition(lam) match {
      case Some(TableEntry(sym, Lambda(_, Some(f), _, _))) => {
        f equals fun match {
          case true => sym.asRep[A=>B]
          case false =>
            createDefinition(fSym, lam)
            fSym
        }
      }
      case None =>
        createDefinition(fSym, lam)
        fSym
    }
  }

  class LambdaStack {
    var stack = new mutable.Stack[Exp[_]]()
    def top: Option[Exp[_]] = stack.isEmpty match { case true => None case _ => Some(stack.top) }
    def push(e: Exp[_]): this.type = { stack.push(e); this }
    def pop: Exp[_] = stack.pop
  }
  var recursion: List[(Function[_,_], Exp[Any], Exp[Any])] = List()
  val lambdaStack = new LambdaStack

  def executeFunction[A,B](f: Exp[A] => Exp[B], x: Exp[A], fSym: Exp[A => B]): Exp[B] = {
    recursion.find(m => m._1 == f) match {
      case None => {
        val saveRecursion = recursion
        recursion = (f, x, fSym)::recursion
        lambdaStack.push(fSym)
        val res = f(x) // execute looking for recursive call back to this exec
        lambdaStack.pop
        recursion = saveRecursion
        res
      }
      case Some((_, _, fSym)) => {// hit recursion call !
        val f = fSym.asInstanceOf[Exp[A=>B]]
        f.isRecursive = true
        val leB = Lazy(f.elem.eb)
        reifyObject(Apply(f, x)(leB))(leB) 
      }
    }
  }
}

