package scalan.staged

import java.lang.reflect.{Method, InvocationTargetException}

import scala.collection.{mutable, Seq}
import scalan.{Scalan, ScalanExp}
import scalan.common.Lazy
import scala.reflect.runtime.universe._

trait Transforming { self: Scalan =>

  trait Pass {
    def name: String
    def config: PassConfig = Pass.defaultPassConfig
    // TODO what arguments?
    def doFinalization(): Unit = {}
    /**
      * Pass specific optional decision.
      * @param d receiver of the method
      * @param m method to invoke
      * @return Some(decision) if some this Pass defines some logic, None - then core behavior is used
      */
    def isInvokeEnabled(d: Def[_], m: Method): Option[Boolean] = None
  }
  object Pass {
    val defaultPassName = "default"
    val defaultPass = new DefaultPass(defaultPassName)
    val defaultPassConfig = defaultPass.config
  }

  case class PassConfig(
    shouldUnpackTuples: Boolean = false,
    shouldExtractFields: Boolean = true,
    constantPropagation: Boolean = true,
    shouldSlice: Boolean = false
    )
  {
    def withConstantPropagation(value: Boolean) = this.copy(constantPropagation = value)
  }

  class DefaultPass(val name: String, override val config: PassConfig = PassConfig()) extends Pass

  //TODO parallel execution of Compilers
  // Current design doesn't allow to run through passes i two Compilers in parallel
  var _currentPass: Pass = Pass.defaultPass
  def currentPass = _currentPass

  def beginPass(pass: Pass): Unit = {
    _currentPass = pass
  }
  def endPass(pass: Pass): Unit = {
    _currentPass = Pass.defaultPass
  }

  case class SingletonElem[T: WeakTypeTag](value: T) extends BaseElem[T](value)

  sealed trait KeyPath {
    def isNone = this == KeyPath.None
    def isAll = this == KeyPath.All
  }
  object KeyPath {
    case object Root extends KeyPath
    case object This extends KeyPath
    case object All extends KeyPath
    case object None extends KeyPath
    case object First extends KeyPath
    case object Second extends KeyPath
    case class Field(name: String) extends KeyPath
  }

  def keyPathElem(kp: KeyPath): Elem[KeyPath] = SingletonElem(kp)

  implicit class KeyPathElemOps(eKeyPath: Elem[KeyPath]) {
    def keyPath = eKeyPath.asInstanceOf[SingletonElem[KeyPath]].value
  }

}

trait TransformingExp extends Transforming { self: ScalanExp =>


  class MapTransformer(private val subst: Map[Exp[_], Exp[_]]) extends Transformer {
    def this(substPairs: (Exp[_], Exp[_])*) {
      this(substPairs.toMap)
    }
    def apply[A](x: Exp[A]): Exp[A] = subst.get(x) match {
      case Some(y) if y != x => apply(y.asRep[A]) // transitive closure
      case _ => x
    }
    def isDefinedAt(x: Rep[_]) = subst.contains(x)
    def domain: Set[Rep[_]] = subst.keySet

    override def toString = if (subst.isEmpty) "MapTransformer.Empty" else s"MapTransformer($subst)"
  }

  object MapTransformer {
    val Empty = new MapTransformer(Map.empty[Exp[_], Exp[_]])

    implicit val ops: TransformerOps[MapTransformer] = new TransformerOps[MapTransformer] {
      def empty = Empty//new MapTransformer(Map.empty)
      def add[A](t: MapTransformer, kv: (Rep[A], Rep[A])): MapTransformer =
        new MapTransformer(t.subst + kv)
    }
  }

  implicit class PartialRewriter(pf: PartialFunction[Exp[_], Exp[_]]) extends Rewriter {
    def apply[T](x: Exp[T]): Exp[T] =
      if (pf.isDefinedAt(x))
        pf(x).asInstanceOf[Exp[T]]
      else
        x
  }

  object DecomposeRewriter extends Rewriter {
    def apply[T](x: Exp[T]): Exp[T] = x match {
      case Def(d) => decompose(d) match {
        case None => x
        case Some(y) => y
      }
      case _ => x
    }
  }

  object InvokeRewriter extends Rewriter {
    def apply[T](x: Exp[T]): Exp[T] = x match {
      case Def(call: MethodCall) =>
        call.tryInvoke match {
          case InvokeSuccess(res) =>
            res.asRep[T]
          case InvokeFailure(e) =>
            if (e.isInstanceOf[DelayInvokeException])
              x
            else
              !!!(s"Failed to invoke $call", e, x)
          case _ => x
        }
      case _ => x
    }
  }

  abstract class Rewriter { self =>
    def apply[T](x: Exp[T]): Exp[T]

    def orElse(other: Rewriter): Rewriter = new Rewriter {
      def apply[T](x: Exp[T]) = {
        val y = self(x)
        (x == y) match { case true => other(x) case _ => y }
      }
    }
    def andThen(other: Rewriter): Rewriter = new Rewriter {
      def apply[T](x: Exp[T]) = {
        val y = self(x)
        val res = other(y)
        res
      }
    }

    def |(other: Rewriter) = orElse(other)
    def ~(other: Rewriter) = andThen(other)
  }

  val NoRewriting: Rewriter = new Rewriter {
    def apply[T](x: Exp[T]) = x
  }

  abstract class Mirror[Ctx <: Transformer : TransformerOps] {
    def apply[A](t: Ctx, rewriter: Rewriter, node: Exp[A], d: Def[A]): (Ctx, Exp[_]) = (t, transformDef(d, t))

    // every mirrorXXX method should return a pair (t + (v -> v1), v1)
    protected def mirrorVar[A](t: Ctx, rewriter: Rewriter, v: Exp[A]): (Ctx, Exp[_]) = {
      val newVar = fresh(Lazy(v.elem))
      val (t1, mirroredMetadata) = mirrorMetadata(t, v, newVar)
      setAllMetadata(newVar, mirroredMetadata)
      (t1 + (v -> newVar), newVar)
    }

    protected def rewriteUntilFixPoint[T](start: Exp[T], mn: MetaNode, rw: Rewriter): Exp[T] = {
      var res = start
      var curr: Exp[T] = res
      do {
        curr = res
        setAllMetadata(curr, mn)
        res = rw(curr)
      } while (res != curr)
      res
    }

    protected def mirrorDef[A](t: Ctx, rewriter: Rewriter, node: Exp[A], d: Def[A]): (Ctx, Exp[_]) = {
      val (t1, mirrored) = apply(t, rewriter, node, d)
      val (t2, mirroredMetadata) = mirrorMetadata(t1, node, mirrored)
      val res = rewriteUntilFixPoint(mirrored, mirroredMetadata, rewriter)
      (t2 + (node -> res), res)
    }

    protected def getMirroredLambdaSym[A, B](node: Exp[A => B]): Exp[_] = fresh(Lazy(node.elem))

    // require: should be called after oldlam.schedule is mirrored
    private def getMirroredLambdaDef(t: Ctx, newLambdaSym: Exp[_], oldLam: Lambda[_,_], newRoot: Exp[_]): Lambda[_,_] = {
      val newVar = t(oldLam.x)
      val newLambdaDef = new Lambda(None, newVar, newRoot, newLambdaSym.asRep[Any=>Any], oldLam.mayInline)
      newLambdaDef
    }

    protected def mirrorLambda[A, B](t: Ctx, rewriter: Rewriter, node: Exp[A => B], lam: Lambda[A, B]): (Ctx, Exp[_]) = {
      var tRes: Ctx = t
      val (t1, newVar) = mirrorNode(t, rewriter, lam, lam.x)
      val newLambdaSym = getMirroredLambdaSym(node)
      lambdaStack.push(newLambdaSym)

      // original root
      val originalRoot = lam.y match {
        case Def(Reify(x, _, _)) => x
        case _ => lam.y
      }
      // new effects may appear during body mirroring (i.e. new Reflect nodes)
      // thus we need to forget original Reify node and create a new one
      val Block(newRoot) = reifyEffects({
        val schedule = lam.filterReifyRoots(lam.scheduleSingleLevel).map(_.sym)
        val (t2, _) = mirrorSymbols(t1, rewriter, lam, schedule)
        tRes = t2
        tRes(originalRoot) // this will be a new root (wrapped in Reify if needed)
      })

      lambdaStack.pop
      val newLambda = getMirroredLambdaDef(tRes, newLambdaSym, lam, newRoot)
      createDefinition(thunkStack.top, newLambdaSym, newLambda)
      val newLambdaExp = toExp(newLambda, newLambdaSym)

      val (tRes2, mirroredMetadata) = mirrorMetadata(tRes, node, newLambdaExp)
      val resLam = rewriteUntilFixPoint(newLambdaExp, mirroredMetadata, rewriter)

      (tRes2 + (node -> resLam), resLam)
    }

    protected def mirrorBranch[A](t: Ctx, rewriter: Rewriter, g: AstGraph, branch: ThunkDef[A]): (Ctx, Exp[_]) = {
      // get original root unwrapping Reify nodes
      val originalRoot = branch.root match {
        case Def(Reify(x, _, _)) => x
        case _ => branch.root
      }
      val schedule = branch.filterReifyRoots(branch.scheduleSingleLevel).map(_.sym)
      val (t2, _) = mirrorSymbols(t, rewriter, branch, schedule)
      val newRoot = t2(originalRoot)
      (t2, newRoot)
    }

    protected def mirrorIfThenElse[A](t: Ctx, rewriter: Rewriter, g: AstGraph, node: Exp[A], ite: IfThenElse[A]): (Ctx, Exp[_]) = {
      g.branches.ifBranches.get(node) match {
        case Some(branches) =>
          var tRes: Ctx = t
          val newIte = IF (t(ite.cond)) THEN {
            val (t1, res) = mirrorBranch(t, rewriter, g, branches.thenBody)
            tRes = t1
            res
          } ELSE {
            val (t2, res) = mirrorBranch(tRes, rewriter, g, branches.elseBody)
            tRes = t2
            res
          }
          (tRes + (node -> newIte), newIte)
        case _ =>
          mirrorDef(t, rewriter, node, ite)
      }
    }

    protected def mirrorThunk[A](t: Ctx, rewriter: Rewriter, node: Exp[Thunk[A]], thunk: ThunkDef[A]): (Ctx, Exp[_]) = {
      val newThunkSym = fresh(Lazy(node.elem))
      val newScope = new ThunkScope(newThunkSym)

      thunkStack.push(newScope)
      val schedule = thunk.scheduleSyms
      val (t1, newSchedule) = mirrorSymbols(t, rewriter, thunk, schedule)
      thunkStack.pop

      val newRoot = t1(thunk.root)
      val newThunk = ThunkDef(newRoot, newSchedule.map { case DefTableEntry(te) => te })

      createDefinition(thunkStack.top, newThunkSym, newThunk)
      (t1 + (node -> newThunkSym), newThunkSym)
    }

    protected def mirrorMetadata[A, B](t: Ctx, old: Exp[A], mirrored: Exp[B]) =
      (t, allMetadataOf(old))

    protected def isMirrored(t: Ctx, node: Exp[_]): Boolean = t.isDefinedAt(node)

    private def setMirroredMetadata(t1: Ctx, node: Exp[_], mirrored: Exp[_]): (Ctx, Exp[_]) = {
      val (t2, mirroredMetadata) = mirrorMetadata(t1, node, mirrored)
      setAllMetadata(mirrored, mirroredMetadata.filterSinglePass)
      (t2, mirrored)
    }

    // TODO make protected
    def mirrorNode[A](t: Ctx, rewriter: Rewriter, g: AstGraph, node: Exp[A]): (Ctx, Exp[_]) = {
      isMirrored(t, node) match {         // cannot use 'if' because it becomes staged
        case true => (t, t(node))
        case _ =>
//          (
          node match {
            case Def(d) => d match {
              case lam: Lambda[a, b] =>
                val (t1, mirrored) = mirrorLambda(t, rewriter, node.asRep[a => b], lam)
                setMirroredMetadata(t1, node, mirrored)
              case th: ThunkDef[a] =>
                val (t1, mirrored) = mirrorThunk(t, rewriter, node.asRep[Thunk[a]], th)
                setMirroredMetadata(t1, node, mirrored)
              case ite: IfThenElse[a] =>
                val (t1, mirrored) = mirrorIfThenElse(t, rewriter, g, node.asRep[a], ite)
                setMirroredMetadata(t1, node, mirrored)
              case _ =>
                mirrorDef(t, rewriter, node, d)
            }
            case _ =>
              val (t1, mirrored) = mirrorVar(t, rewriter, node)
              setMirroredMetadata(t1, node, mirrored)
          }
//          ) match {
//            case (t1, mirrored: Exp[b]) =>
//              val (t2, mirroredMetadata) = mirrorMetadata(t1, node, mirrored)
//              setAllMetadata(mirrored, mirroredMetadata)
//              (t2, mirrored)
//          }
      }
    }

    def mirrorSymbols(t0: Ctx, rewriter: Rewriter, g: AstGraph, nodes: Seq[Exp[_]]) = {
      val (t, revMirrored) = nodes.foldLeft((t0, List.empty[Exp[_]])) {
        case ((t1, nodes), n) =>
          val (t2, n1) = mirrorNode(t1, rewriter, g, n)
          (t2, n1 :: nodes)
      }
      (t, revMirrored.reverse)
    }
  }

  def mirror[Ctx <: Transformer : TransformerOps] = new Mirror[Ctx] {}
  val DefaultMirror = mirror[MapTransformer]

  //  sealed abstract class TupleStep(val name: String)
  //  case object GoLeft extends TupleStep("L")
  //  case object GoRight extends TupleStep("R")
  type TuplePath = List[Int]

  def projectPath(x:Exp[Any], path: TuplePath) = {
    val res = path.foldLeft(x)((y,i) => TupleProjection(y.asRep[(Any,Any)], i))
    res
  }

  // build projection from the root taking projection structure from the tree
  // assert(result.root == root)
  // NOTE: tree.root is not used
  def projectTree(root:Exp[Any], tree: ProjectionTree): ProjectionTree = {
    val newChildren = tree.children.map(child => {
      val i = projectionIndex(child.root)
      val newChildRoot = TupleProjection(root.asRep[(Any,Any)], i)
      projectTree(newChildRoot, child)
    })
    ProjectionTree(root, newChildren)
  }

  def pairMany(env: List[Exp[_]]): Exp[_] =
    env.reduceRight(Pair(_, _))

  abstract class SymbolTree {
    def root: Exp[_]
    def children: List[SymbolTree]
    def mirror(leafSubst: Exp[_] => Exp[_]): SymbolTree
    def paths: List[(TuplePath, Exp[_])]
    def isLeaf = children.isEmpty
  }

  class ProjectionTree(val root: Exp[_], val children: List[ProjectionTree]) extends SymbolTree {
    override def toString = s"""ProjTree(\n${paths.mkString("\n")})"""

    lazy val paths: List[(TuplePath, Exp[_])] =
      if (isLeaf) List((Nil, root))
      else{
        for {
            ch <- children
            (p, s) <- ch.paths
          } yield {
            val i = projectionIndex(ch.root)
            (i :: p, s)
          }
    }

    def mkNewTree(r: Exp[_], cs: List[ProjectionTree]) = ProjectionTree(r, cs)
    def mirror(subst: Exp[_] => Exp[_]): ProjectionTree = {
      val newRoot = subst(root)
      projectTree(newRoot, this)
    }
  }
  object ProjectionTree {
    def apply(root: Exp[_], children: List[ProjectionTree]) = new ProjectionTree(root, children)
    def apply(root: Exp[_], unfoldChildren: ExpAny => List[ExpAny]): ProjectionTree =
      ProjectionTree(root, unfoldChildren(root) map (apply(_, unfoldChildren)))
  }

  class TupleTree(val root: Exp[_], val children: List[TupleTree]) extends SymbolTree {
    override def toString =
      if (isLeaf) root.toString
      else "Tup(%s)".format(children.mkString(","))

    lazy val paths: List[(TuplePath, Exp[_])] = children match {
      case Nil => List((Nil, root))
      case _ =>
        for {
          (i,ch) <- children.indices.toList zip children
          (p, s) <- ch.paths
        } yield (i + 1 :: p, s)
    }

    def mirror(leafSubst: Exp[_] => Exp[_]): TupleTree =
      if (isLeaf)
        TupleTree(leafSubst(root), Nil)
      else {
        val newChildren = children map (_.mirror(leafSubst))
        val newRoot = pairMany(newChildren map (_.root))
        TupleTree(newRoot, newChildren)
      }
  }

  object TupleTree {
    def apply(root: Exp[_], children: List[TupleTree]) = new TupleTree(root, children)

    // require ptree to be sorted by projectionIndex
    def fromProjectionTree(ptree: ProjectionTree, subst: ExpAny => ExpAny): TupleTree =
      if (ptree.isLeaf)
        TupleTree(subst(ptree.root), Nil)
      else {
        val newChildren = ptree.children map (fromProjectionTree(_, subst))
        val newRoot = pairMany(newChildren map (_.root))
        TupleTree(newRoot, newChildren)
      }

    def unapply[T](s: Exp[T]): Option[TupleTree] = {
      s match {
        case Def(Tup(TupleTree(l),TupleTree(r))) =>
          Some(TupleTree(s, List(l, r)))
        case _ => Some(TupleTree(s, Nil))
      }
    }
  }

  abstract class Analyzer {
    def name: String
    override def toString = s"Analysis($name)"
  }

  trait Lattice[M[_]] {
    def maximal[T:Elem]: Option[M[T]]
    def minimal[T:Elem]: Option[M[T]]
    def join[T](a: M[T], b: M[T]): M[T]
  }

  trait BackwardAnalyzer[M[_]] extends Analyzer {
    type MarkedSym = (Exp[T], M[T]) forSome {type T}
    type MarkedSyms = Seq[MarkedSym]
    def keyPrefix: String = name

    def lattice: Lattice[M]
    def defaultMarking[T:Elem]: M[T]

    def updateMark[T](s: Exp[T], other: M[T]): (Exp[T], M[T]) = {
      s -> lattice.join(getMark(s), other)
    }

    def beforeAnalyze[A,B](l: Lambda[A,B]): Unit = {}

    def getInboundMarkings[T](te: TableEntry[T], outMark: M[T]): MarkedSyms

    def getLambdaMarking[A,B](lam: Lambda[A,B], mDom: M[A], mRange: M[B]): M[A => B]

    def getMarkingKey[T](implicit eT:Elem[T]): MetaKey[M[T]] = markingKey[T](keyPrefix).asInstanceOf[MetaKey[M[T]]]

    def clearMark[T](s: Exp[T]): Unit = {
      implicit val eT = s.elem
      s.removeMetadata(getMarkingKey[T])
    }

    def getMark[T](s: Exp[T]): M[T] = {
      implicit val eT = s.elem
      val mark = s.getMetadata(getMarkingKey[T]).getOrElse(defaultMarking[T])
      mark
    }

    def hasMark[T](s: Exp[T]): Boolean = {
      implicit val eT = s.elem
      s.getMetadata(getMarkingKey[T]).isDefined
    }

    def updateOutboundMarking[T](s: Exp[T], mark: M[T]): Unit = {
      implicit val eT = s.elem
      val current = getMark(s)
      val updated = lattice.join(current, mark)
      val key = getMarkingKey[T]
      s.setMetadata(key)(updated, Some(true))
    }

    def backwardAnalyzeRec(g: AstGraph): Unit = {
      val revSchedule = g.schedule.reverseIterator
      for (te <- revSchedule) te match { case te: TableEntry[t] =>
        val s = te.sym
        val d = te.rhs
        // back-propagate analysis information (including from Lambda to Lambda.y, see LevelAnalyzer)
        val outMark = getMark(s)
        val inMarks = getInboundMarkings[t](te, outMark)
        for ((s, mark) <- inMarks) {
          updateOutboundMarking(s, mark)
        }
        d match {
          // additionally if it is Lambda
          case l: Lambda[a,b] =>
            // analyze lambda after the markings were assigned to the l.y during previous propagation step
            backwardAnalyzeRec(l)
            // markings were propagated up to the lambda variable
            val mDom = getMark(l.x)
            val mRange = getMark(l.y)

            // update markings attached to l
            val lMark = getLambdaMarking(l, mDom, mRange)
            updateOutboundMarking(l.self, lMark)
          case _ =>
        }
      }
    }
  }

  trait Marking[T] {
    def elem: Elem[T]
    def basePath: KeyPath = KeyPath.Root
    def nonEmpty: Boolean
  }

  class EmptyMarking[T](val elem: Elem[T]) extends Marking[T] {
    def nonEmpty = false
  }

  type MarkedSym = (Exp[T], Marking[T]) forSome {type T}
  type MarkedSyms = Seq[MarkedSym]

  class MarkingElem[T:Elem] extends BaseElem[Marking[T]](new EmptyMarking[T](element[T]))
  implicit def markingElem[T:Elem] = new MarkingElem[T]

  private val markingKeys = mutable.Map.empty[(String, Elem[_]), MetaKey[_]]

  def markingKey[T](prefix: String)(implicit eT:Elem[T]): MetaKey[Marking[T]] = {
    val key = markingKeys.getOrElseUpdate((prefix, eT), MetaKey[Marking[T]](s"${prefix}_marking[${eT.name}]"))
    key.asInstanceOf[MetaKey[Marking[T]]]
  }
}
