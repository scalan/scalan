package scalan.staged

import scalan.ScalanStaged
import scalan.common.Lazy

trait Transforming { self: ScalanStaged =>

  def mirror[A](d: Def[A], f: Transformer): Exp[_] = d.mirror(f)

  implicit class DefMirroringExtensions[A](d: Def[A]) {
    def mirrorWith[A](newArgs: Exp[_]*) = {
      val args: List[Exp[_]] = dep(d)
      if (newArgs.length != args.length) throw !!!(s"invalid mirroring of $d: different number of args")

      val subst = (args, newArgs).zipped.toMap
      val t = new MapTransformer(subst)
      mirror(d, t)
    }
  }

  //  class SubstTransformer extends Transformer {
  //    private val subst = new HashMap[Exp[Any], Exp[Any]]
  //
  //    def apply[A](x: Exp[A]): Exp[A] = subst.get(x) match {
  //      case Some(y) if y != x => apply(y.asRep[A])  // transitive closure
  //      case _ => x
  //    }
  //    def isDefinedAt(x: Rep[_]) = subst.contains(x)
  //
  //    def +=(kv: (Exp[Any], Exp[Any])) = subst += kv
  //  }

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
      case Def(d) => d.decompose match {
        case None => x
        case Some(y) => y
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
    def apply[A](t: Ctx, rw: Rewriter, x: Exp[A]): (Ctx, Exp[_]) = (t, x.mirror(t))

    // every mirrorXXX method should return a pair (t + (v -> v1), v1)
    protected def mirrorVar[A](t: Ctx, rewriter: Rewriter, v: Exp[A]): (Ctx, Exp[_]) = {
      val newVar = fresh(Lazy(v.elem))
      (t + (v -> newVar), newVar)
    }

    protected def mirrorDef[A](t: Ctx, rewriter: Rewriter, node: Exp[A], d: Def[A]): (Ctx, Exp[_]) = {
      val (t1, mirrored) = apply(t, rewriter, node)
      var res = mirrored
      var curr = res
      do {
        curr = res
        res = rewriter(curr)
      } while (res != curr)

      (t1 + (node -> res), res)
    }

    protected def getMirroredLambdaSym[A, B](node: Exp[A => B]): Exp[_] = fresh(Lazy(node.elem))

    // require: should be called after lam.schedule is mirrored
    private def getMirroredLambdaDef(t: Ctx, newLambdaSym: Exp[_], lam: Lambda[_,_]): Lambda[_,_] = {
      val newVar = t(lam.x)
      val newBody = t(lam.y)
      val newLambdaDef = new LambdaWrapper(None, newVar, newBody, newLambdaSym.asRep[Any=>Any], lam.mayInline)
      // from Scalan
//      val newLambdaDef = new Lambda(None, newVar, newBody)(newVar.elem, newBody.elem) {
//        // TODO problem with types?
//        override val self = newLambdaSym.asRep[A => B]
//      }
      newLambdaDef
    }

    protected def mirrorLambda[A, B](t: Ctx, rewriter: Rewriter, node: Exp[A => B], lam: Lambda[A, B]): (Ctx, Exp[_]) = {
      val (t1, newVar) = mirrorNode(t, rewriter, lam.x)
      val newLambdaSym = getMirroredLambdaSym(node)

      lambdaStack.push(newLambdaSym)
      val schedule = lam.bodySchedule map { case TableEntry(sym, d) => sym }
      val (t2, _) = mirrorSymbols(t1, rewriter, schedule)
      lambdaStack.pop

      val newLambda = getMirroredLambdaDef(t2, newLambdaSym, lam)
      createDefinition(newLambdaSym, newLambda)

      (t2 + (node -> newLambdaSym), newLambdaSym)
    }

    protected def isMirrored(t: Ctx, node: Exp[_]): Boolean = t.isDefinedAt(node)

    // TODO make protected
    def mirrorNode[A](t: Ctx, rewriter: Rewriter, node: Exp[A]): (Ctx, Exp[_]) = {
      isMirrored(t, node) match {         // cannot use 'if' because it becomes staged
        case true => (t, t(node))
        case _ =>
          node match {
            case Def(lam: Lambda[a, b]) =>
              mirrorLambda(t, rewriter, node.asRep[a => b], lam)
            case Def(d) =>
              mirrorDef(t, rewriter, node, d)
            case _ =>
              mirrorVar(t, rewriter, node)
          }
      }
    }

    def mirrorSymbols(t0: Ctx, rewriter: Rewriter, nodes: List[Exp[_]]) =
      nodes.foldLeft((t0, Nil: List[Exp[_]])) { case ((t1, nodes), n) => {
        val (t2, n1) = mirrorNode(t1, rewriter, n)
        (t2, nodes ++ List(n1))
      }}

    // TODO simplify to mirrorNode if possible
    def mirrorSymbol(startNode: Exp[_], rewriter: Rewriter, t: Ctx): (Ctx, Exp[_]) = {
      val (t1, ss) = mirrorSymbols(t, rewriter, List(startNode))
      (t1, ss.head)
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
            ch <- children;
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
    def apply(root: Exp[_], unfoldChildren: AnyExp => List[AnyExp]): ProjectionTree =
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
    def fromProjectionTree(ptree: ProjectionTree, subst: ExpSubst): TupleTree =
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
}
