package scalan.staged

import scalan.ScalanStaged
import scalan.common.Lazy
import scalan.Base

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

  class MapTransformer(val subst: Map[Exp[Any], Exp[Any]] = Map()) extends Transformer {
    def apply[A](x: Exp[A]): Exp[A] = subst.get(x) match {
      case Some(y) if y != x => apply(y.asRep[A])  // transitive closure
      case _ => x
    }
    def isDefinedAt(x: Rep[_]) = subst.contains(x)
    def domain: Set[Rep[_]] = subst.keySet
  }

  object MapTransformer {
    implicit val ops: TransformerOps[MapTransformer] = new TransformerOps[MapTransformer] {
      def empty = new MapTransformer()
      def add(t: MapTransformer, kv: (Rep[_], Rep[_])): MapTransformer =
        new MapTransformer(t.subst + kv)
    }
  }

//  case class RuleRewriter[A](rule: RewriteRule[A]) extends Rewriter {
//    def apply(x: Exp[_]): Exp[_] = rule.unapply(x) match {
//      case Some(args) => rule(args)
//      case _ => x
//    }
//  }
//  implicit def toRuleRewriter[A](rule: RewriteRule[A]): Rewriter = new RuleRewriter(rule)

  implicit class PartialRewriter(val pf: PartialFunction[Exp[_], Exp[_]]) extends Rewriter {
    def apply(x: Exp[_]): Exp[_] = pf.isDefinedAt(x) match {
      case true => pf(x)
      case _ => x
    }
  }

  object DecomposeRewriter extends Rewriter {
    def apply(x: Exp[_]): Exp[_] = x match {
      case Def(d) => d.decompose match {
        case None => x
        case Some(y) => y
      }
      case _ => x
    }
  }



  trait Rewriter extends (Exp[_] => Exp[_]) { self =>

    def orElse(other: Rewriter): Rewriter = (x: Exp[_]) => {
        val y = self(x)
        (x == y) match { case true => other(x) case _ => y }
    }
    def andThen(other: Rewriter): Rewriter = (x: Exp[_]) => {
      val y = self(x)
      val res = other(y)
      res
    }

    def |(other: Rewriter) = orElse(other)
    def ~(other: Rewriter) = andThen(other)

    //def ??(x: Exp[_] => Boolean): Rewriter = (x: Exp[_]) => other(self(x))
  }
  object Rewriter {
    implicit def functionToRewriter(f: Exp[_] => Exp[_]): Rewriter = new Rewriter { def apply(x: Exp[_]) = f(x) }
  }

  val NoRewriting: Rewriter = (x: Exp[_]) => x

  abstract class Mirror[Ctx <: Transformer : TransformerOps] {
    def apply(t: Ctx, rw: Rewriter, x: Exp[_]): (Ctx, Exp[_]) = (t, x.mirror(t))

    // every mirrorXXX method should return a pair (t + (v -> v1), v1)
    protected def mirrorVar(t: Ctx, rewriter: Rewriter, v: Exp[_]): (Ctx, Exp[_]) = {
      val newVar = fresh(Lazy(v.elem.asElem[Any]))
      (t + (v -> newVar), newVar)
    }

    protected def mirrorDef(t: Ctx, rewriter: Rewriter, node: Exp[_], d: Def[_]): (Ctx, Exp[_]) = {
      val (t1, mirrored) = apply(t, rewriter, node)
      var res = mirrored
      var curr = res
      do {
        curr = res
        res = rewriter(curr)
      } while (res != curr)

      (t1 + (node -> res), res)
    }

    protected def getMirroredLambdaSym(node: Exp[_]): Exp[_] = fresh(Lazy(node.elem.asElem[Any]))

    // require: should be called after lam.schedule is mirrored
    private def getMirroredLambdaDef(t: Ctx, newLambdaSym: Exp[_], lam: Lambda[_,_]): Lambda[_,_] = {
      val newVar = t(lam.x)
      val newBody = t(lam.y)
      val newLambdaDef = new LambdaWrapper(None, newVar, newBody, newLambdaSym.asRep[Any=>Any])
      newLambdaDef
    }

    protected def mirrorLambda(t: Ctx, rewriter: Rewriter, node: Exp[_], lam: Lambda[_,_]): (Ctx, Exp[_]) = {
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

    protected def mirrorNode(t: Ctx, rewriter: Rewriter, node: Exp[_]): (Ctx, Exp[_]) = {
      isMirrored(t, node) match {         // cannot use 'if' because it becomes staged
        case true => (t, t(node))
        case _ =>
          node match {
            case Var(_) =>
              mirrorVar(t, rewriter, node)

            case Def(lam: Lambda[_, _]) =>
              mirrorLambda(t, rewriter, node, lam)

            case Def(d) =>
              mirrorDef(t, rewriter, node, d)
          }
      }
    }

    def mirrorSymbols(t0: Ctx, rewriter: Rewriter, nodes: List[Exp[_]]) =
      nodes.foldLeft((t0, Nil: List[Exp[_]])) { case ((t1, nodes), n) => {
        val (t2, n1) = mirrorNode(t1, rewriter, n)
        (t2, nodes ++ List(n1))
      }}

    def mirrorSymbol(startNode: Exp[_], rewriter: Rewriter, t: Ctx): (Ctx, Exp[_]) = {
      val (t1, ss) = mirrorSymbols(t, rewriter, List(startNode))
      (t1, ss.head)
    }
  }

  def mirror[Ctx <: Transformer : TransformerOps] = new Mirror[Ctx] {}
  val DefaultMirror = mirror[MapTransformer]

  object Transformer {
    val Id = new MapTransformer()
  }

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

  //TODO remove by replacing all usages with pairMany
  def mkTuple(xs: List[Rep[_]]): Rep[_] = xs.reverse match {
    case x1 :: (tail@(x2 :: _)) => (x1 /: tail) ((s, x) => Pair(x, s))
    case x :: _ => x
    case _ => ()
  }

  def pairMany(env: List[Exp[Any]]): Exp[Any] =
    env.reduceRight(Pair(_, _))

  def unpairMany(s: Exp[Any]): List[Exp[Any]] = s match {
    case Def(Tup(x,y)) => unpairMany(x) ::: unpairMany(y)
    case _ => List(s)
  }

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

  class TupleTree(val root: Exp[_], val children: List[SymbolTree]) extends SymbolTree {
    override def toString =
      if (isLeaf) root.toString
      else "Tup(%s)".format(children.mkString(","))

    lazy val paths = children match {
      case Nil => List((Nil, root))
      case _ =>
        for {
          (i,ch) <- children.indices.toList zip children
          (p, s) <- ch.paths
        } yield (i + 1 :: p, s)
    }

    def mkNewTree(r: Exp[_], cs: List[SymbolTree]) = TupleTree(r, cs)

    def hasViews: Boolean = {
      paths exists { case (_, s) => symbolHasViews(s) }
    }

    def mirror(leafSubst: Exp[_] => Exp[_]): TupleTree =
      if (isLeaf)
        mkNewTree(leafSubst(root), Nil)
      else {
        val newChildren = children map (_.mirror(leafSubst))
        val newRoot = pairMany(newChildren map (_.root))
        mkNewTree(newRoot, newChildren)
      }


    def fromViewSubst[A](s: Exp[A]) = s match {
      case Def(view: ViewArray[_, _]) => view.arr  //TODO it looks like don't need View anymore?
      case Def(UserTypeExp(iso)) =>
        val repr = iso.from(s)
        repr
      case UserTypeSym(iso) =>
        val repr = iso.from(s)
        repr
      case _ => s
    }

    def toViewSubst[A, B](s_v: Exp[B], s: Exp[A]): Exp[_] = s_v match {
      case Def(view: ViewArray[a, b]) =>             //TODO it looks like don't need View anymore?
        implicit val eB = view.iso.eTo
        ViewArray(s.asRep[Array[a]])(view.iso)
      case Def(UserTypeExp(iso: Iso[A, B] @unchecked)) => iso.to(s)
      case UserTypeSym(iso: Iso[A, B] @unchecked) => iso.to(s)
      case _ => s
    }

    def eliminateViews: TupleTree = {
      mirror(x => fromViewSubst(x))
    }

    def toView(x: Exp[_]) = {
      val subst = (paths map {
        case (p, s_v) =>
          val s = projectPath(x, p)
          val s_iso = toViewSubst(s_v, s)
          s_v -> s_iso
      }).toMap
      val x_iso = mirror(x => subst.getOrElse(x, x))
      x_iso.root
    }

    def fromView(x_iso: Exp[_]): Exp[_] = {
      val subst = (paths map {
        case (p, s_v) =>
          val s_iso = projectPath(x_iso, p)
          val s = fromViewSubst(s_iso)
          s_v -> s
      }).toMap
      val x = mirror(x => subst.getOrElse(x, x))
      x.root
    }
  }

  object TupleTree {
    def apply(root: Exp[_], children: List[SymbolTree]) = new TupleTree(root, children)

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
      val eT = s.elem
      s match {
        case Def(Tup(TupleTree(l),TupleTree(r))) =>
          Some(TupleTree(s, List(l, r)))
        case _ => Some(TupleTree(s, Nil))
      }
    }
  }
}
