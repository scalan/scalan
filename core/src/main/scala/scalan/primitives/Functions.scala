package scalan.primitives

import scalan.staged.ProgramGraphs
import scalan.{Base, Scalan, Lazy}
import scala.collection.mutable
import scala.language.implicitConversions

trait Functions extends Base with ProgramGraphs { self: Scalan =>

  implicit class LambdaOps[A,B](f: Rep[A => B]) {
    def apply(x: Rep[A]): Rep[B] = mkApply(f, x)
    def >>[C](g: Rep[B => C]) = compose(g, f)
    def <<[C](g: Rep[C => A]) = compose(f, g)
    def compile: A => B = self.compile(f)
  }
  implicit def fun[A,B](f: Rep[A] => Rep[B])(implicit eA: LElem[A]): Rep[A => B] = mkLambda(f, true)
  implicit def fun2[A,B,C](f: (Rep[A], Rep[B])=>Rep[C])(implicit eA: LElem[A], eB: LElem[B]): Rep[((A,B))=>C] = mkLambda(f)
  def funGlob[A,B](f: Rep[A] => Rep[B])(implicit eA: LElem[A]): Rep[A => B] = mkLambda(f, false)

  // more convenient to call with explicit eA
  def typedfun[A, B](eA: Elem[A])(f: Rep[A] => Rep[B]): Rep[A => B] =
    fun(f)(Lazy(eA))

  // see BooleanFunctionOps for example usage
  def sameArgFun[A, B, C](sample: Rep[A => C])(f: Rep[A] => Rep[B]): Rep[A => B] =
    typedfun(sample.elem.eDom)(f)

  def composeBi[A, B, C, D](f: Rep[A => B], g: Rep[A => C])(h: (Rep[B], Rep[C]) => Rep[D]): Rep[A => D] = {
    sameArgFun(f) { x => h(f(x), g(x)) }
  }

  class Lambda[A, B](val f: Option[Exp[A] => Exp[B]], val x: Exp[A], val y: Exp[B], private val self0: Rep[A => B], val mayInline: Boolean)
                    (implicit val eA: Elem[A] = x.elem, val eB: Elem[B] = y.elem)
    extends BaseDef[A => B] with AstGraph with Product { thisLambda =>
    override lazy val self = self0

    // ensure all lambdas of the same type have the same hashcode,
    // so they are tested for alpha-equivalence
    override lazy val hashCode: Int = 41 * (41 + x.elem.hashCode) + y.elem.hashCode
    override def equals(other: Any) =
      other match {
        case that: Lambda[_,_] =>
          (that canEqual this) && matchLambdas(this, that, false, Map.empty).isDefined
        case _ => false
      }
    override def toString = s"Lambda(${if (f.isDefined) "f is Some" else "f is None"}, $x => $y})"
    def canEqual(other: Any) = other.isInstanceOf[Lambda[_,_]]

    // Product implementation
    def productElement(n: Int): Any = n match {
      case 0 => x
      case 1 => y
      case _ => throw new NoSuchElementException(s"Lambda.productElement($n) is undefined")
    }
    def productArity: Int = 2

    // AstGraph implementation
    val boundVars = List(x)
    val roots = List(y)
    override lazy val freeVars = super.freeVars
    override lazy val schedule = super.schedule

    def isGlobalLambda: Boolean =
      freeVars.forall { x =>
        val xIsGlobalLambda = x.isLambda && { val Def(lam: Lambda[_, _]) = x; lam.isGlobalLambda }
        x.isConst || xIsGlobalLambda
      }
  }

  type LambdaData[A,B] = (Lambda[A,B], Option[Exp[A] => Exp[B]], Exp[A], Exp[B])
  object Lambda {
    def unapply[A,B](lam: Lambda[A, B]): Option[LambdaData[A,B]] = lam match {
      case null => None
      case _ => Some((lam, lam.f, lam.x, lam.y))
    }
  }

  override def transformDef[A](d: Def[A], t: Transformer) = d match {
    case l: Lambda[a, b] =>
      import l.{eA, eB}
      val newSym = fresh[a => b]
      val newLam = new Lambda(None, t(l.x), t(l.y), newSym, l.mayInline)
      toExp(newLam, newSym).asRep[A]
    case _ => super.transformDef(d, t)
  }

  /**
   * Matcher for lambdas which don't depend on their arguments
   * (but can close over other expressions, unlike VeryConstantLambda).
   */
  object ConstantLambda {
    // if lam.y depends on lam.x indirectly, lam.schedule must contain the dependency path
    // and its length will be > 1
    def unapply[A,B](lam: Lambda[A, B]): Option[Exp[B]] =
      if (lam.schedule.length <= 1 && !dep(lam.y).contains(lam.x) && lam.y != lam.x)
        Some(lam.y)
      else
        None
  }

  /**
   * Matcher for lambdas which return staging-time constants.
   * VeryConstantLambda(x) should be equivalent to ConstantLambda(Def(Const(x)))
   */
  object VeryConstantLambda {
    def unapply[A,B](lam: Lambda[A, B]): Option[B] = lam.y match {
      case Def(Const(y)) => Some(y)
      case _ => None
    }
  }

  // matcher version of Lambda.isIdentity
  object IdentityLambda {
    def unapply[A,B](lam: Lambda[A, B]): Boolean = lam.isIdentity
  }

  case class Apply[A,B]
    (f: Exp[A => B], arg: Exp[A])
    (implicit val eB: LElem[B])   // enforce explicit laziness at call sites to tie recursive knot (see executeFunction)
      extends Def[B]
  {
    def selfType = eB.value
  }

  implicit class LambdaExtensions[A, B](lam: Lambda[A,B]) {
    def argsTree: ProjectionTree = lam.projectionTreeFrom(lam.x)
  }

  implicit class FuncExtensions[A, B](f: Exp[A=>B]) {
    implicit def eA = f.elem.eDom
    def getLambda: Lambda[A,B] = f match {
      case Def(lam: Lambda[_,_]) => lam.asInstanceOf[Lambda[A,B]]
      case _ => !!!(s"Expected symbol of Lambda node but was $f", f)
    }

    def zip[C](g: Rep[A=>C]): Rep[A=>(B,C)] = {
      implicit val eB = f.elem.eRange
      implicit val eC = g.elem.eRange
      fun { (x: Rep[A]) => Pair(f(x), g(x)) }
    }

    def argsTree = getLambda.argsTree
  }

  type Subst = Map[Sym, Sym]

  def alphaEqual(s1: Sym, s2: Sym): Boolean = matchExps(s1, s2, false, Map.empty).isDefined

  def patternMatch(s1: Sym, s2: Sym): Option[Subst] = matchExps(s1, s2, true, Map.empty)

  protected def matchExps(s1: Sym, s2: Sym, allowInexactMatch: Boolean, subst: Subst): Option[Subst] = s1 match {
    case _ if s1 == s2 || subst.get(s1) == Some(s2) || subst.get(s2) == Some(s1) =>
      Some(subst)
    case Def(d1) => s2 match {
      case Def(d2) =>
        matchDefs(d1, d2, allowInexactMatch, subst).map(_ + (s1 -> s2))
      case _ => None
    }
    case _ =>
      if (allowInexactMatch && !subst.contains(s1)) {
        Some(subst + (s1 -> s2))
      } else {
        None
      }
  }

  @inline
  private def matchLambdas(lam1: Lambda[_, _], lam2: Lambda[_, _], allowInexactMatch: Boolean, subst: Subst) =
    if (lam1.x.elem == lam2.x.elem)
      matchExps(lam1.y, lam2.y, allowInexactMatch, subst + (lam1.x -> lam2.x))
    else
      None

  protected def matchDefs(d1: Def[_], d2: Def[_], allowInexactMatch: Boolean, subst: Subst): Option[Subst] = d1 match {
    case lam1: Lambda[_, _] => d2 match {
      case lam2: Lambda[_, _] =>
        matchLambdas(lam1, lam2, allowInexactMatch, subst)
      case _ => None
    }
    case _ =>
      if (d1.getClass == d2.getClass && d1.productArity == d2.productArity && d1.selfType.name == d2.selfType.name) {
        matchIterators(d1.productIterator, d2.productIterator, allowInexactMatch, subst)
      } else
        None
  }

  // generalize to Seq or Iterable if we get nodes with deps of these types
  protected def matchIterators(i1: Iterator[_], i2: Iterator[_], allowInexactMatch: Boolean, subst: Subst): Option[Subst] =
    if (i1.hasNext) {
      if (i2.hasNext) {
        matchAny(i1.next(), i2.next(), allowInexactMatch, subst).flatMap(matchIterators(i1, i2, allowInexactMatch, _))
      } else None
    } else {
      if (i2.hasNext) None else Some(subst)
    }

  protected def matchAny(a1: Any, a2: Any, allowInexactMatch: Boolean, subst: Subst): Option[Subst] = a1 match {
    case s1: Sym => a2 match {
      case s2: Sym =>
        matchExps(s1, s2, allowInexactMatch, subst)
      case _ => None
    }
    case l1: Iterable[_] => a2 match {
      case l2: Iterable[_] =>
        matchIterators(l1.iterator, l2.iterator, allowInexactMatch, subst)
      case _ => None
    }
    case _ => if (a1 == a2) Some(subst) else None
  }

  //=====================================================================================
  //   Function application

  def mkApply[A,B](f: Exp[A => B], x: Exp[A]): Exp[B] = {
    implicit val leB = Lazy(f.elem.eRange)
    if (recursion.valuesIterator.contains(f)) {
      // f is not in Defs table at this time, thus a special case here
      f.isRecursive = true
      // hit recursion call ! so just make an application
      Apply(f, x)
    } else {
      // not in recursion, so lookup definition
      f match {
        case Def(lam: Lambda[A, B] @unchecked) if lam.mayInline => // unfold initial non-recursive function
          try {
            unfoldLambda(lam, x)
          } catch {
            case e: StackOverflowError =>
              if (f.isRecursive)
                Apply(f, x)
              else
                !!!(s"Stack overflow in applying non-recursive $f($x)", e, f, x)
          }
        case Def(Apply(_, _)) => // function that is a result of Apply (curried application)
          Apply(f, x)
        case _ => // unknown function
          Apply(f, x)
      }
    }
  }

  def unfoldLambda[A,B](lam: Lambda[A,B], x: Exp[A]): Exp[B] = {
    lam.f match {
      case Some(g) => g(x) // unfold initial non-recursive function
      case None => mirrorApply(lam, x)  // f is mirrored, unfold it by mirroring
    }
  }

  def unfoldLambda[A,B](f: Exp[A=>B], x: Exp[A]): Exp[B] = {
    val lam = f.getLambda
    unfoldLambda(lam, x)
  }

  def mirrorApply[A,B](lam: Lambda[A, B], s: Exp[A]): Exp[B] = {
    val body = lam.scheduleSyms
    val (t, _) = DefaultMirror.mirrorSymbols(new MapTransformer(lam.x -> s), NoRewriting, lam, body)
    t(lam.y).asRep[B]
  }

  //=====================================================================================
  //   Function reification

  def mkLambda[A,B](f: Exp[A] => Exp[B], mayInline: Boolean)(implicit eA: LElem[A]): Exp[A=>B] = {
    val x = fresh[A]
    lambda(x)(f, mayInline)
  }

  def mkLambda[A,B,C](f: Rep[A]=>Rep[B]=>Rep[C])
                     (implicit eA: LElem[A], eB: Elem[B]): Rep[A=>B=>C] = {
    val y = fresh[B]
    mkLambda((a: Rep[A]) => lambda(y)((b:Rep[B]) => f(a)(b), true), true)
  }

  def mkLambda[A,B,C](f: (Rep[A], Rep[B])=>Rep[C])(implicit eA: LElem[A], eB: LElem[B]): Rep[((A,B))=>C] = {
    implicit val leAB = Lazy(pairElement(eA.value, eB.value))
    mkLambda({ (p: Rep[(A, B)]) =>
      val (x, y) = unzipPair(p)
      f(x, y)
    }, true)
  }

  private def lambda[A,B](x: Rep[A])(f: Exp[A] => Exp[B], mayInline: Boolean)(implicit leA: LElem[A]): Exp[A=>B] = {
    implicit val eA = leA.value
    var eB: Elem[B] = null // will be known after f is executed
    val fSym = fresh[A => B](Lazy { assert(eB != null); funcElement(eA, eB) })
    val Block(y) = reifyEffects(executeFunction(f, x, fSym))
    eB = y.elem
    val eF = fSym.elem // force lazy value in sSym
    reifyFunction0(f, x, y, fSym, mayInline)
  }

  class LambdaStack {
    var stack = new mutable.Stack[Sym]()
    def top: Option[Sym] = stack.isEmpty match { case true => None case _ => Some(stack.top) }
    def push(e: Sym): this.type = { stack.push(e); this }
    def pop: Sym = stack.pop
  }
  protected var recursion = Map.empty[_ => _, Sym]

  protected val lambdaStack = new LambdaStack
  private def executeFunction[A, B](f: Exp[A]=>Exp[B], x: Exp[A], fSym: Exp[A => B]): Exp[B] = {
    recursion.get(f) match {
      case None =>
        val saveRecursion = recursion
        recursion += (f -> fSym)
        lambdaStack.push(fSym)
        val res = f(x) // execute looking for recursive call back to this exec
        lambdaStack.pop
        recursion = saveRecursion
        res
      case Some(fs) => // hit recursion call !
        fs.isRecursive = true
        Apply(fs.asInstanceOf[Exp[A=>B]], x)(Lazy(fSym.elem.eRange))
    }
  }

  private def reifyFunction0[A, B](fun: Exp[A] => Exp[B], x: Exp[A], y: Exp[B], fSym: Exp[A => B], mayInline: Boolean): Exp[A => B] = {
    val lam = new Lambda(Some(fun), x, y, fSym, mayInline)
    findOrCreateDefinition(lam, fSym)
  }

  def identityFun[A](implicit e: Elem[A]) = fun[A, A](x => x)

  def upcustFun[A: Elem, B >: A]: Rep[A => B] = fun[A,B](x => x)

  def constFun[A, B](x: Rep[B])(implicit e: Elem[A]) = {
    implicit val eB = x.elem
    fun[A, B](_ => x)
  }

  def compose[A, B, C](f: Rep[B => C], g: Rep[A => B]): Rep[A => C] = {
    implicit val eA = g.elem.eDom
    implicit val eC = f.elem.eRange
    fun { x => f(g(x)) }
  }

  def compile[A, B](f: Rep[A => B]): A => B = {
    ???
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    case Apply(f @ Def(l: Lambda[a,b]), x) if l.mayInline => {
      f(x)
    }
    case _ => super.rewriteDef(d)
  }
}
