package scalan.primitives

import scalan.staged.{ProgramGraphs, BaseExp}
import scalan.{ScalanExp, ScalanSeq, Scalan}
import collection.mutable
import scala.language.{implicitConversions}
import scalan.common.Lazy

trait Functions { self: Scalan =>
  implicit class LambdaOps[A,B](f: Rep[A => B]) {
    def apply(x: Rep[A]): Rep[B] = mkApply(f, x)
  }

  def mkApply[A,B](f: Rep[A=>B], x: Rep[A]): Rep[B]
  def mkLambda[A,B](fun: Rep[A] => Rep[B], mayInline: Boolean)(implicit eA: LElem[A]): Rep[A => B]
  def mkLambda[A,B,C](fun: Rep[A]=>Rep[B]=>Rep[C])(implicit eA: LElem[A], eB: LElem[B]): Rep[A=>B=>C]
  def mkLambda[A,B,C](fun: (Rep[A], Rep[B])=>Rep[C])(implicit eA: LElem[A], eB: LElem[B]): Rep[((A,B))=>C]
  implicit def fun[A,B](f: Rep[A] => Rep[B])(implicit eA: LElem[A]): Rep[A => B] = mkLambda(f, true)
  implicit def fun2[A,B,C](fun: (Rep[A], Rep[B])=>Rep[C])(implicit eA: LElem[A], eB: LElem[B]): Rep[((A,B))=>C] = mkLambda(fun)
  def funGlob[A,B](f: Rep[A] => Rep[B])(implicit eA: LElem[A]): Rep[A => B] = mkLambda(f, false)
  //def fun[A,B,C](f: Rep[A]=>Rep[B]=>Rep[C])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[A=>B=>C] = mkLambda(f)
  //def letrec[A,B](f: (Rep[A=>B])=>(Rep[A]=>Rep[B]), mayInline: Boolean)(implicit eA: Elem[A], eb:Elem[B]): Rep[A=>B]
  //def fun[A,B,C]  (f: Rep[A] => Rep[B] => Rep[C])(implicit eA: Elem[A], eB: Elem[B]): Rep[A=>B=>C]
}

trait FunctionsSeq extends Functions { self: ScalanSeq =>
  def mkApply[A,B](f: Rep[A=>B], x: Rep[A]): Rep[B] = f(x)
  def mkLambda[A,B](f: Rep[A] => Rep[B], mayInline: Boolean)(implicit eA: LElem[A]): Rep[A => B] = f
  def mkLambda[A,B,C](fun: Rep[A]=>Rep[B]=>Rep[C])(implicit eA: LElem[A], eB: LElem[B]) = fun
  def mkLambda[A,B,C](fun: (Rep[A], Rep[B])=>Rep[C])(implicit eA: LElem[A], eB: LElem[B]): Rep[((A,B))=>C] = {
    case (x, y) => fun(x, y)
  }
  //def fun[A,B,C]  (f: Rep[A] => Rep[B] => Rep[C])(implicit eA: Elem[A], eB: Elem[B]): Rep[A=>B=>C] = f
//  def letrec[A,B](f: (Rep[A=>B])=>(Rep[A]=>Rep[B]), mayInline: Boolean)(implicit eA: Elem[A], eb:Elem[B]): Rep[A=>B] = {
//    f(letrec(f, mayInline))(_)
//  }
}

trait FunctionsExp extends Functions with BaseExp with ProgramGraphs { self: ScalanExp =>

  class Lambda[A, B](val f: Option[Exp[A] => Exp[B]], val x: Exp[A], val y: Exp[B], self0: Rep[A=>B], val mayInline: Boolean)
                    (implicit val eA: Elem[A] = x.elem, val eB: Elem[B] = y.elem)
    extends BaseDef[A => B] with AstGraph with Product { thisLambda =>
    lazy val uniqueOpId = s"Lambda[${eA.name},${eB.name}]"

    override lazy val self = self0
    override def mirror(t: Transformer) = {
      val newSym = fresh[A=>B]
      val newLam = new Lambda(None, t(x), t(y), newSym, mayInline)
      toExp(newLam, newSym)
    }

    // structural equality pattern implementation
    override lazy val hashCode: Int = 41 * (41 + x.hashCode) + y.hashCode
    override def equals(other: Any) =
      other match {
        case that: Lambda[_,_] =>
          (that canEqual this) &&
          (this.x equals that.x) &&
          (this.y equals that.y)
        case _ => false
      }
    override def toString = s"Lambda(${if (f.isDefined) "f is Some" else "f is None"}, $x => $y})"
    def canEqual(other: Any) = other.isInstanceOf[Lambda[_,_]]

    // Product implementation
    val productElements = scala.Array[Any](f, x, y)
    def productElement(n: Int): Any = productElements(n)
    def productArity: Int = 3

    // AstGraph implementation
    val boundVars = List(x)
    val roots = List(y)
    override lazy val freeVars = super.freeVars
    override lazy val schedule = super.schedule

    def isGlobalLambda: Boolean =
      freeVars.forall { x => x.isConst || x.isLambda }
  }

  type LambdaData[A,B] = (Lambda[A,B], Option[Exp[A] => Exp[B]], Exp[A], Exp[B])
  object Lambda {
    def unapply[A,B](d: Def[A => B]): Option[LambdaData[A,B]] = d match {
      case l: Lambda[_,_] =>
        val lam = l.asInstanceOf[Lambda[A,B]]
        Some((lam, lam.f, lam.x, lam.y))
      case _ => None
    }
  }

  case class Apply[A,B]
    (f: Exp[A => B], arg: Exp[A])
    (implicit eB: LElem[B])   // enforce explicit laziness at call sites to tie recursive knot (see executeFunction)
      extends Def[B]
  {
    def selfType = eB.value
    lazy val uniqueOpId = name(arg.elem, selfType)
    override def mirror(t: Transformer) = Apply(t(f), t(arg))(eB)
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

    def zip[C](g: Rep[A=>C]): Rep[A=>(B,C)] =
      fun { (x: Rep[A]) => Pair(f(x), g(x)) }

    def argsTree = getLambda.argsTree
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
        case Def(lam: Lambda[A, B] @unchecked) if !f.isRecursive && lam.mayInline => // unfold initial non-recursive function
          unfoldLambda(f, lam, x)
        case Def(Apply(_, _)) => // function that is a result of Apply (curried application)
          Apply(f, x)
        case _ => // unknown function
          Apply(f, x)
      }
    }
  }

  def unfoldLambda[A,B](f: Exp[A=>B], lam: Lambda[A,B], x: Exp[A]): Exp[B] = {
    lam.f match {
      case Some(g) => g(x) // unfold initial non-recursive function
      case None => mirrorApply(f, x)  // f is mirrored, unfold it by mirroring
    }
  }

  def unfoldLambda[A,B](f: Exp[A=>B], x: Exp[A]): Exp[B] = {
    val lam = f.getLambda
    unfoldLambda(f, lam, x)
  }

  def mirrorApply[A,B](f: Exp[A => B], s: Exp[A], subst: MapTransformer = MapTransformer.Empty): Exp[B] = {
    val Def(lam: Lambda[A, B]) = f
    val body = lam.scheduleSyms
    val (t, _) = DefaultMirror.mirrorSymbols(subst + (lam.x -> s), NoRewriting, body)
    t(lam.y).asRep[B]
  }

  //=====================================================================================
  //   Function reification

  def mkLambda[A,B](f: Exp[A] => Exp[B], mayInline: Boolean)(implicit eA: LElem[A]): Exp[A=>B] = {
    // in Scalan
    // letrec[A,B]((f: Exp[A => B]) => fun, mayInline)
    val x = fresh[A]
    lambda(x)(f, mayInline)
  }

  def mkLambda[A,B,C]
  (fun: Rep[A]=>Rep[B]=>Rep[C])
  (implicit eA: LElem[A], eB: LElem[B]): Rep[A=>B=>C] = {
    val y = fresh[B]
    mkLambda((a: Rep[A]) => lambda(y)((b:Rep[B]) => fun(a)(b), true), true)
  }

  def mkLambda[A,B,C](fun: (Rep[A], Rep[B])=>Rep[C])(implicit eA: LElem[A], eB: LElem[B]): Rep[((A,B))=>C] = {
    implicit val leAB = Lazy(pairElement(eA.value, eB.value))
    mkLambda({ (p: Rep[(A, B)]) =>
      val (x, y) = unzipPair(p)
      fun(x, y)
    }, true)
  }

  def lambda[A,B](x: Rep[A])(fun: Exp[A] => Exp[B], mayInline: Boolean): Exp[A=>B] = {
    val res = fresh[A => B](Lazy(
      !!!("should not be called: this symbol should have definition and element should be taken from corresponding lambda"))
    )
    reifyFunction(fun, x, res, mayInline)
  }

  class LambdaStack {
    var stack = new mutable.Stack[Exp[_]]()
    def top: Option[Exp[_]] = stack.isEmpty match { case true => None case _ => Some(stack.top) }
    def push(e: Exp[_]): this.type = { stack.push(e); this }
    def pop: Exp[_] = stack.pop
  }
  protected var recursion = Map.empty[_ => _, Exp[_]]

  protected val lambdaStack = new LambdaStack
  def executeFunction[A, B](f: Exp[A]=>Exp[B], x: Exp[A], fSym: Exp[A => B]): Exp[B] = {
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

//  def letrec[A:Elem,B:Elem](f: (Rep[A=>B])=>(Rep[A]=>Rep[B]), mayInline: Boolean): Rep[A=>B] = {
//    val x = fresh[A]
//    val res = fresh[A => B]
//    val fun = f(res)
//    reifyFunction(fun, x, res, mayInline)
//  }

//  def reifyFunction[A:Elem, B:Elem](fun: Exp[A] => Exp[B], x: Exp[A], res: Exp[A=>B], mayInline: Boolean): Exp[A=>B] = {
//    val y = executeFunction(fun, x, res)
//    val lam = new Lambda(Some(fun), x/*.asSymbol*/, y, mayInline) { override val self = res }
//    findDefinition(lam) match {
//      case Some(TableEntry(sym, Lambda(Some(f), _, _, _))) if f == fun =>
//        sym
//      case _ =>
//        createDefinition(res/*.asSymbol*/, lam)
//        res
//    }
//  }

  def reifyFunction[A, B](fun: Exp[A] => Exp[B], x: Exp[A], fSym: Exp[A=>B], mayInline: Boolean): Exp[A=>B] = {
    val y = executeFunction(fun, x, fSym)
    val lam = new Lambda(Some(fun), x, y, fSym, mayInline)

    thunkStack.top match {
      case Some(scope) =>
        scope.findDef(lam) match {
          case Some(TableEntry(sym, Lambda(_, Some(f), _, _))) =>
            if (f equals fun) sym.asRep[A=>B]
            else {
              val te = createDefinition(scope.thunkSym, fSym, lam)
              scope += te
              fSym
            }
          case None =>
            val te = createDefinition(scope.thunkSym, fSym, lam)
            scope += te
            te.sym
        }
      case None =>
        findDefinition(globalThunkSym, lam) match {
          case Some(TableEntry(sym, Lambda(_, Some(f), _, _))) => {
            f equals fun match {
              case true => sym.asRep[A=>B]
              case false =>
                createDefinition(globalThunkSym, fSym, lam)
                fSym
            }
          }
          case None =>
            createDefinition(globalThunkSym, fSym, lam)
            fSym
        }
    }
  }

  def functionSplit[A, B, C](f: Rep[A=>B], g: Rep[A=>C]): Rep[A=>(B,C)] =
    fun { (x: Rep[A]) => Pair(f(x), g(x)) }(Lazy(f.elem.eDom))
}

