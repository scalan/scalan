/**
 * Author: Alexander Slesarenko
 * Date: 9/17/12
 */
package scalan.primitives

import scalan.staged.{ProgramGraphs, BaseExp}
import scalan.{ScalanStaged, ScalanSeq, Scalan}
import collection.mutable
import scala.language.{implicitConversions}

trait Functions { self: Scalan =>

  implicit class LambdaOps[A:Elem,B:Elem](f: Rep[A => B]) {
    def apply(x: Rep[A]): Rep[B] = mkApply(f,x)
  }

  def mkLambda[A,B](fun: Rep[A] => Rep[B])(implicit eA: Elem[A], eB: Elem[B]): Rep[A => B]
  def mkLambda[A,B,C](fun: Rep[A]=>Rep[B]=>Rep[C])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[A=>B=>C]

  def mkApply[A:Elem,B:Elem](fun: Rep[A=>B], arg: Rep[A]): Rep[B]
  def letrec[A,B](f: (Rep[A=>B])=>(Rep[A]=>Rep[B]))(implicit eA: Elem[A], eb:Elem[B]): Rep[A=>B]
  def fun[A,B]  (f: Rep[A] => Rep[B])(implicit eA: Elem[A], eB: Elem[B]): Rep[A => B] = mkLambda(f)
  def fun[A,B,C](f: Rep[A]=>Rep[B]=>Rep[C])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[A=>B=>C] = mkLambda(f)
  //def fun[A,B,C](f: (Rep[A],Rep[B])=>Rep[C])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[((A,B))=>C] = mkLambda((p: Rep[(A,B)]) => f(p._1, p._2))
}

trait FunctionsSeq extends Functions { self: ScalanSeq =>
  def mkLambda[A,B](fun: Rep[A] => Rep[B])(implicit eA: Elem[A], eB: Elem[B]): Rep[A => B] = fun
  def mkLambda[A,B,C](fun: Rep[A]=>Rep[B]=>Rep[C])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[A=>B=>C] = fun
  def mkApply[A:Elem,B:Elem](fun: Rep[A => B], arg: Rep[A]): Rep[B] = fun(arg)

  //def fix[A,B](f: (A=>B)=>(A=>B)): A=>B = f(fix(f))(_)
  def letrec[A,B](f: (Rep[A=>B])=>(Rep[A]=>Rep[B]))(implicit eA: Elem[A], eb:Elem[B]): Rep[A=>B] = {
    f(letrec(f))(_)
  }
}

trait FunctionsExp extends Functions with BaseExp with ProgramGraphs { self: ScalanStaged =>

  trait LambdaBase[A,B] extends Def[A => B] {
    implicit def eA: Elem[A]
    implicit def eB: Elem[B]
    def f: Option[Exp[A] => Exp[B]]
    def x: Exp[A]
    def y: Exp[B]
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

    lazy val schedule: List[TP[_]] = {
      isIdentity match {
        case false =>
          val g = new PGraph(y)
          val sh = g.scheduleFrom(x)
          (sh, y) match {
            case (Nil, DefTP(tp)) => List(tp)  // the case when body is const
            case _ => sh
          }

        case _ => Nil
      }
    }

    lazy val scheduleWithConsts: List[TP[_]] = {
      isIdentity match {
        case false =>
          val g = new PGraph(y)
          val sh = g.schedule
          (sh, y) match {
            case (Nil, DefTP(tp)) => List(tp)  // the case when body is const
            case _ => sh
          }

        case _ => Nil
      }
    }
    lazy val scheduleSyms = schedule map { _.sym }

    def scheduleAll: List[TP[_]] = schedule flatMap(tp => tp.rhs match {
      case lam@Lambda(_,_,_) => lam.scheduleAll :+ tp
      case _ => List(tp)
    })

    def isIdentity: Boolean = y == x
    def isLocalDef[T](tp: TP[T]): Boolean = isLocalDef(tp.sym)
    def isLocalDef(s: Exp[Any]): Boolean = scheduleSyms contains s

    lazy val freeVars: Set[Exp[_]] = {
      val alldeps = schedule flatMap { tp => tp.rhs.getDeps }
      val external = alldeps filter { s => !(isLocalDef(s) || s == x)  }
      external.toSet
    }

    def isGlobalLambda: Boolean = {
      val free = freeVars filterNot (_.isConst)
      free.isEmpty
    }

    override def isScalarOp: Boolean = {
      val allScalars = !(schedule exists { tp => !tp.rhs.isScalarOp })
      allScalars
    }
  }

  case class Lambda[A,B]
    (f: Option[Exp[A] => Exp[B]], x: Exp[A], y: Exp[B])
    (implicit val eA: Elem[A], val eB: Elem[B]) extends LambdaBase[A,B]
  {
    override def mirror(t: Transformer) = {
      val newSym = fresh[A=>B]
      
      val newLam = new LambdaWrapper(None, t(x), t(y), newSym)
      toExp(newLam, newSym)
    }
    lazy val objType = element[A => B]
  }
  class LambdaWrapper[A:Elem,B:Elem](
       override val f: Option[Exp[A] => Exp[B]], 
       override val x: Exp[A], 
       override val y: Exp[B], 
       override val thisSymbol: Rep[A=>B])  extends Lambda[A,B](f, x, y) 

  case class Apply[A,B](f: Exp[A => B], arg: Exp[A])(implicit val eA: Elem[A], val objType: Elem[B]) extends Def[B] {
    override def mirror(t: Transformer): Rep[_] = Apply(t(f), t(arg))
  }

  implicit class FuncOps[A:Elem, B:Elem](s: Exp[A=>B]) {
    def getLambda: Lambda[A,B] = s match {
      case Def(lam: Lambda[_,_]) => lam.asInstanceOf[Lambda[A,B]]
      case _ => !!!("Expected symbol of Lambda node but was %s".format(s), s)
    }
  }
  //implicit def extendExpFunc[A:Elem, B:Elem](s: Exp[A=>B]) = new FuncOps(s)

  //=====================================================================================
  //   Function application

  def mkApply[A:Elem,B:Elem](f: Exp[A => B], x: Exp[A]): Exp[B] = {
    recursion.find(m => m._3 == f) match {
      case None =>  // not in recursion, so lookup definition
        f match {
          case Def(lam: Lambda[_,_]) if !f.isRecursive =>  // unfold initial non-recursive function
            unfoldLambda(f, lam.asInstanceOf[Lambda[A,B]], x)
          case Def(Apply(_, _)) =>  // function that is a result of Apply (high order value)
            Apply(f, x)
          case _ => // unknown function
            Apply(f, x)
        }
      case Some(_) =>  // f is not in Defs table at this time, thus a special case here
        f.isRecursive = true
        Apply(f, x) // hit recursion call ! so just make an application
    }
  }

  def unfoldLambda[A:Elem,B:Elem](f: Exp[A=>B], lam: Lambda[A,B], x: Exp[A]): Exp[B] = {
    lam match {
      case Lambda(Some(g),_,_) => g(x) // unfold initial non-recursive function
      case Lambda(None,_,_) => mirrorApply(f, x)  // f is mirrored, unfold it by mirroring
    }
  }
  def unfoldLambda[A:Elem,B:Elem](f: Exp[A=>B], x: Exp[A]): Exp[B] = {
    val lam = f.getLambda
    unfoldLambda(f, lam, x)
  }

  def mirrorApply[A:Elem,B:Elem](f: Exp[A => B], s: Exp[A], subst: Map[Exp[Any], Exp[Any]] = Map()): Exp[B] = {
    val Def(lam@Lambda(_,x,y)) = f
    val body = lam.schedule map { _.sym }
    val (t, _) = DefaultMirror.mirrorSymbols(new MapTransformer(subst ++ Map(x -> s)), NoRewriting, body)
    t(y).asRep[B]
  }

  //=====================================================================================
  //   Function reification

  def mkLambda[A,B](fun: Exp[A] => Exp[B])(implicit eA: Elem[A], eb:Elem[B]) : Exp[A=>B] = {
    letrec[A,B](f => fun)
  }

  def mkLambda[A,B,C]
              (fun: Rep[A]=>Rep[B]=>Rep[C])
              (implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[A=>B=>C] = {
    val y = fresh[B]
    mkLambda((a: Rep[A]) => lambda(y)((b:Rep[B]) => fun(a)(b)))
  }

  def lambda2[A,B,C](x: Rep[A])(fun: Exp[A] => Exp[B] => Exp[C])(implicit eA: Elem[A], eb:Elem[B], ec:Elem[C]): Exp[A=>B=>C] = {
    val y = fresh[B]
    lambda(x)((a: Rep[A]) => lambda(y)((b:Rep[B]) => fun(a)(b)))
  }

  def mkLambda3[A,B,C,D]
              (f: Rep[A]=>Rep[B]=>Rep[C]=>Rep[D])
              (implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[A=>B=>C=>D] = {
    val y = fresh[B]
    val z = fresh[C]
    mkLambda((a:Rep[A]) => lambda(y)((b: Rep[B]) => lambda(z)(f(a)(b))))
  }

  def letrec[A:Elem,B:Elem](f: (Rep[A=>B])=>(Rep[A]=>Rep[B])): Rep[A=>B] = {
    val x = fresh[A]
    val res = fresh[A => B]
    val fun = f(res)
    reifyFunction(fun, x, res)
  }

  def lambda[A:Elem,B:Elem](x: Rep[A])(fun: Exp[A] => Exp[B]): Exp[A=>B] = {
    val res = fresh[A => B]
    reifyFunction(fun, x, res)
  }

  def reifyFunction[A:Elem, B:Elem](fun: Exp[A] => Exp[B], x: Exp[A], res: Exp[A=>B]): Exp[A=>B] = {
    val y = executeFunction(fun, x, res)
    val lam = new  LambdaWrapper(Some(fun), x, y, res)
    findDefinition(lam) match {
      case Some(TP(sym, Lambda(Some(f), _, _))) => {
        f equals fun match {
          case true => sym.asRep[A=>B]
          case false =>
            createDefinition(res, lam)
            res
        }
      }
      case None =>
        createDefinition(res, lam)
        res
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

  def executeFunction[A:Elem,B:Elem](f: Exp[A]=>Exp[B], x: Exp[A], fSym: Exp[A => B]): Exp[B] = {
    recursion.find(m => m._1 == f) match {
      case None =>
        val saveRecursion = recursion
        recursion = (f, x, fSym)::recursion
        lambdaStack.push(fSym)
        val res = f(x) // execute looking for recursive call back to this exec
        lambdaStack.pop
        recursion = saveRecursion
        res
      case Some((_, _, fs)) => // hit recursion call !
        fs.isRecursive = true
        Apply(fs.asInstanceOf[Exp[A=>B]], x)
    }
  }

  def mergeFunctions[A:Elem,B:Elem,C:Elem](f: Rep[A=>B], g: Rep[A=>C]): Rep[A=>(B,C)] =
    fun { (x: Rep[A]) => Pair(f(x), g(x))  }

  override def formatDef(d: Def[_]) = d match {
    case Lambda(_, x, y) => "\\\\%s -> %s".format(x,  (y match { case Def(b) => formatDef(b) case _ => y}))
    case Apply(f, x) => "%s(%s)".format(f, x)
    case _ => super.formatDef(d)
  }
}

