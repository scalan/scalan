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
  def fun[A,B]    (f: Rep[A] => Rep[B])(implicit eA: LElem[A]): Rep[A => B]
  //def fun[A,B,C]  (f: Rep[A] => Rep[B] => Rep[C])(implicit eA: Elem[A], eB: Elem[B]): Rep[A=>B=>C]
}

trait FunctionsSeq extends Functions { self: ScalanSeq =>
  def mkApply[A,B](f: Rep[A=>B], x: Rep[A]): Rep[B] = f(x)
  def fun[A,B]    (f: Rep[A] => Rep[B])(implicit eA: LElem[A]): Rep[A => B] = f
  //def fun[A,B,C]  (f: Rep[A] => Rep[B] => Rep[C])(implicit eA: Elem[A], eB: Elem[B]): Rep[A=>B=>C] = f
}

trait FunctionsExp extends Functions with BaseExp with ProgramGraphs { self: ScalanStaged =>

  trait LambdaBase[A,B] extends Def[A => B] with Product {
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

    val productElements = Array[Any](f, x, y)
    def productElement(n: Int): Any = productElements(n)
    def productArity: Int = 3
    
    
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

    def scheduleAll: List[TP[_]] = schedule.flatMap(tp => tp.rhs match {
      case lam: Lambda[_, _] => lam.scheduleAll :+ tp
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

  class Lambda[A,B](val f: Option[Exp[A] => Exp[B]], val x: Exp[A], val y: Exp[B]) extends LambdaBase[A,B] {
    implicit def eA: Elem[A] = x.elem
    implicit def eB: Elem[B] = y.elem
    override def mirror(t: Transformer) = {
      implicit val eAtoB = element[A=>B]
      val newSym = fresh[A=>B]
      val newLam = new LambdaWrapper(None, t(x), t(y), newSym)
      toExp(newLam, newSym)
    }
    lazy val selfType = element[A => B]
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

  class LambdaWrapper[A,B](
       override val f: Option[Exp[A] => Exp[B]], 
       override val x: Exp[A], 
       override val y: Exp[B], 
       override val self: Rep[A=>B])  extends Lambda[A,B](f, x, y)

  case class Apply[A,B]
    (f: Exp[A => B], arg: Exp[A])
    (implicit eB: LElem[B])   // enforce explicit laziness at call sites to tie recursive knot (see executeFunction)
      extends Def[B]
  {
    def selfType = eB.value
    override def mirror(t: Transformer): Rep[_] = Apply(t(f), t(arg))(eB)
  }

  implicit class FuncOps[A, B](f: Exp[A=>B]) {
    implicit def eA = f.elem.ea
    def getLambda: Lambda[A,B] = f match {
      case Def(lam: Lambda[_,_]) => lam.asInstanceOf[Lambda[A,B]]
      case _ => !!!(s"Expected symbol of Lambda node but was $f", f)
    }
    def zip[C](g: Rep[A=>C]): Rep[A=>(B,C)] =
      fun { (x: Rep[A]) => Pair(f(x), g(x)) }
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
    val body = lam.schedule map { _.sym }
    val (t, _) = DefaultMirror.mirrorSymbols(new MapTransformer(subst ++ Map(x -> s)), NoRewriting, body)
    t(y).asRep[B]
  }

  //=====================================================================================
  //   Function reification

  def fun[A,B](f: Exp[A] => Exp[B])(implicit eA: LElem[A]): Exp[A=>B] = {
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
      case Some(TP(sym, Lambda(_, Some(f), _, _))) => {
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


  override def formatDef(d: Def[_]) = d match {
    case l: Lambda[_, _] =>
      s"\\\\${l.x} -> ${l.y match { case Def(b) => formatDef(b) case y => y}}"
    case Apply(f, x) => s"$f($x)"
    case _ => super.formatDef(d)
  }
}

