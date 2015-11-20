package scalan.monads

import scalan._
import scala.reflect.runtime.universe._

trait Frees extends Base { self: MonadsDsl =>

  type RFree[F[_],A] = Rep[Free[F,A]]
  sealed trait Free[F[_], A] extends Def[Free[F,A]] {
    implicit def eA: Elem[A]
    implicit def cF: Cont[F]

    def flatMap[B:Elem](f: Rep[A] => Rep[Free[F,B]]): Rep[Free[F,B]] = flatMapBy(fun(f))
    def flatMapBy[B:Elem](f: Rep[A => Free[F,B]]): Rep[Free[F,B]] = Bind(self, f)

    def mapBy[B:Elem](f: Rep[A => B]): Rep[Free[F,B]] =
      flatMap(a => Return(f(a)))

    def map[B:Elem](f: Rep[A] => Rep[B]): Rep[Free[F,B]] = mapBy(fun(f))

    def foldMap[G[_]:Monad](f: F ~> G): Rep[G[A]]
    def run[G[_]:Monad](f: F ~> G): Rep[G[A]] = step.foldMap(f)
    def step: Rep[Free[F,A]] = self   // for Return and Suspend
    def resume(implicit fF: Functor[F]): Rep[F[Free[F,A]] | A]
//    def resume(implicit fF: Functor[F]): Rep[F[A] | A]
//    =
//      self.selfType1.asInstanceOf[FreeElem[F,_,_]] match {
//          case r: ReturnElem[F,A] => self.asRep[Return[F,A]].a.asRight[F[A]]
//          case s: SuspendElem[F,A] => self.asRep[Suspend[F,A]].a.asLeft[A]
//          case b: BindElem[F,s,A] =>
//            val b = seld.asRep[Bind[F,s,A]]
//
//      }
  }
  trait FreeCompanion

  abstract class Return[F[_],A]
        (val a: Rep[A])
        (implicit val eA: Elem[A], val cF: Cont[F]) extends Free[F,A]
  {
    override def flatMap[B:Elem](f: Rep[A] => Rep[Free[F,B]]): Rep[Free[F,B]] = f(a)
    override def flatMapBy[B:Elem](f: Rep[A => Free[F,B]]): Rep[Free[F,B]] = f(a)

    def foldMap[G[_]:Monad](f: F ~> G): Rep[G[A]] = Monad[G].unit(a)
    def resume(implicit fF: Functor[F]): Rep[F[Free[F,A]] | A] = a.asRight[F[Free[F,A]]]
  }
  trait ReturnCompanion

  abstract class Suspend[F[_],A]
        (val a: Rep[F[A]])
        (implicit val eA: Elem[A], val cF: Cont[F]) extends Free[F, A] {

    def foldMap[G[_]:Monad](trans: F ~> G): Rep[G[A]] = trans(a)

    def resume(implicit fF: Functor[F]): Rep[F[Free[F,A]] | A] = {
      val res: Rep[F[Free[F,A]]] = fF.map(a)((a: Rep[A]) => Return(a))
      res.asLeft[A]
    }
  }
  trait SuspendCompanion

  abstract class Bind[F[_],S,B]
        (val a: Rep[Free[F, S]], val f: Rep[S => Free[F,B]])
        (implicit val eS: Elem[S], val eA: Elem[B], val cF: Cont[F]) extends Free[F,B] {

    override def flatMap[R:Elem](f1: Rep[B] => Rep[Free[F,R]]): Rep[Free[F,R]] =
      a.flatMap((s: Rep[S]) => f(s).flatMap(f1))
    override def flatMapBy[R:Elem](f1: Rep[B => Free[F,R]]): Rep[Free[F,R]] =
      a.flatMap((s: Rep[S]) => f(s).flatMapBy(f1))

    def foldMap[G[_]:Monad](trans: F ~> G): Rep[G[B]] = a match {
      case Def(susp: Suspend[F,S]) =>
        Monad[G].flatMap(trans(susp.a)) { s =>
          f(s).run(trans)
        }
      case Def(d) => !!!(s"Should be eliminated by method step: Unexpected $this where symbol $a of type ${a.selfType1} has def $d")
      case _ => !!!(s"Should be eliminated by method step: Unexpected $this where symbol $a of type ${a.selfType1}")
    }
//      Monad[G].flatMap(trans(a)) { s =>
//        f(s).foldMap(trans)
//      }
    override lazy val step: Rep[Free[F,B]] = a match {
      case Def(b: Bind[F,s,S]) => b.a.flatMap((a: Rep[s]) => b.f(a).flatMapBy(f)).step
      case Def(ret: Return[F,S]) => f(ret.a).step
      case _ => self
    }

    def resume(implicit fF: Functor[F]): Rep[F[Free[F,B]] | B] = a.selfType1.asInstanceOf[Elem[_]] match {
      case r: ReturnElem[F,S] => f(a.asRep[Return[F,S]].a).resume
      case s: SuspendElem[F,S] => fF.map(a.asRep[Suspend[F,S]].a)(s => f(s)).asLeft[B]
      case b: BindElem[F,s,S] =>
        val b = self.asRep[Bind[F,s,S]]
        b.a.flatMap(s => b.f(s).flatMapBy(f)).resume
    }
  }
  trait BindCompanion

}

trait FreesDsl extends ScalanDsl with impl.FreesAbs with Frees with Monads { self: MonadsDsl =>

  trait FreeCont[F[_]] extends Cont[({type f[x] = Free[F,x]})#f] {
    implicit def cF: Cont[F]
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[Free[F,T]]
    def lift[T](implicit eT: Elem[T]) = element[Free[F,T]]
  }

  def freeMonad[F[_]:Cont]: Monad[({type f[a] = Free[F,a]})#f] =
    new Monad[({type f[a] = Free[F,a]})#f] with FreeCont[F] {
      def cF = container[F]
      // suppress implicit resolution of this method (as it leads to stackoverflow)
      override def toMonadic[A: Elem](a: Rep[Free[F, A]]) = super.toMonadic(a)

      def unit[A:Elem](a: Rep[A]) = Return(a)
      override def flatMap[A:Elem,B:Elem](fa: Rep[Free[F, A]])(f: Rep[A] => Rep[Free[F, B]]) = fa flatMap f
    }

}


trait FreesDslSeq extends FreesDsl with impl.FreesSeq with ScalanCtxSeq  { self: MonadsDslSeq =>

}

trait FreesDslExp extends FreesDsl with impl.FreesExp with ScalanExp { self: MonadsDslExp =>


}