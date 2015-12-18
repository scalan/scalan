package scalan.monads

import scalan._
import scala.reflect.runtime.universe._

trait FreeMs extends Base { self: MonadsDsl =>

  type RFreeM[F[_],A] = Rep[FreeM[F,A]]
  sealed trait FreeM[F[_], A] extends Def[FreeM[F,A]] {
    implicit def eA: Elem[A]
    implicit def cF: Cont[F]

    def flatMapBy[B:Elem](f: Rep[A => FreeM[F,B]]): Rep[FreeM[F,B]] = FlatMap(self, f)

    def mapBy[B:Elem](f: Rep[A => B]): Rep[FreeM[F,B]] =
      self.flatMap(a => Done(f(a)))

    def resume(implicit fF: Functor[F]): Rep[F[FreeM[F,A]] | A]

    private[FreeMs] def resumeFlatMap[B: Elem](f: Rep[A => FreeM[F,B]])(implicit fF: Functor[F]): Rep[F[FreeM[F,B]] | B]
  }
  trait FreeMCompanion

  implicit class FreeMExtensions[F[_], A](x: RFreeM[F, A]) {
    private implicit def eA = x.selfType1.asInstanceOf[FreeMElem[F, A, _]].eA
    private implicit def cF = x.selfType1.asInstanceOf[FreeMElem[F, A, _]].cF

    def flatMap[B:Elem](f: Rep[A] => Rep[FreeM[F,B]]): Rep[FreeM[F,B]] = x.flatMapBy(fun(f))

    def map[B:Elem](f: Rep[A] => Rep[B]): Rep[FreeM[F,B]] = x.mapBy(fun(f))
  }

  abstract class Done[F[_], A]
        (val a: Rep[A])
        (implicit val eA: Elem[A], val cF: Cont[F]) extends FreeM[F,A]
  {
    override def flatMapBy[B:Elem](f: Rep[A => FreeM[F,B]]): Rep[FreeM[F,B]] = f(a)
    def resume(implicit fF: Functor[F]): Rep[F[FreeM[F,A]] | A] = a.asRight[F[FreeM[F,A]]]
    private[FreeMs] def resumeFlatMap[B: Elem](f: Rep[A => FreeM[F,B]])(implicit fF: Functor[F]): Rep[F[FreeM[F,B]] | B] = f(a).resume
  }
  trait DoneCompanion

  abstract class More[F[_],A]
        (val k: Rep[F[FreeM[F,A]]])
        (implicit val eA: Elem[A], val cF: Cont[F]) extends FreeM[F, A] {

    def resume(implicit fF: Functor[F]): Rep[F[FreeM[F,A]] | A] = k.asLeft[A]
    private[FreeMs] def resumeFlatMap[B: Elem](f: Rep[A => FreeM[F,B]])(implicit fF: Functor[F]): Rep[F[FreeM[F,B]] | B] =
      fF.map(k)(fs => fs.flatMapBy(f)).asLeft[B]
  }
  trait MoreCompanion

  abstract class FlatMap[F[_],S,B]
        (val a: Rep[FreeM[F, S]], val f: Rep[S => FreeM[F,B]])
        (implicit val eS: Elem[S], val eA: Elem[B], val cF: Cont[F]) extends FreeM[F,B] {

    override def flatMapBy[R:Elem](f1: Rep[B => FreeM[F,R]]): Rep[FreeM[F,R]] =
      a.flatMap((s: Rep[S]) => f(s).flatMapBy(f1))

    def resume(implicit fF: Functor[F]): Rep[F[FreeM[F,B]] | B] = a.resumeFlatMap(f)

    private[FreeMs] def resumeFlatMap[C: Elem](g: Rep[B => FreeM[F,C]])(implicit fF: Functor[F]): Rep[F[FreeM[F,C]] | C] =
      a.flatMap((x: Rep[S]) => f(x) flatMapBy g).resume
  }
  trait FlatMapCompanion

}

trait FreeMsDsl extends ScalanDsl with impl.FreeMsAbs with FreeMs with Monads { self: MonadsDsl =>

  trait FreeMCont[F[_]] extends Cont[({type f[x] = FreeM[F,x]})#f] {
    implicit def cF: Cont[F]
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[FreeM[F,T]]
    def lift[T](implicit eT: Elem[T]) = element[FreeM[F,T]]
  }

  implicit def freeMMonad[F[_]:Cont]: FreeMCont[F] with Monad[({type f[a] = FreeM[F,a]})#f] =
    new FreeMCont[F] with Monad[({type f[a] = FreeM[F,a]})#f] {
      def cF = container[F]
      // suppress implicit resolution of this method (as it leads to stackoverflow)
      override def toMonadic[A: Elem](a: Rep[FreeM[F, A]]) = super.toMonadic(a)
      def unit[A:Elem](a: Rep[A]) = Done(a)
      override def flatMap[A:Elem,B:Elem](fa: Rep[FreeM[F, A]])(f: Rep[A] => Rep[FreeM[F, B]]) = fa flatMap f
    }

}


trait FreeMsDslSeq extends ScalanDslSeq with impl.FreeMsSeq { self: MonadsDslSeq =>

}

trait FreeMsDslExp extends ScalanDslExp with impl.FreeMsExp { self: MonadsDslExp =>


}