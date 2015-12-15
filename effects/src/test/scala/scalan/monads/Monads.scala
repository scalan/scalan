package scalan.monads

import scalan.collections.ListOps
import scalan._
import scala.reflect.runtime.universe._

/**
 * Created by slesarenko on 05/01/15.
 */
import language.higherKinds // Disable warnings for type constructor polymorphism

trait Monads extends Base with ListOps { self: MonadsDsl =>

  trait Monad[F[_]] extends Functor[F] {
    implicit def thisCont: Cont[F] = this
    def unit[A:Elem](a: Rep[A]): Rep[F[A]]

    def flatMap[A:Elem,B:Elem](ma: Rep[F[A]])(f: Rep[A] => Rep[F[B]]): Rep[F[B]] =
      join(map(ma)(f))

    def map[A:Elem,B:Elem](ma: Rep[F[A]])(f: Rep[A] => Rep[B]): Rep[F[B]] =
      flatMap(ma)(a => unit(f(a)))

    def map2[A:Elem,B:Elem,C:Elem](ma: Rep[F[A]], mb: Rep[F[B]])(f: (Rep[A], Rep[B]) => Rep[C]): Rep[F[C]] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))

    def sequence[A:Elem](lma: Rep[List[F[A]]]): Rep[F[List[A]]] =
      lma.foldRight[F[List[A]]](unit(SList.empty[A])) { (p: Rep[(F[A],F[List[A]])]) =>
        val Pair(ma, mla) = p
        map2(ma, mla)(_ :: _)
      }

    def traverse[A:Elem,B:Elem](la: Lst[A])(f: Rep[A] => Rep[F[B]]): Rep[F[List[B]]] =
      la.foldRight[F[List[B]]](unit(SList.empty[B])){ (in: Rep[(A,F[List[B]])]) =>
        val Pair(a, mlb) = in
        map2(f(a), mlb)(_ :: _)
      }

//    // For `List`, the `replicateM` function will generate a list of lists.
//    // It will contain all the lists of length `n` with elements selected from the
//    // input list.
//    // For `Option`, it will generate either `Some` or `None` based on whether the
//    // input is `Some` or `None`. The `Some` case will contain a list of length `n`
//    // that repeats the element in the input `Option`.
//    // The general meaning of `replicateM` is described very well by the
//    // implementation `sequence(List.fill(n)(ma))`. It repeats the `ma` monadic value
//    // `n` times and gathers the results in a single value, where the monad `M`
//    // determines how values are actually combined.
//
//    // Recursive version:
//    def _replicateM[A](n: Int, ma: F[A]): F[List[A]] =
//      if (n <= 0) unit(List[A]()) else map2(ma, replicateM(n - 1, ma))(_ :: _)
//
    // Using `sequence` and the `List.fill` function of the standard library:
    def replicateM[A:Elem](n: Rep[Int])(ma: Rep[F[A]]): Rep[F[List[A]]] =
      sequence(SList.replicate(n, ma))

    def replicateM_[A:Elem](n: Rep[Int])(f: Rep[F[A]]): Rep[F[Unit]] =
      foreachM(SList.replicate(n, f))(skip)

    def foldM[A:Elem,B:Elem](la: Lst[A])(z: Rep[B])(f: (Rep[B],Rep[A]) => Rep[F[B]]): Rep[F[B]] =
      la.foldLeft[F[B]](unit(z)){ (in: Rep[(F[B],A)]) =>
        val Pair(fb, a) = in
        flatMap(fb)(b => f(b,a))
      }

    def foldM_[A:Elem,B:Elem](l: Lst[A])(z: Rep[B])(f: (Rep[B],Rep[A]) => Rep[F[B]]): Rep[F[Unit]] =
      skip { foldM(l)(z)(f) }

    def foreachM[A:Elem](l: Rep[List[A]])(f: Rep[A] => Rep[F[Unit]]): Rep[F[Unit]] =
      foldM_(l)(())((u,a) => skip(f(a)))
    def seq[A:Elem,B:Elem,C:Elem](f: Rep[A] => Rep[F[B]], g: Rep[B] => Rep[F[C]]): Rep[A => F[C]] =
      compose(f, g)

    def as[A:Elem,B:Elem](a: Rep[F[A]])(b: Rep[B]): Rep[F[B]] = map(a)(_ => b)
    def skip[A:Elem](a: Rep[F[A]]): Rep[F[Unit]] = as(a)(())
    def when[A:Elem](b: Rep[Boolean])(fa: => Rep[F[A]]): Rep[F[Boolean]] =
      IF (b) { as(fa)(true) } ELSE { unit(false) }

//    def forever[A,B](a: F[A]): F[B] = {
//      lazy val t: F[B] = forever(a)
//      a flatMap (_ => t)
//    }
//    def while_(a: F[Boolean])(b: F[Unit]): F[Unit] = {
//      lazy val t: F[Unit] = while_(a)(b)
//      a flatMap (c => skip(when(c)(t)))
//    }
//    def doWhile[A:Elem](a: Rep[F[A]])(cond: Rep[A] => Rep[F[Boolean]]): Rep[F[Unit]] = for {
//      a1 <- a
//      ok <- cond(a1)
//      _ <- IF (ok) { doWhile(a)(cond) } else unit(())
//    } yield ()


    def compose[A:Elem,B:Elem,C:Elem](f: Rep[A] => Rep[F[B]], g: Rep[B] => Rep[F[C]]): Rep[A => F[C]] =
      fun {a => flatMap(f(a))(g)}

//    def _flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] =
//      compose((_:Unit) => ma, f)(())

    def join[A:Elem](mma: Rep[F[F[A]]]): Rep[F[A]] = flatMap(mma)(ma => ma)

    def filterM[A:Elem](ms: Lst[A])(f: Rep[A] => Rep[F[Boolean]]): Rep[F[List[A]]] =
      ms.foldRight(unit(SList.empty[A])){ (in: Rep[(A,F[List[A]])]) =>
        val Pair(x, y) = in
        val h = compose(f, (b: Rep[Boolean]) => IF (b) THEN {map2(unit(x),y)(_ :: _)} ELSE { y })
        h(x)
      }

    override def toString = s"Monad[${name}}]"

    // syntax
    implicit def toMonadic[A:Elem](a: Rep[F[A]]): Monadic[F,A] =
      new Monadic[F,A] { val F = Monad.this; val eA = element[A]; def get = a }
  }

  trait Monadic[F[_],A] {
    val F: Monad[F]
    implicit val eA: Elem[A]
    import F._
    def get: Rep[F[A]]
    private val a = get
    def map[B:Elem](f: Rep[A] => Rep[B]): Rep[F[B]] = F.map(a)(f)
    def flatMap[B:Elem](f: Rep[A] => Rep[F[B]]): Rep[F[B]] = F.flatMap(a)(f)
    def **[B:Elem](b: Rep[F[B]]) = F.map2(a,b)(Pair(_,_))
    def *>[B:Elem](b: Rep[F[B]]) = F.map2(a,b)((_,b) => b)
    def map2[B:Elem,C:Elem](b: Rep[F[B]])(f: (Rep[A],Rep[B]) => Rep[C]): Rep[F[C]] = F.map2(a,b)(f)
    def as[B:Elem](b: Rep[B]): Rep[F[B]] = F.as(a)(b)
    def skip: Rep[F[Unit]] = F.skip(a)
    def replicateM(n: Int) = F.replicateM(n)(a)
    def replicateM_(n: Int) = F.replicateM_(n)(a)
    //def withFilter(p: Rep[A] => Rep[Boolean]): Rep[F[A]] = a
  }

  object Monad {
    def apply[F[_]:Monad]: Monad[F] = implicitly[Monad[F]]
  }

  type Id[A] = A

  trait IdCont extends Cont[Id] {
    def tag[T](implicit tT: WeakTypeTag[T]) = tT
    def lift[T](implicit eT: Elem[T]) = eT
  }

  implicit val identityMonad: Monad[Id] = new Monad[Id] with IdCont {
    def unit[A:Elem](a: Rep[A]) = a
    override def flatMap[A:Elem,B:Elem](a: Rep[A])(f: Rep[A] => Rep[B]) = f(a)
  }

  type Oper[A] = Int => (Int, A)

  trait OperCont extends Cont[Oper] {
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[Oper[T]]
    def lift[T](implicit eT: Elem[T]) = funcElement(element[Int], element[(Int,T)])
  }

  implicit val operationMonad: Monad[Oper] = new Monad[Oper] with OperCont {
    def unit[A:Elem](a: Rep[A]) = fun {i => (i, a)}

    override def flatMap[A:Elem,B:Elem](op: Rep[Oper[A]])(f: Rep[A] => Rep[Oper[B]]) =
      fun { (i: Rep[Int]) =>
        val Pair(i1, a) = op(i)
        val op1 = f(a)
        op1(i1)
      }
  }


  object IdOper extends (Id ~> Oper) {
    def cIn = container[Id]
    def cOut = container[Oper]
    def apply[A:Elem](i: Rep[Id[A]]): Rep[Oper[A]] = eval(i)(element[A])
  }

  object OperOper extends (Oper ~> Oper) {
    def cIn = container[Oper]
    def cOut = container[Oper]
    def apply[A:Elem](i: Rep[Oper[A]]): Rep[Oper[A]] = i
  }

}

trait MonadsDsl extends ScalanDsl with Monads
  with FreesDsl
  with CoproductsDsl
  with ReadersDsl
  with StatesDsl
  with FreeStatesDsl
  with FreeMsDsl
{
  def eval[A:Elem](v: Rep[A]): Rep[Oper[A]] = fun {i => console_eval(i,v)}
}

trait MonadsDslSeq extends ScalanDslSeq
  with MonadsDsl
  with FreesDslSeq
  with CoproductsDslSeq
  with ReadersDslSeq
  with StatesDslSeq
  with FreeStatesDslSeq
  with FreeMsDslSeq
{
}

trait MonadsDslExp extends ScalanDslExp
  with MonadsDsl
  with FreesDslExp
  with CoproductsDslExp
  with ReadersDslExp
  with StatesDslExp
  with FreeStatesDslExp
  with FreeMsDslExp
{
}
