package scalan.monads

import scalan._
import scala.reflect.runtime.universe._

trait States extends Base { self: MonadsDsl =>

  type RepState[S,A] = Rep[State[S,A]]
  trait State[S, A] extends Reifiable[State[S, A]] {
    implicit def eS: Elem[S]
    implicit def eA: Elem[A]
//    def map[B](f: A => B): State[S, B] =
//      flatMap(a => unit(f(a)))
//    def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
//      flatMap(a => sb.map(b => f(a, b)))
//    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
//      val (a, s1) = run(s)
//      f(a).run(s1)
//    })
    def run: Rep[S => (A, S)]
  }
  trait StateCompanion {
//    def apply[S,A]()
    def get[S:Elem]: Rep[State[S,S]] = StateBase(fun { s => (s,s) })
    def set[S:Elem](s: Rep[S]): Rep[State[S,Unit]] = StateBase(fun { _ => Pair((),s) })
  }
  

  abstract class StateBase[S, A]
      (val run: Rep[S => (A,S)])
      (implicit val eS: Elem[S], val eA: Elem[A]) extends State[S, A] {
  }
  trait StateBaseCompanion
}


trait StatesDsl extends impl.StatesAbs { self: MonadsDsl =>

  implicit def stateCont[S:Elem]: Cont[({type f[x] = State[S,x]})#f] = new Container[({type f[x] = State[S,x]})#f] {
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[State[S,T]]
    def lift[T](implicit eT: Elem[T]) = element[State[S,T]]
  }

  implicit def stateMonad[S:Elem]: Monad[({type f[x] = State[S,x]})#f] = new Monad[({type f[x] = State[S,x]})#f] {
    def cF = stateCont
    def unit[A:Elem](a: Rep[A]): Rep[State[S, A]] = {
      StateBase(fun {s => (a,s)})
    }
    override def flatMap[A:Elem,B:Elem](st: Rep[State[S, A]])(f: Rep[A] => Rep[State[S, B]]): Rep[State[S, B]] =
      StateBase(fun { (s: Rep[S]) =>
        val Pair(a, s1) = st.run(s)
        val st2 = f(a)
        st2.run(s1)
      })
  }
}

trait StatesDslSeq extends impl.StatesSeq { self: MonadsDslSeq =>
}

trait StatesDslExp extends impl.StatesExp { self: MonadsDslExp =>
}
