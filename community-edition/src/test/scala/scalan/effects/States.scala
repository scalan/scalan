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
    def apply[S:Elem,A:Elem](r: Rep[S] => Rep[(A,S)]) = StateBase(fun(r))
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
  override def rewriteDef[T](d: Def[T]) = d match {
    case Apply(Def(StateMethods.run(Def(ArrayFold(xs, start: RepState[s,a], f)))), s0) =>
      xs.elem match {
        case el: ArrayElem[t] =>
          implicit val eT = el.eItem
          val st = start.asRep[State[s,a]]
          val step = f.asRep[((State[s,a],t)) => State[s,a]]
          val initState = s0.asRep[s]
          implicit val eS = initState.elem
          val init = st.run(initState)
          implicit val eA = init.elem.eFst
          xs.asRep[Array[t]].foldLeft(init){p: Rep[((a,s),t)] => {
            val Pair(Pair(a,s), t) = p
            step(Pair(State({ _ => Pair(a,s) }), t)).run(s)
          }}
      }
    case _ => super.rewriteDef(d)
  }
}
