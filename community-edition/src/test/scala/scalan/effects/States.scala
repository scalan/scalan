package scalan.monads

import scalan._
import scala.reflect.runtime.universe._

trait States extends Base { self: MonadsDsl =>

  type RepState[S,A] = Rep[State0[S,A]]
  trait State0[S, A] extends Reifiable[State0[S, A]] {
    implicit def eS: Elem[S]
    implicit def eA: Elem[A]
    def run: Rep[S => (A, S)]
  }
  trait State0Companion extends StateManager {
    type State[S,A] = State0[S,A]
    def apply[S:Elem,A:Elem](r: Rep[S] => Rep[(A,S)]): Rep[State[S,A]] = StateBase(fun(r))
    def get[S:Elem]: Rep[State[S,S]] = apply { s => (s,s) }
    def set[S:Elem](s: Rep[S]): Rep[State[S,Unit]] = apply { _ => Pair((),s) }
    def run[S: Elem, A: Elem](s: Rep[S], t: Rep[State[S, A]]) = t.run(s)._1
  }

  abstract class StateBase[S, A]
      (val run: Rep[S => (A,S)])
      (implicit val eS: Elem[S], val eA: Elem[A]) extends State0[S, A] {
  }
  trait StateBaseCompanion extends State0Companion

//  abstract class LoggingState[L, S, A]
//      (val logginRun: Rep[(L,S) => (A,(L,S))])
//      (implicit val eL: Elem[L], val eS: Elem[S], val eA: Elem[A]) extends State[S, A] {
//    def run = fun { s: Rep[S] => }
//  }
//  trait LoggingStateCompanion extends StateCompanion

}


trait StatesDsl extends impl.StatesAbs { self: MonadsDsl =>

  trait StateManager {
    type State[S,A]
    def apply[S:Elem,A:Elem](r: Rep[S] => Rep[(A,S)]): Rep[State[S,A]]
    def get[S:Elem]: Rep[State[S,S]]
    def set[S:Elem](s: Rep[S]): Rep[State[S,Unit]]
    def run[S:Elem,A:Elem](s: Rep[S], t: Rep[State[S,A]]): Rep[A]
  }

  implicit def state0Cont[S:Elem]: Cont[({type f[x] = State0[S,x]})#f] = new Container[({type f[x] = State0[S,x]})#f] {
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[State0[S,T]]
    def lift[T](implicit eT: Elem[T]) = element[State0[S,T]]
  }

  implicit def state0Monad[S:Elem]: Monad[({type f[x] = State0[S,x]})#f] = new Monad[({type f[x] = State0[S,x]})#f] {
    def cF = state0Cont
    def unit[A:Elem](a: Rep[A]): Rep[State0[S, A]] = {
      StateBase(fun {s: Rep[S] => (a,s)})
    }
    override def flatMap[A:Elem,B:Elem](st: Rep[State0[S, A]])(f: Rep[A] => Rep[State0[S, B]]): Rep[State0[S, B]] =
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
    // Rule: xs.fold(init,f).run(s0) ==> xs.fold(init.run(s0), ((a,s),t) => f(State(_ => (a,s)),t).run(s))
    case Apply(Def(State0Methods.run(Def(ArrayFold(xs, start: RepState[s,a], f)))), s0) =>
      xs.elem match {
        case el: ArrayElem[t] =>
          implicit val eT = el.eItem
          val st = start.asRep[State0[s,a]]
          val step = f.asRep[((State0[s,a],t)) => State0[s,a]]
          val initState = s0.asRep[s]
          implicit val eS = initState.elem
          val init = st.run(initState)
          implicit val eA = init.elem.eFst
          xs.asRep[Array[t]].foldLeft(init){p: Rep[((a,s),t)] => {
            val Pair(Pair(a,s), t) = p
            step(Pair(State0({ _ => Pair(a,s) }), t)).run(s)
          }}
      }
    case Apply(Def(State0Methods.run(Def(CollectionMethods.foldLeft(xs, start: RepState[s,a], f)))), s0) =>
      xs.elem match {
        case el: CollectionElem[t,_] =>
          implicit val eT = el.eItem
          val st = start.asRep[State0[s,a]]
          val step = f.asRep[((State0[s,a],t)) => State0[s,a]]
          val initState = s0.asRep[s]
          implicit val eS = initState.elem
          val init = st.run(initState)
          implicit val eA = init.elem.eFst
          xs.asRep[Collection[t]].foldLeft(init, {p: Rep[((a,s),t)] => {
            val Pair(Pair(a,s), t) = p
            step(Pair(State0({ _ => Pair(a,s) }), t)).run(s)
          }})
      }
    case _ => super.rewriteDef(d)
  }
}
