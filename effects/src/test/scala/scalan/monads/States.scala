package scalan.monads

import scalan._
import scala.reflect.runtime.universe._

trait States extends Base { self: MonadsDsl =>

  type RepState[S,A] = Rep[State0[S,A]]
  trait State0[S, A] extends Def[State0[S, A]] {
    implicit def eS: Elem[S]
    implicit def eA: Elem[A]
    def run: Rep[S => (A, S)]
  }
  trait State0Companion {
    def unit[S: Elem, A: Elem](a: Rep[A]) = StateBase(fun {s: Rep[S] => (a,s)})
//    def apply[S:Elem,A:Elem](r: Rep[S] => Rep[(A,S)]): Rep[State0[S,A]] = StateBase(fun(r))
    def get[S:Elem]: Rep[State0[S,S]] = StateBase(fun { s => (s,s) })
    def set[S:Elem](s: Rep[S]): Rep[State0[S,Unit]] = StateBase(fun { _ => Pair((),s) })
    def run[S: Elem, A: Elem](t: Rep[State0[S, A]], s: Rep[S]): Rep[(A, S)] = t.run(s)
    def eval[S: Elem, A: Elem](t: Rep[State0[S, A]], s: Rep[S]): Rep[A] = run(t,s)._1
  }

  abstract class StateBase[S, A]
      (val run: Rep[S => (A,S)])
      (implicit val eS: Elem[S], val eA: Elem[A]) extends State0[S, A] {
  }
  trait StateBaseCompanion extends State0Companion
}


trait StatesDsl extends impl.StatesAbs { self: MonadsDsl =>

  trait StateManager[S] {
    type State[A]
    def eS: Elem[S]
    implicit def stateElem[A:Elem]: Elem[State[A]]
    def unit[A:Elem](a: Rep[A]): Rep[State[A]]
    def get: Rep[State[S]]
    def set(s: Rep[S]): Rep[State[Unit]]
    def eval[A:Elem](t: Rep[State[A]], s: Rep[S]): Rep[A]
    implicit val monad: Monad[State]
  }

  trait State0Cont[S] extends Cont[({type f[x] = State0[S,x]})#f] {
    implicit def eS: Elem[S]
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[State0[S,T]]
    def lift[T](implicit eT: Elem[T]) = element[State0[S,T]]
    def unlift[T](implicit eFT: Elem[State0[S, T]]) = eFT.asInstanceOf[State0Elem[S,T,_]].eA
    def getElem[T](fa: Rep[State0[S, T]]) = fa.selfType1
    def unapply[T](e: Elem[_]) = e match {
      case te: State0Elem[_, _, _] => Some(te.asElem[State0[S,T]])
      case _ => None
    }
  }

  implicit def state0Monad[S:Elem]: Monad[({type f[x] = State0[S,x]})#f] = new State0Cont[S] with Monad[({type f[x] = State0[S,x]})#f] {
    def eS = element[S]
    def unit[A:Elem](a: Rep[A]): Rep[State0[S, A]] = State0.unit(a)
    override def flatMap[A:Elem,B:Elem](st: Rep[State0[S, A]])(f: Rep[A] => Rep[State0[S, B]]): Rep[State0[S, B]] =
      StateBase(fun { (s: Rep[S]) =>
        val Pair(a, s1) = st.run(s)
        val st2 = f(a)
        st2.run(s1)
      })
  }

  class State0Manager[S:Elem] extends StateManager[S] {
    type State[A] = State0[S,A]
    def eS: Elem[S] = element[S]
    def unit[A:Elem](a: Rep[A]): Rep[State[A]] = State0.unit(a)
//    def apply[A:Elem](r: Rep[S] => Rep[(A,S)]): Rep[State[A]] = State0(fun (r))
    def get: Rep[State[S]] = State0.get[S]
    def set(s: Rep[S]): Rep[State[Unit]] = State0.set(s)
    def eval[A:Elem](t: Rep[State[A]], s: Rep[S]): Rep[A] = State0.eval(t, s)
    implicit val monad: Monad[State] = state0Monad[S]
    implicit def stateElem[A:Elem]: Elem[State[A]] = state0Element
  }
}

trait StatesDslStd extends impl.StatesStd { self: MonadsDslStd =>
}

trait StatesDslExp extends impl.StatesExp { self: MonadsDslExp =>
  override def rewriteDef[T](d: Def[T]) = d match {
    case State0CompanionMethods.eval(t: Rep[State0[s,a]], state) =>
      t.run(state.asRep[s])._1
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
            step(Pair(StateBase(fun { _ => Pair(a,s) }), t)).run(s)
          }}
      }
    case Apply(Def(State0Methods.run(Def(CollectionMethods.foldLeft(xs, start: RepState[s,a] @unchecked, f)))), s0) =>
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
            step(Pair(StateBase(fun { _ => Pair(a,s) }), t)).run(s)
          }})
      }
    case _ => super.rewriteDef(d)
  }
}
