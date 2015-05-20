package scalan.effects

import scalan._
import scala.reflect.runtime.universe._
import scalan.monads.{MonadsDslExp, MonadsDslSeq, MonadsDsl}

trait FreeStates extends Base { self: MonadsDsl =>

  type RepStateF[S,A] = Rep[StateF[S,A]]
  trait StateF[S, A] extends Reifiable[StateF[S, A]] {
    implicit def eS: Elem[S]
    implicit def eA: Elem[A]
  }

  trait StateFCompanion {
    def unit[S: Elem, A: Elem](a: Rep[A]): Rep[FreeState[S,A]] = Done[({type f[x] = StateF[S,x]})#f, A](a)
//    def apply[S:Elem,A:Elem](r: Rep[S] => Rep[(A,S)]): Rep[FreeState[S,A]] = ???

    def get[S:Elem]: Rep[FreeState[S,S]] = {
      type F[x] = StateF[S, x]
      More[F, S](StateGet[S, FreeM[F,S]](fun { s: Rep[S] => Done[F, S](s) }))
    }

    def set[S:Elem](s: Rep[S]): Rep[FreeState[S,Unit]] = {
      type F[x] = StateF[S, x]
      More[F, Unit](StatePut(s, Done[F,Unit](())))
    }

    def eval[S: Elem, A: Elem](t: Rep[FreeState[S, A]], s: Rep[S]): Rep[A] = {
      type F[x] = StateF[S, x]
      val r: FreeState[S,A] = proxyFreeM[F, A](t)
      r.resume(statefFunctor).fold(
        l => l match {
          case Def(g: StateGet[S, FreeM[F,A]] @unchecked) => eval(g.f(s), s)
          case Def(p: StatePut[S, FreeM[F,A]] @unchecked) => eval(p.a, p.s)
          case _ => patternMatchError(l)
        },
        r => r)
    }
  }

  abstract class StateGet[S, A]
      (val f: Rep[S => A])
      (implicit val eS: Elem[S], val eA: Elem[A]) extends StateF[S, A] {
  }
  trait StateGetCompanion extends StateFCompanion

  abstract class StatePut[S, A]
    (val s: Rep[S], val a: Rep[A])
    (implicit val eS: Elem[S], val eA: Elem[A]) extends StateF[S, A] {
  }
  trait StatePutCompanion extends StateFCompanion

}


trait FreeStatesDsl extends impl.FreeStatesAbs { self: MonadsDsl =>

  type FreeState[S,A] = FreeM[({type f[x] = StateF[S,x]})#f, A]

  implicit def statefFunctor[S:Elem] = new Functor[({type λ[α] = StateF[S,α]})#λ] {
    def map[A:Elem,B:Elem](m: Rep[StateF[S, A]])(f: Rep[A] => Rep[B]) = m match {
      case Def(g: StateGet[_,_]) => StateGet((s: Rep[S]) => f(g.f.asRep[S=>A](s)))
      case Def(p: StatePut[_,_]) => StatePut(p.s.asRep[S], f(p.a.asRep[A]))
      case _ => patternMatchError(m)
    }
  }

  implicit def stateFCont[S:Elem]: Cont[({type f[x] = StateF[S,x]})#f] = new Container[({type f[x] = StateF[S,x]})#f] {
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[StateF[S,T]]
    def lift[T](implicit eT: Elem[T]) = element[StateF[S,T]]
  }

  class FreeStateManager[S:Elem] extends StateManager[S] {
    type F[A] = StateF[S,A]
    type State[A] = FreeState[S,A]
    def eS: Elem[S] = element[S]
    def unit[A:Elem](a: Rep[A]): Rep[State[A]] = StateF.unit(a)
//    def apply[A:Elem](r: Rep[S] => Rep[(A,S)]): Rep[State[A]] = State0.apply(r)
    def get: Rep[State[S]] = StateF.get[S]
    def set(s: Rep[S]): Rep[State[Unit]] = StateF.set(s)
    def eval[A:Elem](t: Rep[State[A]], s: Rep[S]): Rep[A] = StateF.eval(t, s)
    implicit val monad: Monad[State] = freeMMonad[F]//(freeMCont[F](stateFCont))
    implicit def stateElem[A:Elem]: Elem[State[A]] = freeMElement[F,A]
  }
}

trait FreeStatesDslSeq extends impl.FreeStatesSeq { self: MonadsDslSeq =>
}

trait FreeStatesDslExp extends impl.FreeStatesExp { self: MonadsDslExp =>
}
