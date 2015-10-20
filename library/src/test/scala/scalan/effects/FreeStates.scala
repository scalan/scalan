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

    def get[S:Elem]: Rep[FreeState[S,S]] = {
      type F[x] = StateF[S, x]
      More[F, S](StateGet[S, FreeM[F,S]](fun { s: Rep[S] => Done[F, S](s) }))
    }

    def set[S:Elem](s: Rep[S]): Rep[FreeState[S,Unit]] = {
      type F[x] = StateF[S, x]
      More[F, Unit](StatePut(s, Done[F,Unit](())))
    }

    def run[S: Elem, A: Elem](t: Rep[FreeState[S, A]], s: Rep[S]): Rep[(A, S)] = {
      implicit val freeStateElement: Elem[FreeState[S, A]] =
        freeMElement[({type f[x] = StateF[S,x]})#f, A](stateFFunctor[S], element[A])

      lazy val runFun: Rep[((FreeState[S, A], S)) => (A, S)] = funRec[(FreeState[S, A], S), (A, S)] { (f: Rep[((FreeState[S, A], S)) => (A, S)]) =>
        val f1 = { g: Rep[StateGet[S, FreeState[S, A]]] => f((g.f(s), s))}
        val f2 = { p: Rep[StatePut[S, FreeState[S, A]]] => f((p.a, p.s))}
        { p: Rep[(FreeState[S, A], S)] =>
          val Pair(t, s) = p
          t.resume(stateFFunctor).fold(
            //        l => MATCH(l) {
            //          case Def(g: StateGet[S, FreeState[S, A]] @unchecked) => run(g.f(s), s)
            //          case Def(p: StatePut[S, FreeState[S, A]] @unchecked) => run(p.a, p.s)
            //          case _ => patternMatchError(l)
            //        },
            l => patternMatch(l)(
              MkBranch[StateGet[S, FreeState[S, A]]].make(f1),
              MkBranch[StatePut[S, FreeState[S, A]]].make(f2)
            )(None),
            r => (r, s))
        }
      }

      runFun((t, s))
    }

  }

  implicit class StateFCompanionExtensions(c: Rep[StateFCompanion]) {
    def eval[S: Elem, A: Elem](t: Rep[FreeState[S, A]], s: Rep[S]): Rep[A] =
      c.run(t,s)._1
  }

  implicit class FreeStateExtensions[S: Elem, A: Elem](t: Rep[FreeState[S, A]]) {
    def run(s: Rep[S]): Rep[(A, S)] = {
      StateF.run(t, s)
//      implicit val freeStateElement: Elem[FreeState[S, A]] =
//        freeMElement[({type f[x] = StateF[S,x]})#f, A](stateFCont[S], element[A])
//
//      t.resume(statefFunctor).fold(
//        //        l => MATCH(l) {
//        //          case Def(g: StateGet[S, FreeState[S, A]] @unchecked) => run(g.f(s), s)
//        //          case Def(p: StatePut[S, FreeState[S, A]] @unchecked) => run(p.a, p.s)
//        //          case _ => patternMatchError(l)
//        //        },
//        l => patternMatch(l, List(
//          (classOf[StateGet[_, _]], fun { g: Rep[StateGet[S, FreeState[S, A]]] @unchecked => g.f(s).run(s)}),
//          (classOf[StatePut[_, _]], fun { p: Rep[StatePut[S, FreeState[S, A]]] @unchecked => p.a.run(p.s)})
//        ), None),
//        r => (r, s))
    }
  }

  implicit def proxyFreeState[S, A](t: Rep[FreeState[S, A]]): FreeState[S, A] = {
    type F[x] = StateF[S, x]
    proxyFreeM[F, A](t)
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

  implicit def stateFFunctor[S: Elem]: Functor[({type λ[α] = StateF[S,α]})#λ] = new Functor[({type λ[α] = StateF[S,α]})#λ] {
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[StateF[S,T]]
    def lift[T](implicit eT: Elem[T]) = element[StateF[S,T]]
    def map[A:Elem,B:Elem](m: Rep[StateF[S, A]])(f: Rep[A] => Rep[B]) = patternMatch(m)(
      MkBranch[StateGet[S, A]].make { g => StateGet((s: Rep[S]) => f(g.f.asRep[S=>A](s))) },
      MkBranch[StatePut[S, A]].make { p => StatePut(p.s.asRep[S], f(p.a.asRep[A])) }
    )(None)
  }

  class FreeStateManager[S:Elem] extends StateManager[S] {
    type F[A] = StateF[S,A]
    type State[A] = FreeState[S,A]
    def eS: Elem[S] = element[S]
    def unit[A:Elem](a: Rep[A]): Rep[State[A]] = StateF.unit(a)
    def get: Rep[State[S]] = StateF.get[S]
    def set(s: Rep[S]): Rep[State[Unit]] = StateF.set(s)
    def eval[A:Elem](t: Rep[State[A]], s: Rep[S]): Rep[A] = StateF.eval(t, s)
    implicit val monad: Monad[State] = freeMMonad[F]
    implicit def stateElem[A:Elem]: Elem[State[A]] = freeMElement[F,A]
  }
}

trait FreeStatesDslSeq extends impl.FreeStatesSeq { self: MonadsDslSeq =>
}

trait FreeStatesDslExp extends impl.FreeStatesExp { self: MonadsDslExp =>
  override def rewriteDef[T](d: Def[T]) = d match {
    // Rule: xs.fold(init,f).run(s0) ==> xs.fold(init.run(s0), ((a,s),t) => f(State(_ => (a,s)),t).run(s))
    case StateFCompanionMethods.run(Def(ArrayFold(xs, start: Rep[FreeState[s,a]] @unchecked, f)), s0) =>
      xs.elem match {
        case el: ArrayElem[t] =>
          implicit val eT = el.eItem
          val st = start.asRep[FreeState[s,a]]
          val step = f.asRep[((FreeState[s,a],t)) => FreeState[s,a]]
          val initState = s0.asRep[s]
          implicit val eS = initState.elem
          implicit val eA = st.elem.asInstanceOf[FreeMElem[({type f[x] = StateF[s,x]})#f, a, _]].eA
          val init = st.run(initState)
          xs.asRep[Array[t]].foldLeft(init){p: Rep[((a,s),t)] =>
            val Pair(Pair(a,s), t) = p
            val state1 = StateF.set(s).mapBy(constFun(a))
            step(Pair(state1, t)).run(s)
          }
      }
    case StateFCompanionMethods.run(Def(CollectionMethods.foldLeft(xs, start: Rep[FreeState[s,a]] @unchecked, f)), s0) =>
      xs.elem match {
        case el: CollectionElem[t,_] =>
          implicit val eT = el.eItem
          val st = start.asRep[FreeState[s,a]]
          val step = f.asRep[((FreeState[s,a],t)) => FreeState[s,a]]
          val initState = s0.asRep[s]
          implicit val eS = initState.elem
          implicit val eA = st.elem.asInstanceOf[FreeMElem[({type f[x] = StateF[s,x]})#f, a, _]].eA
          val init = st.run(initState)
          xs.asRep[Collection[t]].foldLeft(init, {p: Rep[((a,s),t)] =>
            val Pair(Pair(a,s), t) = p
            val state1 = StateF.set(s).mapBy(constFun(a))
            step(Pair(state1, t)).run(s)
          })
      }
    //case StateFCompanionMethods.run()
    case _ => super.rewriteDef(d)
  }
}
