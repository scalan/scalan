package scalan.stream

import scalan._
import scala.reflect.runtime.universe._
import scalan.monads.{MonadsDsl, Monads}

trait Processes extends Base with Containers { self: ProcessesDsl =>

  type RProc[F[_],O] = Rep[Process[F,O]]
  type IO[A] = Oper[A]

  implicit def defaultProcessElem[F[_]:Cont, O:Elem]: Elem[Process[F,O]] =
    element[Emit[F,O]].asElem[Process[F,O]]

  sealed trait Process[F[_], O] extends Def[Process[F,O]] {
    implicit def eO: Elem[O]
    implicit def cF: Cont[F]
    def map[O2:Elem](f: Rep[O] => Rep[O2]): RProc[F,O2]
    def onHalt(f: Rep[Throwable] => RProc[F,O]): RProc[F,O]

    def ++(p: Th[Process[F,O]]): RProc[F,O] =
      this.onHalt { (t: Rep[Throwable]) =>
        IF ( t === End) {
          Process.Try(p) // we consult `p` only on normal termination
        } ELSE {
          Halt(t)
        }}

    //    def flatMap[B:Elem](f: Rep[A] => Rep[Process[F,B]]): Rep[Process[F,B]] = Bind(self, fun(f))
//
//    def mapBy[B:Elem](f: Rep[A => B]): RProcess[F,B] =
//      flatMap(a => Return(f(a)))
//    def foldMap[G[_]:Monad](f: F ~> G): Rep[G[A]]
//    def run[G[_]:Monad](f: F ~> G): Rep[G[A]] = step.foldMap(f)
//    def step: Rep[Process[F,A]] = self   // for Return and Suspend
  }
  trait ProcessCompanion {
    def emit[F[_]:Cont,O:Elem](head: Rep[O], tail: Rep[Process[F,O]]): Rep[Process[F,O]] =
      Emit(head, tail)

    def await[F[_]:Cont,A:Elem,O:Elem]
             (req: Rep[F[A]])(recv: Rep[Throwable | A] => RProc[F,O]): RProc[F,O] =
      Await(req, fun(recv))

    def eval[F[_]:Cont,A:Elem](a: Rep[F[A]]): RProc[F,A] =
      await[F,A,A](a) { sum =>
        sum.fold(err => Halt(err), a => Emit(a, Halt(End)))
      }

    def evalIO[A:Elem](a: Rep[IO[A]]): RProc[IO,A] =
      eval[IO,A](a)

    def Try[F[_]:Cont,O:Elem](p: Th[Process[F,O]]): RProc[F,O] =
      TRY {
        p
      } CATCH {
        (e: Rep[Throwable]) => Halt[F,O](e)
      }

  }

  abstract class Await[F[_],A,O]
      (val req: Rep[F[A]], val recv: Rep[(Throwable | A) => Process[F,O]])
      (implicit val eA: Elem[A], val eO: Elem[O], val cF: Cont[F]) extends Process[F,O] {

    override def map[O2: Elem](f: (Rep[O]) => Rep[O2]) =
      Await(req, fun { in: Rep[Throwable | A] => recv(in).map(f) })
    def onHalt(f: Rep[Throwable] => RProc[F,O]): RProc[F,O] =
      Await(req, fun { in: Rep[Throwable | A] => recv(in).onHalt(f) })

//    override def flatMap[R:Elem](f1: Rep[B] => Rep[Process[F,R]]): Rep[Process[F,R]] = {
//      a.flatMap((s: Rep[S]) => f(s).flatMap(f1))
//    }
//
//    def foldMap[G[_]:Monad](trans: F ~> G): Rep[G[B]] = a match {
//      case Def(susp: Suspend[F,S]) =>
//        Monad[G].flatMap(trans(susp.a)) { s =>
//          f(s).run(trans)
//        }
//      case Def(d) => !!!(s"Should be eliminated by method step: Unexpected $this where symbol $a of type ${a.selfType1} has def $d")
//      case _ => !!!(s"Should be eliminated by method step: Unexpected $this where symbol $a of type ${a.selfType1}")
//    }
//    override def step = a match {
//      case Def(b: Bind[F,s,S]) => b.a.flatMap((a: Rep[s]) => b.f(a).flatMap(s => f(s))).step
//      case Def(ret: Return[F,S]) => f(ret.a).step
//      case _ => self
//    }
  }
  trait AwaitCompanion

  abstract class Emit[F[_],O]
        (val head: Rep[O], val tail: Rep[Process[F,O]])
        (implicit val eO: Elem[O], val cF: Cont[F]) extends Process[F,O]
  {
    def map[O2: Elem](f: (Rep[O]) => Rep[O2]) =
      Process.Try(Thunk { Emit(f(head), tail map f) })
    def onHalt(f: Rep[Throwable] => RProc[F,O]): RProc[F,O] =
      Emit(head, tail.onHalt(f))
  }
  trait EmitCompanion

  abstract class Halt[F[_],O]
        (val err: Rep[Throwable])
        (implicit val eO: Elem[O], val cF: Cont[F]) extends Process[F, O] {
    def map[O2: Elem](f: (Rep[O]) => Rep[O2]) =
      Halt(err)
    def onHalt(f: Rep[Throwable] => RProc[F,O]): RProc[F,O] =
      Process.Try(Thunk { f(err) })

//    def foldMap[G[_]:Monad](trans: F ~> G): Rep[G[O]] = trans(a)
  }
  trait HaltCompanion

}

trait ProcessesDsl extends ScalanDsl with impl.ProcessesAbs with Processes with MonadsDsl {

  implicit def processCont[F[_]:Cont]: Cont[({type f[x] = Process[F,x]})#f] = new Cont[({type f[x] = Process[F,x]})#f] {
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[Process[F,T]]
    def lift[T](implicit eT: Elem[T]) = element[Process[F,T]].asElem[Process[F,T]]
    def unlift[T](implicit eFT: Elem[Process[F,T]]) = eFT.asInstanceOf[ProcessElem[F,T,_]].eO
    def getElem[T](fa: Rep[Process[F,T]]) = fa.selfType1
    def unapply[T](e: Elem[_]) = e match {
      case e: ProcessElem[_,_,_] => Some(e.asElem[Process[F,T]])
      case _ => None
    }
  }

  /* Special exception indicating normal termination */
  lazy val End: Rep[Throwable] = new Exception("End").asInstanceOf[Throwable]

  /* Special exception indicating forceful termination */
  lazy val Kill: Rep[Throwable] = new Exception("Kill").asInstanceOf[Throwable]

  def tryCatch[A](t: Th[A], c: Rep[Throwable] => Rep[A]): Rep[A]

  def TRY[A](thunk: Th[A]): TryBranch[A] = new TryBranch(thunk)

  class TryBranch[A](thunk: Th[A]) {
    def CATCH(handler: Rep[Throwable] => Rep[A]): Rep[A] = tryCatch(thunk, handler)
  }

  //  def freeMonad[F[_]:Cont]: Monad[({type f[a] = Process[F,a]})#f] =
//    new Monad[({type f[a] = Process[F,a]})#f] {
//      def cF = freeCont
//      def unit[A:Elem](a: Rep[A]) = Return(a)
//      override def flatMap[A:Elem,B:Elem](fa: Rep[Process[F, A]])(f: Rep[A] => Rep[Process[F, B]]) = fa flatMap f
//    }
//
//  def eval[A:Elem](v: Rep[A]): Rep[Oper[A]]
}


trait ProcessesDslSeq extends ScalanDslStd with impl.ProcessesSeq {

  //def eval[A:Elem](v: Rep[A]): Rep[Oper[A]] = i => (i + 1, v)
  def tryCatch[A:Elem](t: Th[A], c: (Rep[Throwable]) => Rep[A]) =
    try {
      t.force()
    }
    catch { case e: Throwable =>
      c(e)
    }
}

trait ProcessesDslExp extends ScalanDslExp with impl.ProcessesExp {

//  def eval[A:Elem](v: Rep[A]): Rep[Oper[A]] = fun { i => Eval(i, v) }

//  case class Eval[A:Elem](i: Rep[Int], v: Rep[A]) extends BaseDef[(Int, A)]
  def tryCatch[A](t: Th[A], c: (Rep[Throwable]) => Rep[A]) = ???
}