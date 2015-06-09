package scalan.monads

import scalan._
import scala.reflect.runtime.universe._

trait Readers extends Base { self: MonadsDsl =>

  type RepReader[Env,A] = Rep[Reader[Env,A]]
  trait Reader[Env,A] extends Reifiable[Reader[Env, A]] { self =>
    implicit def eEnv: Elem[Env]
    implicit def eA: Elem[A]
    def run: Rep[Env => A]
  }
  trait ReaderCompanion {
    def ask[R:Elem]: Rep[Reader[R, R]] = ReaderBase(fun {r => r})
  }

  abstract class ReaderBase[Env, A](val run: Rep[Env => A])
                                 (implicit val eEnv: Elem[Env], val eA: Elem[A]) extends Reader[Env, A] {

  }
  trait ReaderBaseCompanion
}


trait ReadersDsl extends impl.ReadersAbs { self: MonadsDsl =>

  implicit def readerCont[R:Elem]: Cont[({type f[x] = Reader[R,x]})#f] = new Container[({type f[x] = Reader[R,x]})#f] {
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[Reader[R,T]]
    def lift[T](implicit eT: Elem[T]) = element[Reader[R,T]] //.asElem[Reader[R,T]]
  }

  implicit def readerMonad[R:Elem]: Monad[({type f[x] = Reader[R,x]})#f] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def cF = readerCont
    def unit[A:Elem](a: Rep[A]): RepReader[R, A] = {
      ReaderBase(fun {_ => a})
    }
    override def flatMap[A:Elem,B:Elem](st: RepReader[R, A])(f: Rep[A] => RepReader[R, B]): RepReader[R, B] =
      ReaderBase(fun {r => f(st.run(r)).run(r)})
  }

}

trait ReadersDslSeq extends impl.ReadersSeq { self: MonadsDslSeq =>
}

trait ReadersDslExp extends impl.ReadersExp { self: MonadsDslExp =>
}
