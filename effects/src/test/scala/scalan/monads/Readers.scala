package scalan.monads

import scalan._
import scala.reflect.runtime.universe._

trait Readers extends Base { self: MonadsDsl =>

  type RepReader[Env,A] = Rep[Reader[Env,A]]
  trait Reader[Env,A] extends Def[Reader[Env, A]] { self =>
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

  trait ReaderCont[R] extends Cont[({type f[x] = Reader[R,x]})#f] {
    implicit def eR: Elem[R]
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[Reader[R,T]]
    def lift[T](implicit eT: Elem[T]) = element[Reader[R,T]] //.asElem[Reader[R,T]]
    def unlift[T](implicit eFT: Elem[Reader[R, T]]) = eFT.asInstanceOf[ReaderElem[R,T,_]].eA
    def getElem[T](fa: Rep[Reader[R, T]]) = fa.selfType1
    def unapply[T](e: Elem[_]) = e match {
      case te: ReaderElem[_, _, _] => Some(te.asElem[Reader[R,T]])
      case _ => None
    }
  }

  implicit def readerMonad[R:Elem]: Monad[({type f[x] = Reader[R,x]})#f] = new ReaderCont[R] with Monad[({type f[x] = Reader[R,x]})#f] {
    def eR = element[R]
    def unit[A:Elem](a: Rep[A]): RepReader[R, A] = {
      ReaderBase[R,A](fun {_: Rep[R] => a})
    }
    override def flatMap[A:Elem,B:Elem](st: RepReader[R, A])(f: Rep[A] => RepReader[R, B]): RepReader[R, B] =
      ReaderBase(fun {r => f(st.run(r)).run(r)})
  }

}
