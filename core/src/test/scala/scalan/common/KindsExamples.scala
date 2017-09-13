package scalan.common

import scala.collection.Seq
import scalan.Scalan
import scala.reflect.runtime.universe._

trait KindsExamples extends Scalan with KindsModule {
  type Id[A] = A

  implicit val functorId: Functor[Id] = new Functor[Id] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[Id[A]]
    def lift[A](implicit evA: Elem[A]) = evA
    def unlift[T](implicit eFT: Elem[Id[T]]) = eFT
    def getElem[T](fa: Rep[Id[T]]) = !!!("Operation is not supported by Id container " + fa)
    def unapply[T](e: Elem[_]) = Some(e.asElem[Id[T]])
    def map[A, B](a: Rep[Id[A]])(f: (Rep[A]) => Rep[B]) = f(a)
  }

  lazy val t1 = fun { (in: Rep[Kind[Id,Int]]) => in }

  lazy val kindMap = fun { (in: Rep[Kind[Id,Int]]) => in.mapBy(fun {x => x + 1}) }
  
  // should compile:
  // val e = null.asInstanceOf[BindElem[F,_,_] forSome {type F[A]}].typeArgs("B")._1.asElem
}
