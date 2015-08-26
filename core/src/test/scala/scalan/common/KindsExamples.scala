package scalan.common

import scala.collection.Seq
import scalan.Scalan
import scala.reflect.runtime.universe._

trait KindsExamples extends Scalan with KindsDsl {
  type Id[A] = A

  implicit val functorId: Functor[Id] = new Functor[Id] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[Id[A]]
    def lift[A](implicit evA: Elem[A]) = evA
    def map[A: Elem, B: Elem](a: Rep[Id[A]])(f: (Rep[A]) => Rep[B]) = f(a)
  }

  lazy val t1 = fun { (in: Rep[Kind[Id,Int]]) => in }

  lazy val kindMap = fun { (in: Rep[Kind[Id,Int]]) => in.mapBy(fun {x => x + 1}) }
}
