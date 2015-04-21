package scalan.common

import scala.collection.Seq
import scalan.Scalan
import scala.reflect.runtime.universe._

trait KindsExamples extends Scalan with KindsDsl {
  type Id[A] = A

  implicit val containerId: Cont[Id] = new Container[Id] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[Id[A]]
    def lift[A](implicit evA: Elem[A]) = evA
  }

  lazy val t1 = fun { (in: Rep[Kind[Id,Int]]) => in }

  lazy val kindMap = fun { (in: Rep[Kind[Id,Int]]) => in.mapBy(fun {x => x + 1}) }
}
