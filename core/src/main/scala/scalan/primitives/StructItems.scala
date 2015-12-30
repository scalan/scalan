package scalan.primitives

import scalan._
import scala.reflect.runtime.universe._

trait StructItems extends ViewsDsl with Entities  { self: StructsDsl with Scalan =>

  trait StructItem[Val] extends Def[StructItem[Val]] {
    def eVal: Elem[Val]
    def key: Rep[StructKey]
    def value: Rep[Val]
  }

  abstract class StructItemBase[Val]
        (val key: Rep[StructKey], val value: Rep[Val])
        (implicit val eVal: Elem[Val])
    extends StructItem[Val]

}

trait StructItemsDsl extends impl.StructItemsAbs { self: StructsDsl with Scalan =>

  trait StructItemFunctor extends Functor[StructItem] {
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[StructItem[T]]
    def lift[T](implicit eT: Elem[T]) = element[StructItem[T]]
    def unlift[T](implicit eFT: Elem[StructItem[T]]) = eFT.asInstanceOf[StructItemElem[T,_]].eVal
    def getElem[T](fa: Rep[StructItem[T]]) = fa.selfType1
    def map[A:Elem,B:Elem](xs: Rep[StructItem[A]])(f: Rep[A] => Rep[B]) = StructItemBase(xs.key, f(xs.value))
  }
  implicit val containerStructItem: Functor[StructItem] = new StructItemFunctor {}

}
