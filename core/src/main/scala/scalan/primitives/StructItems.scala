package scalan.primitives

import scala.annotation.unchecked.uncheckedVariance
import scalan._
import scala.reflect.runtime.universe._

trait StructItems extends ViewsDsl with Entities  { self: StructsDsl with Scalan =>

  trait StructItem[@uncheckedVariance +Val, Schema] extends Def[StructItem[Val @uncheckedVariance, Schema]] {
    def eVal: Elem[Val @uncheckedVariance]
    def eSchema: Elem[Schema]
    def key: Rep[StructKey]
    def value: Rep[Val]
  }

  abstract class StructItemBase[Val, Schema]
        (val key: Rep[StructKey], val value: Rep[Val])
        (implicit val eVal: Elem[Val], val eSchema: Elem[Schema])
    extends StructItem[Val, Schema]

}

trait StructItemsDsl extends impl.StructItemsAbs { self: StructsDsl with Scalan =>

  trait StructItemFunctor[S] extends Functor[({type f[x] = StructItem[x,S]})#f] {
    implicit def eS: Elem[S]
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[StructItem[T,S]]
    def lift[T](implicit eT: Elem[T]) = element[StructItem[T,S]]
    def unlift[T](implicit eFT: Elem[StructItem[T,S]]) = eFT.asInstanceOf[StructItemElem[T,S,_]].eVal
    def getElem[T](fa: Rep[StructItem[T,S]]) = fa.selfType1
    def map[A:Elem,B:Elem](xs: Rep[StructItem[A,S]])(f: Rep[A] => Rep[B]) = StructItemBase(xs.key, f(xs.value))
  }
  implicit def containerStructItem[S:Elem]: Functor[({type f[x] = StructItem[x,S]})#f] =
    new StructItemFunctor[S] { def eS = element[S] }

  implicit class StructExtensionsForStructItem[S <: Struct](s: Rep[S])(implicit eS: Elem[S]) {
    def getItem(i: Rep[Int]): Rep[StructItem[_, S]] = ??? // TODO struct
  }
}
