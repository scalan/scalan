package scalan.primitives

import scala.annotation.unchecked.uncheckedVariance
import scalan._
import scala.reflect.runtime.universe._
import scalan.common.OverloadHack.Overloaded1

trait StructItems extends ViewsDsl with Entities  { self: StructsDsl with Scalan =>

  trait StructItem[@uncheckedVariance +Val, Schema <: Struct] extends Def[StructItem[Val @uncheckedVariance, Schema]] {
    def eVal: Elem[Val @uncheckedVariance]
    def eSchema: Elem[Schema]
    def key: Rep[StructKey[Schema]]
    def value: Rep[Val]
  }

  abstract class StructItemBase[Val, Schema <: Struct]
        (val key: Rep[StructKey[Schema]], val value: Rep[Val])
        (implicit val eVal: Elem[Val], val eSchema: Elem[Schema])
    extends StructItem[Val, Schema]

}

trait StructItemsDsl extends impl.StructItemsAbs { self: StructsDsl with Scalan =>

  def struct_getItem[S <: Struct](s: Rep[S], i: Rep[Int])(implicit eS: Elem[S]): Rep[StructItem[_,S]]

  def struct_getItem[S <: Struct](s: Rep[S], i: Int)(implicit eS: Elem[S], o1: Overloaded1): Rep[StructItem[_,S]] = {
    val names = eS.fieldNames
    val keySet = KeySet(names)
    val value = s(names(i))
    val key = IndexStructKey[S](i)
    StructItemBase(key, value)(eS.fields(i)._2.asElem[Any], eS)
  }

  trait StructItemFunctor[S <: Struct] extends Functor[({type f[x] = StructItem[x,S]})#f] {
    implicit def eS: Elem[S]
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[StructItem[T,S]]
    def lift[T](implicit eT: Elem[T]) = element[StructItem[T,S]]
    def unlift[T](implicit eFT: Elem[StructItem[T,S]]) = eFT.asInstanceOf[StructItemElem[T,S,_]].eVal
    def getElem[T](fa: Rep[StructItem[T,S]]) = fa.selfType1
    def map[A:Elem,B:Elem](xs: Rep[StructItem[A,S]])(f: Rep[A] => Rep[B]) = StructItemBase(xs.key, f(xs.value))
  }
  implicit def containerStructItem[S <: Struct : Elem]: Functor[({type f[x] = StructItem[x,S]})#f] =
    new StructItemFunctor[S] { def eS = element[S] }

  implicit class StructExtensionsForStructItem[S <: Struct](s: Rep[S])(implicit eS: Elem[S]) {
    def getItem(i: Int): Rep[StructItem[_, S]] = struct_getItem(s, i)
    def getItem(i: Rep[Int]): Rep[StructItem[_, S]] = struct_getItem(s, i)
  }

}

trait StructItemsDslSeq extends impl.StructItemsSeq {self: StructsDsl with ScalanSeq =>
  def struct_getItem[S <: Struct](s: Rep[S], i: Rep[Int])(implicit eS: Elem[S]): Rep[StructItem[_,S]] = struct_getItem(s, i)
}
trait StructItemsDslExp extends impl.StructItemsExp {self: StructsDsl with ScalanExp =>

  def struct_getItem[S <: Struct](s: Rep[S], i: Rep[Int])(implicit eS: Elem[S]): Rep[StructItem[_,S]] =
    i match {
      case Def(Const(i: Int)) => struct_getItem(s, i)
      case _ =>
        ???
    }
}
