package scalan.primitives

import scala.annotation.unchecked.uncheckedVariance
import scalan._
import scala.reflect.runtime.universe._
import OverloadHack.{Overloaded2, Overloaded1}

trait StructItems extends ViewsModule with Entities  { self: Structs with Scalan =>

  trait StructItem[@uncheckedVariance +Val, Schema <: Struct] extends Def[StructItem[Val @uncheckedVariance, Schema]] {
    def eVal: Elem[Val @uncheckedVariance]
    def eSchema: Elem[Schema]
    def key: Rep[StructKey[Schema]]
    def value: Rep[Val]
  }

  abstract class StructItemBase[Val, Schema <: Struct]
        (val key: Rep[StructKey[Schema]], val value: Rep[Val])
//        (implicit val eVal: Elem[Val], val eSchema: Elem[Schema])
    extends StructItem[Val, Schema]

}

trait StructItemsModule extends impl.StructItemsDefs { self: Structs with Scalan =>

  def struct_getItem[S <: Struct](s: Rep[S], i: Int)(implicit o1: Overloaded1): Rep[StructItem[_,S]] = {
    val value = s.getUntyped(i)
    val eS = s.elem
    val key = IndexStructKey[S](i)(eS)
    StructItemBase(key, value)
  }

  def struct_setItem[S <: Struct](s: Rep[S], i: Rep[Int], v: Rep[_]): Rep[S] = {
    updateField(s, s.elem.fieldNames(i.asValue), v)
  }

  trait StructItemFunctor[S <: Struct] extends Functor[({type f[x] = StructItem[x,S]})#f] {
    implicit def eS: Elem[S]
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[StructItem[T,S]]
    def lift[T](implicit eT: Elem[T]) = element[StructItem[T,S]]
    def unlift[T](implicit eFT: Elem[StructItem[T,S]]) = eFT.asInstanceOf[StructItemElem[T,S,_]].eVal
    def getElem[T](fa: Rep[StructItem[T,S]]) = fa.elem
    def unapply[T](e: Elem[_]) = e match {
      case e: StructItemElem[_, _, _] => Some(e.asElem[StructItem[T,S]])
      case _ => None
    }
    def map[A,B](xs: Rep[StructItem[A,S]])(f: Rep[A] => Rep[B]) = {
      val res = f(xs.value)
      implicit val eB = res.elem
      StructItemBase(xs.key, res)
    }
  }
  implicit def structItemContainer[S <: Struct : Elem]: Functor[({type f[x] = StructItem[x,S]})#f] =
    new StructItemFunctor[S] { def eS = element[S] }

  implicit class StructElemExtensionsForStructItem[S <: Struct](eS: Elem[S]) {
    def getItemElem[V](i: Int): Elem[StructItem[V, S]] = {
      val eV = eS(i).asElem[V]
      structItemElement(eV, eS)
    }
    def getItemElem[V](fieldName: String): Elem[StructItem[V, S]] = {
      val eV = eS(fieldName).asElem[V]
      structItemElement(eV, eS)
    }
  }

  implicit class StructExtensionsForStructItem[S <: Struct](s: Rep[S]) {
    def getItem[A](i: Int): Rep[StructItem[A, S]] = {
      val item = struct_getItem(s, i)
      item.asRep[StructItem[A,S]]
    }
    def getItem[A](i: Rep[Int]): Rep[StructItem[A, S]] = struct_getItem(s, i).asRep[StructItem[A,S]]
    def getItem[A](k: Rep[StructKey[S]])(implicit o: Overloaded2): Rep[StructItem[A,S]] = struct_getItem(s, k.index).asRep[StructItem[A,S]]
    def setItem(i: Rep[Int], v: Rep[_]): Rep[S] = struct_setItem(s, i, v)
    def setItem(k: Rep[StructKey[S]], v: Rep[_])(implicit o: Overloaded2): Rep[S] = struct_setItem(s, k.index, v)
  }

  def struct_getItem[S <: Struct](s: Rep[S], i: Rep[Int]): Rep[StructItem[_,S]] =
    i match {
      case Def(Const(i: Int)) => struct_getItem(s, i)
      case _ =>
        ???
    }
}

