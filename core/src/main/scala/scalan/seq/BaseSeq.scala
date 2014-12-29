package scalan.seq

import scala.language.implicitConversions
import scalan.{Base, ScalanSeq}

trait BaseSeq extends Base { self: ScalanSeq =>
  type Rep[+A] = A
  type Def[+A] = A

  override def toRep[A](x: A)(implicit eA: Elem[A]) = x

  override def def_unapply[T](e: Rep[T]): Option[Def[T]] = Some(e)

  override def repReifiable_getElem[T <: Reifiable[T]](x: Rep[T]): Elem[T] = x.selfType
}
