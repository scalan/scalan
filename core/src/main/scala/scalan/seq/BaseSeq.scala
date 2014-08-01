package scalan.seq

import scala.language.implicitConversions
import scalan.{Base, ScalanSeq}

trait BaseSeq extends Base {
  type Rep[+A] = A

  override def toRep[A](x: A)(implicit eA: Elem[A]) = x
}
