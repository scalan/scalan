package scalan.seq

import scala.language.implicitConversions
import scalan.{Base, ScalanSeq}

trait BaseSeq extends Base { self: ScalanSeq =>
  type Rep[+A] = A

  override protected def stagingExceptionMessage(message: String, syms: Seq[Rep[_]]) = message

  override def toRep[A](x: A)(implicit eA: Elem[A]) = x

  override def def_unapply[A](e: Rep[A]): Option[Def[A]] = e match {
    case e: Def[A @unchecked] => Some(e)
    case _ => None
  }

  override def reifyObject[A](x: Def[A]): Rep[A] = x.asInstanceOf[A]

  override def repDef_getElem[T <: Def[_]](x: Rep[T]): Elem[T] = x.selfType.asElem[T]
}
