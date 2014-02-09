package scalan.seq

import scalan.{ScalanSeq, Base}
import scala.language.{implicitConversions}

trait BaseSeq extends Base { self: ScalanSeq =>

  override def toRep[A](x: A)(implicit eA: Elem[A]) = x

}
