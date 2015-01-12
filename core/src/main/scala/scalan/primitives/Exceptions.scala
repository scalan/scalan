package scalan.primitives

import scalan.{ScalanExp, ScalanSeq, Scalan}
import scalan.common.Lazy

trait Exceptions { self: Scalan =>
  implicit val ThrowableElement: Elem[Throwable] = new BaseElem[Throwable]
}

trait ExceptionsSeq extends Exceptions { self: ScalanSeq =>
}

trait ExceptionsExp extends Exceptions { self: ScalanExp =>
}
