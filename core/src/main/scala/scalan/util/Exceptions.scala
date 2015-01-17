package scalan.util

import scalan._

trait Exceptions extends Base with BaseTypes { self: ExceptionsDsl =>

  type RThrow = Rep[Throwable]
  trait SThrowable extends BaseTypeEx[Throwable, SThrowable] { self =>
   // @External def getMessage: Rep[String]
  }
  trait SThrowableCompanion

  abstract class SException(val value: Rep[Throwable]) extends SThrowable {
  }
  trait SExceptionCompanion

  implicit val ThrowableElement: Elem[Throwable] = new BaseElem[Throwable]

}

trait ExceptionsDsl extends impl.ExceptionsAbs {
}

trait ExceptionsDslSeq extends impl.ExceptionsSeq {
}

trait ExceptionsDslExp extends impl.ExceptionsExp {
}
