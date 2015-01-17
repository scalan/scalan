package scalan.util

import scalan._
import scalan.common.Default

trait Exceptions extends Base with BaseTypes { self: ExceptionsDsl =>

  type RThrow = Rep[Throwable]
  trait SThrowable extends BaseTypeEx[Throwable, SThrowable] { self =>
   // @External def getMessage: Rep[String]
  }
  trait SThrowableCompanion  {
    def defaultVal = Default.defaultVal(new Exception("default exception"))
  }

  abstract class SException(val value: Rep[Throwable]) extends SThrowable {
  }
  trait SExceptionCompanion

}

trait ExceptionsDsl extends impl.ExceptionsAbs {
}

trait ExceptionsDslSeq extends impl.ExceptionsSeq {
}

trait ExceptionsDslExp extends impl.ExceptionsExp {
}
