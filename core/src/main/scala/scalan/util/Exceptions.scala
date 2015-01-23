package scalan.util

import scalan._
import scalan.common.Default

trait Exceptions extends Base with BaseTypes { self: ExceptionsDsl =>

  type RThrow = Rep[Throwable]
  trait SThrowable extends BaseTypeEx[Throwable, SThrowable] { self =>
   @External def getMessage: Rep[String]
  }
  trait SThrowableCompanion extends ExCompanion0[Throwable]  {
    @Constructor def apply(msg: Rep[String]): Rep[Throwable]
  }
  implicit def defaultExceptionValue = Default.defaultVal(new Throwable("default exception"))

  abstract class SException(val value: Rep[Throwable]) extends SThrowable {
    def getMessage: Rep[String] =
      methodCallEx[String](self, this.getClass.getMethod("getMessage"), List())
  }
  trait SExceptionCompanion

}

trait ExceptionsDsl extends impl.ExceptionsAbs {
}

trait ExceptionsDslSeq extends impl.ExceptionsSeq {
  trait SeqSThrowable
}

trait ExceptionsDslExp extends impl.ExceptionsExp {
}
