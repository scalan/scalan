package scalan.util

import scalan._
import scalan.common.Default

trait Exceptions extends Base with BaseTypes { self: ExceptionsDsl =>

  type RThrow = Rep[Throwable]
  trait SThrowable extends BaseTypeEx[Throwable, SThrowable] { self =>
    @External def getMessage: Rep[String]
    @External def initCause (cause: Rep[Throwable]) : Rep[Throwable]
  }
  trait SThrowableCompanion extends ExCompanion0[Throwable]  {
    @Constructor def apply(msg: Rep[String]): Rep[Throwable]
  }
  implicit def defaultExceptionValue = Default.defaultVal(new Throwable("default exception"))

  abstract class SException(val wrappedValueOfBaseType: Rep[Throwable]) extends SThrowable {
    def getMessage: Rep[String] =
      methodCallEx[String](self, this.getClass.getMethod("getMessage"), List())

    def initCause (cause: Rep[Throwable]) : Rep[Throwable] = {
      methodCallEx[Throwable](self, this.getClass.getMethod("initCause", classOf[Object]), List(cause.wrappedValueOfBaseType.asInstanceOf[AnyRef]))
    }
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
