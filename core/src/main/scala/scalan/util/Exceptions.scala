package scalan.util

import scalan._
import scalan.common.Default

trait Exceptions extends Base with TypeWrappers { self: ExceptionsDsl =>

  type RThrow = Rep[Throwable]
  trait SThrowable extends TypeWrapper[Throwable, SThrowable] { self =>
    @External def getMessage: Rep[String]
    @External def initCause (cause: Rep[SThrowable]) : Rep[SThrowable]
    def wrappedValueOfBaseType: Rep[Throwable]
  }
  trait SThrowableCompanion extends ExCompanion0[Throwable]  {
    @Constructor def apply(msg: Rep[String]): Rep[SThrowable]
  }
  lazy val DefaultOfThrowable = Default.defaultVal(new Throwable("default exception"))

  abstract class SException(val wrappedValueOfBaseType: Rep[Throwable]) extends SThrowable {
    def getMessage: Rep[String] =
      methodCallEx[String](self, this.getClass.getMethod("getMessage"), List())

    def initCause (cause: Rep[SThrowable]) : Rep[SThrowable] = {
      methodCallEx[SThrowable](self, this.getClass.getMethod("initCause", classOf[Object]), List(cause.asInstanceOf[AnyRef]))
    }
  }
  trait SExceptionCompanion

}

trait ExceptionsDslSeq extends impl.ExceptionsSeq {
  trait SeqSThrowable
}
