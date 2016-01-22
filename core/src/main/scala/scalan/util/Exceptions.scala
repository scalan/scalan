package scalan.util

import scalan._
import scalan.common.Default

trait Exceptions extends Base with TypeWrappers { self: ExceptionsDsl =>

  type RThrow = Rep[Throwable]
  trait SThrowable extends TypeWrapper[Throwable, SThrowable] { self =>
    @External def getMessage: Rep[String]
    @External def initCause (cause: Rep[SThrowable]) : Rep[SThrowable]
  }
  trait SThrowableCompanion extends ExCompanion0[Throwable]  {
    @Constructor def apply(msg: Rep[String]): Rep[SThrowable]
  }
  lazy val DefaultOfThrowable = Default.defaultVal(new Throwable("default exception"))

}

trait ExceptionsDslStd extends impl.ExceptionsStd {
  trait StdSThrowable
}
