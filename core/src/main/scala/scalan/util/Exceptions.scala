package scalan.util

import scalan._
import scalan.common.Default

trait Exceptions extends Base with BaseTypes { self: ExceptionsDsl =>

  type RThrow = Rep[Throwable]
  trait SThrowable extends BaseTypeEx[Throwable, SThrowable] { self =>
   @External def getMessage: Rep[String]
  }
  trait SThrowableCompanion extends ExCompanion0[Throwable]  {
    def defaultVal = Default.defaultVal(new Throwable("default exception"))
    def apply(msg: Rep[String]): Rep[Throwable] = newObjEx(classOf[Throwable], List(msg.asRep[AnyRef]))
  }
  implicit def defaultSThrowableElem: Elem[SThrowable] = element[SException].asElem[SThrowable]

  abstract class SException(val value: Rep[Throwable]) extends SThrowable {
    def getMessage: Rep[String] =
      methodCallEx[String](self, this.getClass.getMethod("getMessage"), List())
  }
  trait SExceptionCompanion

}

trait ExceptionsDsl extends impl.ExceptionsAbs {
}

trait ExceptionsDslSeq extends impl.ExceptionsSeq {
}

trait ExceptionsDslExp extends impl.ExceptionsExp {
}
