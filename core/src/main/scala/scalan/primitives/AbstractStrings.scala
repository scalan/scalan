package scalan.primitives

import scalan._
import scalan.common.Default

trait AbstractStrings extends Base with TypeWrappers { self: AbstractStringsDsl =>

  type RStr = Rep[String]
  trait AString extends Reifiable[AString] {
    def wrappedValueOfBaseType: Rep[String]
  }
  trait AStringCompanion extends ExCompanion0[String]  {
    def defaultVal = Default.defaultVal("")
    def apply(msg: Rep[String]): Rep[String] = newObjEx(classOf[String], List(msg))
  }

  abstract class SString(val wrappedValueOfBaseType: Rep[String]) extends AString
  trait SStringCompanion

  abstract class CString(val wrappedValueOfBaseType: Rep[String]) extends AString
  trait CStringCompanion
}
