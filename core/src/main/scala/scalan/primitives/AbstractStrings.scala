package scalan.primitives

import scalan._
import scalan.common.Default

trait AbstractStrings extends Base with BaseTypes { self: AbstractStringsDsl =>

  type RStr = Rep[String]
  trait AString extends BaseTypeEx[String, AString] { self => }
  trait AStringCompanion extends ExCompanion0[String]  {
    def defaultVal = Default.defaultVal("")
    def apply(msg: Rep[String]): Rep[String] = newObjEx(classOf[String], List(msg.asRep[AnyRef]))
  }
  implicit def defaultAStringElem: Elem[AString] = element[SString].asElem[AString]

  abstract class SString(val wrappedValueOfBaseType: Rep[String]) extends AString
  trait SStringCompanion

  abstract class CString(val wrappedValueOfBaseType: Rep[String]) extends AString
  trait CStringCompanion
}

trait AbstractStringsDsl extends impl.AbstractStringsAbs {
}

trait AbstractStringsDslSeq extends impl.AbstractStringsSeq {
}

trait AbstractStringsDslExp extends impl.AbstractStringsExp {
}
