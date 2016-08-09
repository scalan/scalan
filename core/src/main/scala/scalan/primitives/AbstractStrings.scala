package scalan.primitives

import scalan._

trait AbstractStrings extends Base with TypeWrappers { self: AbstractStringsDsl =>

  type RStr = Rep[String]
  trait AString extends Def[AString] {
    def wrappedValue: Rep[String]
  }
  trait AStringCompanion extends ExCompanion0[String] {
    def apply(msg: Rep[String]): Rep[String] = newObjEx[String](msg)
  }

  abstract class SString(val wrappedValue: Rep[String]) extends AString
  trait SStringCompanion

  abstract class CString(val wrappedValue: Rep[String]) extends AString
  trait CStringCompanion
}
