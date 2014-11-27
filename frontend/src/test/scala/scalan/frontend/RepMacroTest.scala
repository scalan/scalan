package scalan.frontend

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.FunSuite

import scalan.{ScalanCtxSeq, ScalanDsl}

class RepMacroTest extends FunSuite with TypeCheckedTripleEquals {

  val seq = new ScalanCtxSeq with Examples

  test("constant") {
    assert(rep(1) === 1)
  }

  test("constant YY") {
    assert(repYY(1) === 1)
  }
}

trait Examples extends ScalanDsl {

  val constant: Rep[Int] = 1
}
