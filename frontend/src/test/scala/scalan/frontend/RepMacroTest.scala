package scalan.frontend

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.FunSuite

import scalan.{ScalanCtxSeq, ScalanDsl}

class RepMacroTest extends FunSuite with TypeCheckedTripleEquals with IdRep {

  val seq = new ScalanCtxSeq with Examples

  test("constant") {
    assert(rep(1) === 1)
  }

  test("constant YY") {
    assert(repYY(1) === 1)
  }

  test("value") {
    val r = rep {
      val x = 1
      x
    }
    assert(r === Rep(1))
  }

}

trait Examples extends ScalanDsl {

  val constant: Rep[Int] = 1
}
