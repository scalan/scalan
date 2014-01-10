/**
 * User: s00747473
 * Date: 11/16/13
 */
package tests.scalan.primitives

import scalan.{ScalanCtxStaged, ScalanCtxSeq}
import org.scalatest.FlatSpec

class PrimitivesExamplesSuite extends FlatSpec {

  "when mixing trait" should "be constructed in Seq context" in {
      val ctx = new ScalanCtxSeq with PrimitiveExamples {}
  }
  it should "be constructed in Staged context" in {
    val ctx = new ScalanCtxStaged with PrimitiveExamples {}
  }

  "in staged context" should "stage functions" in {
    val ctx = new ScalanCtxStaged with PrimitiveExamples {}
    val f1 = ctx.id
    val f2 = ctx.inc
  }
 }
