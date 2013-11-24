/**
 * User: s00747473
 * Date: 11/16/13
 */
package tests.scalan.primitives

import org.junit.Test
import scalan.{ScalanDsl, ScalanCtxStaged, ScalanCtxShallow}

class PrimitivesTests extends {

   val seq = new ScalanCtxShallow
   val stag = new ScalanCtxStaged

  @Test def seqPrimitives() {
    import seq._;
  }

 }
