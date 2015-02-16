package scalan.performance

import org.scalameter.api._
import scalan.parrays.PArrayExamples
import scalan.{ScalanCommunityDslExp, ScalanCommunityDslSeq, ScalanCtxExp}
import scalan.parrays.PArraysDslExp

class PArrayBenchmarkExp extends BaseBenchmark with ScalanCommunityDslExp with PArrayExamples {
  val sizes = Gen.range("size")(100, 1000, 100)
  
  val arrays = sizes.map(size => fromArray(toRep((0 to (size - 1)).toArray)))
  
  performance of "PArray" in {
    measure method "map" in {
      using(arrays) in {
    	a => mapped(a)
      }
    }
  }
}