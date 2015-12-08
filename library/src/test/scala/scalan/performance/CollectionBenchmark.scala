package scalan.performance

import org.scalameter.api._
import scalan.collections.CollectionExamples
import scalan.{ScalanCommunityDslExp, ScalanCommunityDslSeq, ScalanCtxExp}

class CollectionBenchmarkExp extends BaseBenchmark {
  val scalan = new ScalanCommunityDslExp with CollectionExamples
  import scalan._

  val sizes = Gen.range("size")(100, 1000, 100)
  
  val arrays = sizes.map(size => fromArray(toRep((0 to (size - 1)).toArray)))
  
  performance of "Collection" in {
    measure method "map" in {
      using(arrays) in {
    	a => mapped(a)
      }
    }
  }
}