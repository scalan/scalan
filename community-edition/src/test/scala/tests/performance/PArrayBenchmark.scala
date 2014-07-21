package tests.performance

import org.scalameter.api._
import tests.scalan.arrays.PArrayExamples
import scalan.ScalanStagedImplementation
import scalan.arrays.PArraysDslExp

class PArrayBenchmarkStaged extends BaseBenchmark with ScalanStagedImplementation with PArrayExamples with PArraysDslExp {
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