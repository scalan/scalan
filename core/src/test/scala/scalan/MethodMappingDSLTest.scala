package scalan

import scalan.compilation.language._

class MethodMappingDSLTest extends BaseTests {
  object TestMethodMappingDSL extends MethodMappingDSL {
    {
      import Scala._
      mapModule("scalan.flint.DataFrames").types(// apply?
        mapType("FlintDataFrame").to("org.spark.RDD").methods(
          mapMethod("length").to("count"),
          mapMethod("top", 'compare, 'n).args('n).implicitArgs(MkOrdering('compare))
        ))
    }

    {
      import Cxx._
      mapModule("scalan.flint.DataFrames").withHeader("flint.h").withNamespace("flint").types(
        mapType("FlintDataFrame", 'eT).to("data_frame").methods(
          mapMethod("saveFile").to("output"),
          mapMethod("top", 'compare, 'n).templateArgs('compare).args('n),
          mapMethod("join", 'innerRdd, 'outerKey, 'innerKey, 'estimation, 'kind, 'eI, 'eK)
            .templateArgs('eI, 'eK, VoidInOut('outerKey), VoidInOut('innerKey))
            .args('innerRdd, 'estimation, EnumIndex('kind))),
        mapType("InputDF").to("input_data_frame").methods())
    }
  }

  test("Scala Method") {
    // TODO asInstanceOf should be possible to remove
    val scalaMethod = TestMethodMappingDSL.mappingsFor(Scala).head.getMethod("scalan.flint.DataFrames$FlintDataFrame", "length", None)
    scalaMethod.map(_.method.mappedName) should be(Some("count"))
  }

  test("C++ Method") {
    val cxxMethod = TestMethodMappingDSL.mappingsFor(Cxx).head.getMethod("scalan.flint.DataFrames$FlintDataFrame", "length", None)
    cxxMethod should be(None)
//    cxxMethod.mappedName should equal("length")
  }
}