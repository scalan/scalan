package scalan

import scalan.compilation.language._
import CxxMapping._
import ScalaMapping._

class MethodMappingDSLTest extends BaseTests {
  object TestMethodMappingDSL extends MethodMappingDSL {

    MapModuleScala("scalan.flint.DataFrames").types( // apply?
      MapTypeScala("FlintDataFrame").to("org.spark.RDD").methods(
        MapMethodScala("length").to("count"),
        MapMethodScala("top", 'compare, 'n).args('n).implicitArgs(MkOrdering('compare))
      ))

    MapModuleCxx("scalan.flint.DataFrames").withHeader("flint.h").withNamespace("flint").types(
      MapTypeCxx("FlintDataFrame", 'eT).to("data_frame").methods(
        MapMethodCxx("saveFile").to("output"),
        MapMethodCxx("top", 'compare, 'n).templateArgs('compare).args('n),
        MapMethodCxx("join", 'innerRdd, 'outerKey, 'innerKey, 'estimation, 'kind, 'eI, 'eK)
          .templateArgs('eI, 'eK, VoidInOut('outerKey), VoidInOut('innerKey))
          .args('innerRdd, 'estimation, EnumIndex('kind))),
      MapTypeCxx("InputDF").to("input_data_frame").methods())
  }

  test("Scala Method") {
    // TODO asInstanceOf should be possible to remove
    val scalaMethod = TestMethodMappingDSL.mappingDSLs(SCALA).head.getMethod("scalan.flint.DataFrames$FlintDataFrame", "length", None).get.asInstanceOf[(ScalaLibrary, ScalaType, ScalaMethod)]._3
    scalaMethod.mappedName should equal("count")
  }

  test("C++ Method") {
    val cxxMethod = TestMethodMappingDSL.mappingDSLs(CXX).head.getMethod("scalan.flint.DataFrames$FlintDataFrame", "length", None)
    cxxMethod should be(None)
//    cxxMethod.mappedName should equal("length")
  }
}