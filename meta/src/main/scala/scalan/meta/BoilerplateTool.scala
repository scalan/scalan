package scalan.meta

import com.typesafe.scalalogging.slf4j.StrictLogging

class BoilerplateTool extends StrictLogging {
  val scalanTypeSynonyms = Map(
    "Conv" -> "Converter"
  )
  lazy val scalanConfig = CodegenConfig(
    name = "scalan",
    srcPath = "../core/src/main/scala",
    entityFiles = List(
      "scalan/Converters.scala"
    ),
    scalanTypeSynonyms,
    baseContextTrait = "", // not defined means not declare
    seqContextTrait = "",
    stagedContextTrait = ""
  )

  val coreTypeSynonyms = scalanTypeSynonyms ++ Map(
    "RThrow" -> "Throwable",
    "Arr" -> "Array",
    "MM" -> "MMap",
    "IntRep" -> "Int",
    "DoubleRep" -> "Double",
    "BoolRep" -> "Boolean",
    "UnitRep" -> "Unit",
    "NothingRep" -> "Nothing",
    "ByteRep" -> "Byte",
    "ShortRep" -> "Short",
    "CharRep" -> "Char",
    "LongRep" -> "Long",
    "FloatRep" -> "Float",
    "DoubleRep" -> "Double",
    "ROption" -> "SOption"
  )
  lazy val coreConfig = CodegenConfig(
    name = "core",
    srcPath = "../core/src/main/scala",
    entityFiles = List(
      "scalan/primitives/AbstractStrings.scala",
      "scalan/util/Exceptions.scala"
    ),
    coreTypeSynonyms
  )

  val coreTestsTypeSynonyms = coreTypeSynonyms ++ Map(
    "RSeg" -> "Segment"
  )
  lazy val coreTestsConfig = CodegenConfig(
    name = "coretests",
    srcPath = "../core/src/test/scala",
    entityFiles = List(
      "scalan/common/Segments.scala"
      , "scalan/common/Kinds.scala"
    ),
    coreTestsTypeSynonyms
  )

  val collectTypeSynonyms = coreTypeSynonyms ++ Map(
    "Coll" -> "Collection", "PairColl" -> "PairCollection", "NColl" -> "NestedCollection"
  )
  lazy val libConfig = CodegenConfig(
    name = "lib",
    srcPath = "../library/src/main/scala",
    entityFiles = List(
       "scalan/collections/HashSets.scala"
      , "scalan/collections/Seqs.scala"
      , "scalan/collections/MultiMap.scala"
      , "scalan/collections/BitSets.scala"
    ),
    collectTypeSynonyms
  )

  val laTypeSynonyms = collectTypeSynonyms ++ Map(
    "Vector" -> "AbstractVector", "Matrix" -> "AbstractMatrix"
  )
  lazy val laConfig = CodegenConfig(
    name = "la",
    srcPath = "../library/src/main/scala",
    entityFiles = List(
        "scalan/linalgebra/Vectors.scala"
      , "scalan/linalgebra/Matrices.scala"
    ),
    laTypeSynonyms
  )

  val eeTypeSynonyms = coreTypeSynonyms ++ Map(
    "PS" -> "PSet", "Dist" -> "Distributed", "PA" -> "PArray", "NA" -> "NestedArray", "PM" -> "PMap",
    "Vec" -> "Vector", "Matr" -> "Matrix"
  )
  lazy val eeConfig = CodegenConfig(
    name = "ee",
    srcPath = "../../scalan/src/main/scala",
    entityFiles = List(
      "scalan/trees/Trees.scala",
      "scalan/math/Matrices.scala",
      "scalan/math/Vectors.scala",
      "scalan/collections/PSets.scala",
      "scalan/dists/Dists.scala",
      "scalan/parrays/PArrays.scala"
    ),
    eeTypeSynonyms,
    baseContextTrait = "scalan.ScalanEnterprise",
    seqContextTrait = "scalan.ScalanEnterpriseSeq",
    stagedContextTrait = "scalan.ScalanEnterpriseExp"
  )

  val effectsTypeSynonyms = Map(
    "MonadRep"    -> "Monad",
    "RFree"       -> "Free",
    "RCoproduct"  -> "Coproduct",
    "RepReader" -> "Reader",
    "RepInteract" -> "Interact",
    "RepAuth" -> "Auth"
  )
  lazy val effectsConfig = CodegenConfig(
    name = "effects",
    srcPath = "../library/src/test/scala/scalan/effects",
    entityFiles = List(
      "Frees.scala",
      "Coproducts.scala",
      "Interactions.scala",
      "Auths.scala",
      "IOs.scala",
      "Readers.scala",
      "Processes.scala"
    ),
    effectsTypeSynonyms
  )

  val effects2TypeSynonyms = Map(
    "RFree"       -> "Free",
    "RCoproduct"  -> "Coproduct",
    "RepInteract" -> "Interact",
    "RepAuth" -> "Auth"
  )
  lazy val effects2Config = CodegenConfig(
    name = "effects2",
    srcPath = "../library/src/test/scala/",
    entityFiles = List(
      "scalan/effects/IOs.scala",
      "scalan/effects/Readers.scala",
      "scalan/effects/States.scala",
      "scalan/effects/FreeStates.scala",
      "scalan/effects/FreeMs.scala",
      "scalan/effects/Processes.scala",
      "scalan/effects/Frees.scala",
      "scalan/effects/Coproducts.scala",
      "scalan/effects/Interactions.scala",
      "scalan/effects/Auths.scala"
    ),
    effects2TypeSynonyms
  )

  lazy val collectionsConfig = CodegenConfig(
    name = "collections",
    srcPath = "../library/src/main/scala",
    entityFiles = List(
      "scalan/collections/Collections.scala"
     ),
    collectTypeSynonyms
  )

  val graphTypeSynonyms = collectTypeSynonyms ++ Map("PG" -> "Graph", "REdge" -> "EdgeType")
  lazy val graphConfig = CodegenConfig(
    name = "graphs",
    srcPath = "../library/src/main/scala",
    entityFiles = List(
      "scalan/graphs/Graphs.scala",
      "scalan/graphs/Vertices.scala",
      "scalan/graphs/Edges.scala",
      "scalan/graphs/Fronts.scala"
    ),
    graphTypeSynonyms
  )

  val metaTestTypeSynonyms = coreTypeSynonyms ++ Map(
    "RMetaTest" -> "MetaTest"
  )
  lazy val metaTestConfig = CodegenConfig(
    name = "metaTest",
    srcPath = "../core/src/test/scala",
    entityFiles = List(
      "scalan/common/MetaTests.scala"
    ),
    metaTestTypeSynonyms
  )

  def getConfigs(args: Array[String]): Seq[CodegenConfig] =
    args.flatMap { arg => configsMap.getOrElse(arg,
      sys.error(s"Unknown codegen config $arg. Allowed values: ${configsMap.keySet.mkString(", ")}"))
    }.distinct

  val configsMap = Map(
    "scalan" -> List(scalanConfig),
    "core" -> List(coreConfig),
    "coretests" -> List(coreTestsConfig),
    "lib" -> List(libConfig),
    "collections" -> List(collectionsConfig),
    "la" -> List(laConfig),
    "graphs" -> List(graphConfig),
    "mt" -> List(metaTestConfig),
    "ee" -> List(eeConfig),
    "effects" -> List(effectsConfig),
    "effects2" -> List(effects2Config),
    "lib-all" -> List(scalanConfig, coreConfig, coreTestsConfig, libConfig, collectionsConfig, laConfig, graphConfig, metaTestConfig, effects2Config),
    "all" -> List(scalanConfig, coreConfig, coreTestsConfig, libConfig, collectionsConfig, laConfig, graphConfig, metaTestConfig, effects2Config, eeConfig)
  )

  def main(args: Array[String]) {
    val configs = getConfigs(args)

    if (configs.isEmpty) {
      logger.warn("BoilerplateTool run without configs")
    } else {
      for (c <- configs) {
        println(s"Processing ${c.srcPath}")
        new EntityManagement(c).generateAll()
        println(s"Ok\n")
      }
    }
  }
}

object BoilerplateToolRun extends BoilerplateTool {
}
