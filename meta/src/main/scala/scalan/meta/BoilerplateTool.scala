package scalan.meta

import com.typesafe.scalalogging.StrictLogging

class BoilerplateTool extends StrictLogging {
  val scalanTypeSynonyms = Map(
    "Conv" -> "Converter"
  )
  lazy val scalanConfig = CodegenConfig(
    name = "scalan",
    srcPath = "../core/src/main/scala",
    entityFiles = List(
      "scalan/Converters.scala",
      "scalan/Views.scala"
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
    "BoolRep" -> "Boolean",
    "ROption" -> "SOption"
  )
  lazy val coreConfig = CodegenConfig(
    name = "core",
    srcPath = "../core/src/main/scala",
    entityFiles = List(
      "scalan/util/Exceptions.scala"
    ),
    coreTypeSynonyms,
    baseContextTrait = "scalan.Scalan",
    seqContextTrait = "scalan.ScalanStd",
    stagedContextTrait = "scalan.Scalan"
  )

  val coreTestsTypeSynonyms = coreTypeSynonyms ++ Map(
    "RSeg" -> "Segment",
    "RMetaTest" -> "MetaTest"
  )
  lazy val coreTestsConfig = CodegenConfig(
    name = "coretests",
    srcPath = "../core/src/test/scala",
    entityFiles = List(
      "scalan/common/Segments.scala",
      "scalan/common/Kinds.scala",
      "scalan/common/MetaTests.scala"
    ),
    coreTestsTypeSynonyms
  )

  val specTypeSynonyms = coreTypeSynonyms ++ Map(
    "RepIsoFunc" -> "IsoFunc"
  )
  lazy val specConfig = CodegenConfig(
    name = "spec",
    srcPath = "../core/src/main/scala",
    entityFiles = List(
      "scalan/dynamic/Specializations.scala"
    ),
    specTypeSynonyms
  )

  val collectTypeSynonyms = coreTypeSynonyms ++ Map(
    "Coll" -> "Collection", "PairColl" -> "PairCollection", "NColl" -> "NestedCollection"
  )
  lazy val collectionsConfig = CodegenConfig(
    name = "collections",
    srcPath = "../collections/src/main/scala",
    entityFiles = List(
       "scalan/collections/HashSets.scala"
      , "scalan/collections/Seqs.scala"
      , "scalan/collections/MultiMap.scala"
      , "scalan/collections/BitSets.scala"
      , "scalan/collections/Collections.scala"
    ),
    collectTypeSynonyms
  )

  val laTypeSynonyms = collectTypeSynonyms ++ Map(
    "Vec" -> "Vector", "Matr" -> "Matrix"
  )
  lazy val laConfig = CodegenConfig(
    name = "la",
    srcPath = "../linear-algebra/src/main/scala",
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
    seqContextTrait = "scalan.ScalanEnterpriseStd",
    stagedContextTrait = "scalan.ScalanEnterpriseExp"
  )

  lazy val effectsConfig = CodegenConfig(
    name = "effects",
    srcPath = "../effects/src/test/scala/",
    entityFiles = List(
      "scalan/monads/IOs.scala",
      "scalan/monads/Readers.scala",
      "scalan/monads/States.scala",
      "scalan/monads/FreeStates.scala",
      "scalan/monads/FreeMs.scala",
      "scalan/monads/Processes.scala",
      "scalan/monads/Frees.scala",
      "scalan/monads/Coproducts.scala",
      "scalan/monads/Interactions.scala",
      "scalan/monads/Auths.scala"
    )
  )

  val graphTypeSynonyms = collectTypeSynonyms ++ Map("PG" -> "Graph", "REdge" -> "EdgeType")
  lazy val graphConfig = CodegenConfig(
    name = "graphs",
    srcPath = "../graphs/src/main/scala",
    entityFiles = List(
      "scalan/graphs/Graphs.scala",
      "scalan/graphs/Vertices.scala",
      "scalan/graphs/Edges.scala",
      "scalan/graphs/Fronts.scala"
    ),
    graphTypeSynonyms
  )

  lazy val structsConfig = CodegenConfig(
    name = "structs",
    srcPath = "../core/src/main/scala",
    entityFiles = List(
      "scalan/primitives/StructKeys.scala",
      "scalan/primitives/StructItems.scala"
    ),
    baseContextTrait = "", // not defined means not declare
    seqContextTrait = "",
    stagedContextTrait = ""
  )

  def getConfigs(args: Array[String]): Seq[CodegenConfig] =
    args.flatMap { arg => configsMap.getOrElse(arg,
      sys.error(s"Unknown codegen config $arg. Allowed values: ${configsMap.keySet.mkString(", ")}"))
    }.distinct

  val configsMap = Map(
    scalanConfig.name -> List(scalanConfig),
    structsConfig.name -> List(structsConfig),
    coreConfig.name -> List(coreConfig),
    coreTestsConfig.name -> List(coreTestsConfig),
    "allcore" -> List(scalanConfig, coreConfig, structsConfig, coreTestsConfig, specConfig),
  "collections" -> List(collectionsConfig),
  "la" -> List(laConfig),
  "graphs" -> List(graphConfig),
  "ee" -> List(eeConfig),
  "effects" -> List(effectsConfig),
  "lib-all" -> List(scalanConfig, coreConfig, coreTestsConfig, collectionsConfig, laConfig, graphConfig, effectsConfig, structsConfig),
  "all" -> List(scalanConfig, coreConfig, coreTestsConfig, collectionsConfig, laConfig, graphConfig, effectsConfig, structsConfig)
  )

  def main(args: Array[String]) {
    val configs = getConfigs(args)

    if (configs.isEmpty) {
      logger.warn("BoilerplateTool run without configs")
    } else {
      for (c <- configs) {
        println(s"Processing ${c.srcPath}")
        entityManagement(c).generateAll()
        println(s"Ok\n")
      }
    }
  }

  def entityManagement(c: CodegenConfig): EntityManagement = new EntityManagement(c)
}

object BoilerplateToolRun extends BoilerplateTool {
}
