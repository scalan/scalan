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
    baseContextTrait = "", // not defined means not declare
    seqContextTrait = "",
    stagedContextTrait = "",
    extraImports = List(
      "scala.reflect.runtime.universe._", "scala.reflect._",
      "scalan.common.Default"),
    scalanTypeSynonyms
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
    "DoubleRep" -> "Double"
  )
  lazy val coreConfig = CodegenConfig(
    name = "core",
    srcPath = "../core/src/main/scala",
    entityFiles = List(
      "scalan/primitives/AbstractStrings.scala",
      "scalan/util/Exceptions.scala"
    ),
    baseContextTrait = "Scalan",
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._", "scala.reflect._",
      "scalan.common.Default"),
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
    baseContextTrait = "Scalan",
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._", "scala.reflect._",
      "scalan.common.Default"),
    coreTestsTypeSynonyms
  )

  val collectTypeSynonyms = coreTypeSynonyms ++ Map(
    "Coll" -> "Collection", "PairColl" -> "PairCollection", "NColl" -> "NestedCollection"
  )
  lazy val ceConfig = CodegenConfig(
    name = "ce",
    srcPath = "../community-edition/src/main/scala",
    entityFiles = List(
       "scalan/collections/HashSets.scala"
      , "scalan/collections/Seqs.scala"
      , "scalan/collections/MultiMap.scala"
//      , "scalan/collections/BitSets.scala"
    ),
    baseContextTrait = "Scalan",
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._", "scala.reflect._",
      "scalan.common.Default"),
    collectTypeSynonyms
  )

  val laTypeSynonyms = collectTypeSynonyms ++ Map(
    "Vector" -> "AbstractVector", "Matrix" -> "AbstractMatrix"
  )
  lazy val laConfig = CodegenConfig(
    name = "la",
    srcPath = "../community-edition/src/main/scala",
    entityFiles = List(
//        "scalan/linalgebra/Vectors.scala"
//      , "scalan/linalgebra/Matrices.scala"
    ),
    baseContextTrait = "Scalan",
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._", "scala.reflect._",
      "scalan.common.Default"),
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
    baseContextTrait = "ScalanEnterprise",
    seqContextTrait = "ScalanEnterpriseSeq",
    stagedContextTrait = "ScalanEnterpriseExp",
    extraImports = List(
      "scala.reflect.runtime.universe._", "scala.reflect._",
      "scalan.common.Default"),
    eeTypeSynonyms
  )

  val effectsTypeSynonims = Map(
    "MonadRep"    -> "Monad",
    "RFree"       -> "Free",
    "RCoproduct"  -> "Coproduct",
    "RepReader" -> "Reader",
    "RepInteract" -> "Interact",
    "RepAuth" -> "Auth"
    // declare your type synonims for User Defined types here (see type PA[A] = Rep[PArray[A]])
  )
  lazy val effectsConfig = CodegenConfig(
    name = "effects",
    srcPath = "../community-edition/src/test/scala/scalan/effects",
    entityFiles = List(
      "Frees.scala",
      "Coproducts.scala",
      "Interactions.scala",
      "Auths.scala",
      "IOs.scala",
      "Readers.scala",
      "Processes.scala"
    ),
    baseContextTrait = "Scalan",
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._", "scala.reflect._",
      "scalan.common.Default"),
    effectsTypeSynonims
  )

  val effects2TypeSynonims = Map(
    "RFree"       -> "Free",
    "RCoproduct"  -> "Coproduct",
    "RepInteract" -> "Interact",
    "RepAuth" -> "Auth"
    // declare your type synonims for User Defined types here (see type PA[A] = Rep[PArray[A]])
  )
  lazy val effects2Config = CodegenConfig(
    name = "effects2",
    srcPath = "../community-edition/src/test/scala/",
    entityFiles = List(
      "scalan/effects/IOs.scala",
      "scalan/effects/Readers.scala",
      "scalan/effects/States.scala",
//      "scalan/effects/FreeStates.scala",
      "scalan/effects/FreeMs.scala",
      "scalan/effects/Processes.scala",
      "scalan/effects/Frees.scala",
      "scalan/effects/Coproducts.scala",
      "scalan/effects/Interactions.scala",
      "scalan/effects/Auths.scala"
    ),
    baseContextTrait = "Scalan",
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._", "scala.reflect._",
      "scalan.common.Default"),
    effects2TypeSynonims
  )

  lazy val collectionsConfig = CodegenConfig(
    name = "collections",
    srcPath = "../community-edition/src/main/scala",
    entityFiles = List(
      "scalan/collections/Collections.scala"
     ),
    baseContextTrait = "Scalan",
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._", "scala.reflect._",
      "scalan.common.Default"),
    collectTypeSynonyms
  )

  val graphTypeSynonyms = collectTypeSynonyms ++ Map("PG" -> "Graph", "REdge" -> "EdgeType")
  lazy val graphConfig = CodegenConfig(
    name = "graphs",
    srcPath = "../community-edition/src/main/scala",
    entityFiles = List(
      "scalan/graphs/Graphs.scala",
      "scalan/graphs/Vertices.scala",
      "scalan/graphs/Edges.scala",
      "scalan/graphs/Fronts.scala"
    ),
    baseContextTrait = "Scalan",
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._", "scala.reflect._",
      "scalan.common.Default"),
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
    baseContextTrait = "Scalan",
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scalan.{Scalan, ScalanSeq, ScalanExp}",
      "scala.reflect.runtime.universe._", "scala.reflect._",
      "scalan.common.Default"),
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
    "ce" -> List(ceConfig),
    "collections" -> List(collectionsConfig),
    "la" -> List(laConfig),
    "graphs" -> List(graphConfig),
    "mt" -> List(metaTestConfig),
    "ee" -> List(eeConfig),
    "effects" -> List(effectsConfig),
    "effects2" -> List(effects2Config),
    "ce-all" -> List(scalanConfig, coreConfig, coreTestsConfig, ceConfig, collectionsConfig, laConfig, graphConfig, metaTestConfig, effects2Config),
    "all" -> List(scalanConfig, coreConfig, coreTestsConfig, ceConfig, collectionsConfig, laConfig, graphConfig, metaTestConfig, effects2Config, eeConfig)
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
