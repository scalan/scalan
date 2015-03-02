package scalan.meta

import com.typesafe.scalalogging.slf4j.StrictLogging

class BoilerplateTool extends StrictLogging {
  val coreTypeSynonyms = Map(
    "RThrow" -> "Throwable",
    "Arr" -> "Array",
    "MM" -> "MMap"
  )
  lazy val coreConfig = CodegenConfig(
    name = "core",
    srcPath = "../core/src/main/scala",
    entityFiles = List(
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

  val coreTestsTypeSynonyms = Map(
    "RSeg" -> "Segment"
  )
  lazy val coreTestsConfig = CodegenConfig(
    name = "coretests",
    srcPath = "../core/src/test/scala",
    entityFiles = List(
      "scalan/common/Segments.scala"
    ),
    baseContextTrait = "Scalan",
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._", "scala.reflect._",
      "scalan.common.Default"),
    coreTestsTypeSynonyms
  )

  val liteTypeSynonyms = Map(
    "PA" -> "PArray", "NA" -> "NArray", "Vec" -> "Vector", "Matr" -> "Matrix"
  )
  val collectTypeSynonyms = Map(
    "PG" -> "Graph" , "Coll" -> "Collection", "NColl" -> "NestedCollection"
  )
  lazy val ceConfig = CodegenConfig(
    name = "ce",
    srcPath = "../community-edition/src/main/scala",
    entityFiles = List(
      "scalan/parrays/PArrays.scala"
      , "scalan/collections/HashSets.scala"
      , "scalan/collections/Seqs.scala"
      , "scalan/linalgebra/Vectors.scala"
      , "scalan/linalgebra/Matrices.scala"
      , "scalan/collections/MultiMap.scala"
    ),
    baseContextTrait = "Scalan",
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._", "scala.reflect._",
      "scalan.common.Default"),
    coreTypeSynonyms ++ liteTypeSynonyms
  )

  val eeTypeSynonyms = Set(
    "PS" -> "PSet", "Dist" -> "Distributed"
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
    coreTypeSynonyms ++ liteTypeSynonyms ++ eeTypeSynonyms
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
    srcPath = "../../scalan-effects/src/main/scala",
    entityFiles = List(
      "scalan/monads/Frees.scala",
      "scalan/monads/Coproducts.scala",
      "scalan/examples/Interactions.scala",
      "scalan/examples/Auths.scala",
      "scalan/examples/IOs.scala",
      "scalan/monads/Readers.scala",
      "scalan/stream/Processes.scala"
    ),
    baseContextTrait = "Scalan",
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._", "scala.reflect._",
      "scalan.common.Default"),
    effectsTypeSynonims
  )

  lazy val collectionsConfig = CodegenConfig(
    name = "collections",
    srcPath = "../community-edition/src/main/scala",
    entityFiles = List(
      "scalan/collection/Collections.scala"
     ),
    baseContextTrait = "Scalan",
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._", "scala.reflect._",
      "scalan.common.Default"),
    collectTypeSynonyms
  )

  lazy val graphConfig = CodegenConfig(
    name = "graphs",
    srcPath = "../community-edition/src/main/scala",
    entityFiles = List(
      "scalan/graphs/Graphs.scala",
      "scalan/graphs/Vertices.scala",
      "scalan/graphs/Edges.scala"
    ),
    baseContextTrait = "Scalan",
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._", "scala.reflect._",
      "scalan.common.Default"),
    collectTypeSynonyms
  )

  lazy val frontsConfig = CodegenConfig(
    name = "fronts",
    srcPath = "../community-edition/src/main/scala",
    entityFiles = List(
      "scalan/graphs/Fronts.scala"
    ),
    baseContextTrait = "ScalanCommunityDsl",
    seqContextTrait = "ScalanCommunityDslSeq",
    stagedContextTrait = "ScalanCommunityDslExp",
    extraImports = List(
      "scala.reflect.runtime.universe._", "scala.reflect._",
      "scalan.common.Default"),
    Map()
  )

  def getConfigs(args: Array[String]): Seq[CodegenConfig] =
    args.flatMap { arg => configsMap.getOrElse(arg,
      sys.error(s"Unknown codegen config $arg. Allowed values: ${configsMap.keySet.mkString(", ")}"))
    }.distinct

  val configsMap = Map(
    "fronts" -> List(frontsConfig),
    "graphs" -> List(graphConfig),
    "collections" -> List(collectionsConfig),
    "coretests" -> List(coreTestsConfig),
    "core" -> List(coreConfig),
    "ce" -> List(ceConfig),
    "ee" -> List(eeConfig),
    "effects" -> List(effectsConfig),
    "ce-all" -> List(coreTestsConfig, coreConfig, ceConfig),
    "all" -> List(coreTestsConfig, ceConfig, eeConfig)
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
