package scalan.meta

import scalan.meta.ScalanAst.STraitCall

class BoilerplateTool {
  val coreTypeSynonyms = Map(
    "RThrow" -> "Throwable"
  )
  lazy val coreConfig = CodegenConfig(
    srcPath = "../core/src/main/scala",
    entityFiles = List(
      "scalan/util/Exceptions.scala"
    ),
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._",
      "scalan.common.Default"),
    coreTypeSynonyms
  )

  val coreTestsTypeSynonyms = Map(
    "RSeg" -> "Segment"
  )
  lazy val coreTestsConfig = CodegenConfig(
    srcPath = "../core/src/test/scala",
    entityFiles = List(
      "scalan/common/Segments.scala"
    ),
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._",
      "scalan.common.Default"),
    coreTestsTypeSynonyms
  )

  val liteTypeSynonyms = Map(
    "PA" -> "PArray", "NA" -> "NArray", "Vec" -> "Vector", "Matr" -> "Matrix"
  )
  lazy val liteConfig = CodegenConfig(
    srcPath = "../community-edition/src/main/scala",
    entityFiles = List(
      "scalan/parrays/PArrays.scala"
      , "scalan/collection/HashSets.scala"
      , "scalan/linalgebra/Vectors.scala"
      , "scalan/linalgebra/Matrices.scala"
    ),
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._",
      "scalan.common.Default"),
    coreTestsTypeSynonyms ++ liteTypeSynonyms
  )

  val eeTypeSynonyms = Set(
    "PS" -> "PSet", "PM" -> "PMap", "Dist" -> "Distributed"
  )
  lazy val scalanConfig = CodegenConfig(
    srcPath = "../../scalan/src/main/scala",
    entityFiles = List(
      "scalan/trees/Trees.scala",
      "scalan/math/Matrices.scala",
      "scalan/math/Vectors.scala",
      "scalan/collections/Sets.scala",
      "scalan/dists/Dists.scala",
    // don't regenerate by default because this will break
    // FuncArray. See comments there.
//      "scalan/parrays/PArrays.scala",
      "scalan/iterators/Iterators.scala"
    ),
    seqContextTrait = "ScalanEnterpriseSeq",
    stagedContextTrait = "ScalanEnterpriseExp",
    extraImports = List(
      "scala.reflect.runtime.universe._",
      "scalan.common.Default"),
    coreTestsTypeSynonyms ++ liteTypeSynonyms ++ eeTypeSynonyms
  )
  lazy val scalanFullConfig = scalanConfig.copy(entityFiles = scalanConfig.entityFiles :+ "scalan/parrays/PArrays.scala")

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
    srcPath = "../../scalan-effects/src/main/scala",
    entityFiles = List(
      //"scalan/monads/Monads.scala"
      //, "scalan/monads/Functors.scala"
      "scalan/monads/FreeMs.scala"
      //"scalan/io/Frees.scala"
      //"scalan/monads/Coproducts.scala"
      //"scalan/monads/Interactions.scala"
      //"scalan/monads/Auths.scala"
      //"scalan/monads/Readers.scala"
    ),
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._",
      "scalan.common.Default"),
    effectsTypeSynonims
  )

  def getConfigs(args: Array[String]): Seq[CodegenConfig] =
    args.flatMap { arg => configsMap.getOrElse(arg,
      sys.error(s"Unknown codegen config $arg. Allowed values: ${configsMap.keySet.mkString(", ")}"))
    }.distinct

  val configsMap = Map(
    "coretests" -> List(coreTestsConfig),
    "core" -> List(coreConfig),
    "ce" -> List(liteConfig),
    "ee" -> List(scalanConfig),
    "ee-full" -> List(scalanFullConfig),
    "effects" -> List(effectsConfig),
    "ce-all" -> List(coreTestsConfig, coreConfig, liteConfig),
    "all" -> List(coreTestsConfig, liteConfig, scalanConfig)
  )

  def main(args: Array[String]) {
    val configs = getConfigs(args)

    for (c <- configs) {
      println(s"Processing ${c.srcPath}")
      new EntityManagement(c).generateAll()
      println(s"Ok\n")
    }
  }
}

object BoilerplateToolRun extends BoilerplateTool {
}
