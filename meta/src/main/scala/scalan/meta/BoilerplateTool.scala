package scalan.meta

import scalan.meta.ScalanAst.STraitCall

class BoilerplateTool {
  val coreTypeSynonyms = Map(
    "Arr" -> "Array"
  )
  lazy val coreConfig = CodegenConfig(
    srcPath = "core/src/main/scala",
    entityFiles = List(
    ),
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._",
      "scalan.common.Default"),
    coreTypeSynonyms
  )

  val liteTypeSynonyms = Map(
    "PA" -> "PArray", "NA" -> "NArray", "Vec" -> "Vector", "Matr" -> "Matrix"
  )
  lazy val liteConfig = CodegenConfig(
    srcPath = "community-edition/src/main/scala",
    entityFiles = List(
      "scalan/parrays/PArrays.scala"
      , "scalan/linalgebra/Vectors.scala"
      , "scalan/linalgebra/Matrices.scala"
    ),
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._",
      "scalan.common.Default"),
    coreTypeSynonyms ++ liteTypeSynonyms
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
      "scalan/parrays/PArrays.scala"
    ),
    seqContextTrait = "ScalanEnterpriseSeq",
    stagedContextTrait = "ScalanEnterpriseExp",
    extraImports = List(
      "scala.reflect.runtime.universe._",
      "scalan.common.Default"),
    coreTypeSynonyms ++ liteTypeSynonyms ++ eeTypeSynonyms
  )

  def getConfigs(args: Array[String]) = args.flatMap(_ match {
    case "core" => List(coreConfig)
    case "lite" => List(liteConfig)
    case "ee" => List(scalanConfig)
    case "all" => List(coreConfig, liteConfig, scalanConfig)
    case _ => List(coreConfig)
  }).toSet.toSeq

  def main(args: Array[String]) {
    val configs = getConfigs(args)

    configs.foreach { new EntityManagement(_).generateAll() }
    println("Ok.")
  }
}

object BoilerplateToolRun extends BoilerplateTool {
}
