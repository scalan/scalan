package scalan.meta

import scalan.meta.ScalanAst.STraitCall

class BoilerplateTool {
  val coreTypeSynonims = Map(
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
    coreTypeSynonims,
    PartialFunction.empty
  )

  val liteTypeSynonims = Map(
    "PA" -> "PArray", "Vec" -> "Vector", "Matr" -> "Matrix"
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
    coreTypeSynonims ++ liteTypeSynonims,
    PartialFunction.empty
  )

  val eeTypeSynonims = Set(
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
    coreTypeSynonims ++ liteTypeSynonims ++ eeTypeSynonims,
    { case STraitCall("NA", Seq(t)) => STraitCall("PArray", List(STraitCall("PArray", List(t)))) }
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
