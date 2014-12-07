package scalan.meta

object BoilerplateTool {
  val coreTypeSynonims = Set(
    "RepMonad", "Arr"
  )
  lazy val coreConfig = CodegenConfig(
    srcPath = "core/src/main/scala",
    entityFiles = List(
      "scalan/monads/Monads.scala"
    ),
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._",
      "scalan.common.Default"),
    coreTypeSynonims
  )

  val liteTypeSynonims = Set(
    "PA", "Vec", "Matr"
  )
  lazy val liteConfig = CodegenConfig(
    srcPath = "community-edition/src/main/scala",
    entityFiles = List(
      "scalan/parrays/PArrays.scala"
      ,"scalan/linalgebra/Vectors.scala"
      ,"scalan/linalgebra/Matrices.scala"
      //,"scalan/iterators/Iters.scala"
      //, "main/scala/scalan/rx/Trees.scala"
    ),
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._",
      "scalan.common.Default"),
    coreTypeSynonims ++ liteTypeSynonims
  )

  val eeTypeSynonims = Set(
    "Vec", "Matr", "PS", "Dist"
  )
  lazy val scalanConfig = CodegenConfig(
    srcPath = "../../scalan/src/main/scala",
    entityFiles = List(
      "scalan/trees/Trees.scala",
      "scalan/math/Matrices.scala",
      "scalan/math/Vectors.scala",
      "scalan/collections/Sets.scala",
      "scalan/dists/Dists.scala"
    ),
    seqContextTrait = "ScalanEnterpriseSeq",
    stagedContextTrait = "ScalanEnterpriseExp",
    extraImports = List(
      "scala.reflect.runtime.universe._",
      "scalan.common.Default"),
    coreTypeSynonims ++ liteTypeSynonims ++ eeTypeSynonims
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
