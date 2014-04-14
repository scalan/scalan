/**
 * User: Alexander Slesarenko
 * Date: 12/1/13
 */
package scalan.meta

object BoilerplateTool extends App {
  val scalanConfig = CodegenConfig(
    isLite = false,
    srcPath = "../scalan/src/main/scala",
    proxyTrait = "scalan.lms.common.ProxyExp",
    stagedViewsTrait = "scalan.staged.StagedViews",
    entityFiles = List(
      "scalan/trees/Trees.scala",
      "scalan/math/Matrices.scala",
      "scalan/math/Vectors.scala",
      "scalan/collections/Sets.scala"
    ),
    isoNames = ("A","B"),
    extraImports = List(
      "scala.reflect.runtime.universe._", 
      "scalan.common.Common",
      "scalan.staged.ScalanStaged",
      "scalan.sequential.ScalanSeq")
  )

  val liteConfig = CodegenConfig(
    isLite = true,
    srcPath = "../scalan-lite/core/src/main/scala",
    entityFiles = List(
      "scalan/arrays/PArrays.scala",
      "scalan/types/Types.scala"
      //, "main/scala/scalan/rx/Trees.scala"
    ),
    proxyTrait = "scalan.ProxyExp",
    stagedViewsTrait = "scalan.ViewsExp",
    isoNames = ("From", "To"),
    extraImports = List(
      "scala.reflect.runtime.universe._", 
      "scalan.common.Default.defaultVal")
  )

  val ctx = new EntityManagement(scalanConfig)
  ctx.generateAll
}
