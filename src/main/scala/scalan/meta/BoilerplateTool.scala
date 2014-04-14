/**
 * User: Alexander Slesarenko
 * Date: 12/1/13
 */
package scalan.meta

object BoilerplateTool extends App {
  val scalanConfig = CodegenConfig(
    isLite = false,
    srcPath = "../scalan/src",
    proxyTrait = "scalan.lms.common.ProxyExp",
    stagedViewsTrait = "scalan.staged.StagedViews",
    entityFiles = List(
      "main/scala/scalan/trees/Trees.scala",
      "main/scala/scalan/math/Matrices.scala",
      "main/scala/scalan/math/Vectors.scala",
      "main/scala/scalan/collections/Sets.scala"
    ),
    isoNames = ("A","B"),
    extraImports = List("scalan.common.Common")
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

  val ctx = new EntityManagement(liteConfig)
  ctx.generateAll
}
