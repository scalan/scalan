/**
 * User: Alexander Slesarenko
 * Date: 12/1/13
 */
package scalan.meta

object BoilerplateTool extends App {
  val scalanConfig = CodegenConfig(
    isLite = false,
    srcPath = "../scalan/src",
    stagedViewsTrait = "StagedViews",
    entityFiles = List(
      "main/scala/scalan/trees/Trees.scala",
      "main/scala/scalan/math/Matrices.scala",
      "main/scala/scalan/collections/Sets.scala"
    ),
    isoNames = ("A","B")
  )

  val liteConfig = CodegenConfig(
    isLite = true,
    srcPath = "../scalan-lite/src",
    entityFiles = List(
      "main/scala/scalan/rx/Reactive.scala"
      //, "main/scala/scalan/rx/Trees.scala"
    ),
    stagedViewsTrait = "ViewsExp",
    isoNames = ("From", "To")
  )

  val ctx = new EntityManagement(scalanConfig)
  ctx.generateAll
}
