/**
 * User: Alexander Slesarenko
 * Date: 12/1/13
 */
package scalan.meta

object BoilerplateTool extends App {
  val defConf = CodegenConfig.default

  val scalanConfig = defConf.copy(
    srcPath = "../scalan/src",
    entityFiles = List(
      "main/scala/scalan/trees/Trees.scala",
      "main/scala/scalan/math/Matrices.scala",
      "main/scala/scalan/collections/Sets.scala"
    ),
    isoNames = ("A","B")
  )

  val liteConfig = defConf.copy(
    srcPath = "/home/s00747473/Projects/scalan-lite/src",
    entityFiles = List(
      "main/scala/scalan/rx/Reactive.scala"
      //, "main/scala/scalan/rx/Trees.scala"
    ),
    stagedViewsTrait = "ViewsExp"
  )

  val ctx = new EntityManagement(scalanConfig)
  ctx.generateAll
}
