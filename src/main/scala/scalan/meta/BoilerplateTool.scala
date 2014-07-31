package scalan.meta

object BoilerplateTool {
  lazy val scalanConfig = CodegenConfig(
    isLite = true,
    srcPath = "../scalan/src/main/scala",
    proxyTrait = "scalan.ProxyExp",
    stagedViewsTrait = "scalan.ViewsExp",
    entityFiles = List(
      "scalan/trees/Trees.scala",
      "scalan/math/Matrices.scala",
      "scalan/math/Vectors.scala",
      "scalan/collections/Sets.scala"
    ),
    extraImports = List(
      "scala.reflect.runtime.universe._", 
      "scalan.common.Common",
      "scalan.staged.ScalanStaged",
      "scalan.sequential.ScalanSeq")
  )

  lazy val liteConfig = CodegenConfig(
    isLite = true,
    srcPath = "../scalan-lite/core/src/main/scala",
    entityFiles = List(
      "../../../../community-edition/src/main/scala/scalan/arrays/PArrays.scala"
      ,"scalan/types/Types.scala"
      ,"../../../../community-edition/src/main/scala/scalan/linalgebra/Vectors.scala"
      ,"../../../../community-edition/src/main/scala/scalan/linalgebra/Matrices.scala"
      //,"scalan/iterators/Iters.scala"
      //, "main/scala/scalan/rx/Trees.scala"
    ),
    proxyTrait = "scalan.ProxyExp",
    stagedViewsTrait = "scalan.ViewsExp",
    extraImports = List(
      "scala.reflect.runtime.universe._", 
      "scalan.common.Default.defaultVal")
  )


  def main(args: Array[String]) {
    lazy val ctx = new EntityManagement(liteConfig)

    ctx.generateAll
  }
}
