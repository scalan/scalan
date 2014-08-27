package scalan.meta

object BoilerplateTool {
  lazy val scalanConfig = CodegenConfig(
    srcPath = "../scalan/src/main/scala",
    entityFiles = List(
      "scalan/trees/Trees.scala",
      "scalan/math/Matrices.scala",
      "scalan/math/Vectors.scala",
      "scalan/collections/Sets.scala"
    ),
    seqContextTrait = "ScalanEnterpriseSeq",
    stagedContextTrait = "ScalanEnterpriseExp",
    extraImports = List(
      "scala.reflect.runtime.universe._",
      "scalan.common.Default")
  )

  lazy val liteConfig = CodegenConfig(
    srcPath = "../scalan-lite/core/src/main/scala",
    entityFiles = List(
      "../../../../community-edition/src/main/scala/scalan/arrays/PArrays.scala"
      ,"scalan/types/Types.scala"
      ,"../../../../community-edition/src/main/scala/scalan/linalgebra/Vectors.scala"
      ,"../../../../community-edition/src/main/scala/scalan/linalgebra/Matrices.scala"
      //,"scalan/iterators/Iters.scala"
      //, "main/scala/scalan/rx/Trees.scala"
    ),
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._", 
      "scalan.common.Default")
  )


  def main(args: Array[String]) {
    lazy val ctx = new EntityManagement(scalanConfig)

    ctx.generateAll
  }
}
