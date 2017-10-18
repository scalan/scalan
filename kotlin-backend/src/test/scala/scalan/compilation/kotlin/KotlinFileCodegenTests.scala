package scalan.compilation.kotlin

import scalan.compilation.{CodegenConfig, IndentLevel}
import scalan.util.FileUtil
import scalan.{BaseNestedTests, Scalan}

class KotlinFileCodegenTests extends BaseNestedTests {
  val config = CodegenConfig("kotlin-backend/generated")
  val ctx = new Scalan
  val gen = new KotlinFileCodegen(ctx, config)

  describe("translateToSrc") {
    for ((name, m) <- gen.modules) {
      val pn = m.packageName.split('.').mkString("/")
      val moduleFile = FileUtil.file(config.basePath, pn, s"$name.kt")
      FileUtil.withFile(moduleFile) { implicit writer =>
        implicit val ident = IndentLevel(0)
        println(moduleFile)
        gen.emitModule(name)
      }
    }
  }
}
