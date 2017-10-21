package scalan.compilation.kotlin

import scala.WArraysModule
import scala.impl.WArraysModule
import scalan.compilation.{IndentLevel, CodegenConfig}
import scalan.util.FileUtil
import scalan.{BaseNestedTests, Scalan}
import scalanizer.collections.ColsModule
import scalanizer.collections.impl.ColsModule

class KotlinFileCodegenTests extends BaseNestedTests {
  val ctx = new Scalan with WArraysModule with ColsModule
  val config = CodegenConfig("kotlin-backend/generated", Seq(ColsModule.name))
  val gen = new KotlinFileCodegen(ctx, config)

  it("generateModules") {
    for ((mi, m) <- gen.modules) {
      val name = m.name
      val pn = m.packageName.split('.').mkString("/")
      val moduleFile = FileUtil.file(config.basePath, pn, s"$name.kt")
      FileUtil.withFile(moduleFile) { implicit writer =>
        implicit val ident = IndentLevel(0)
        println(moduleFile)
        gen.emitModule(mi)
      }
    }
  }
}
