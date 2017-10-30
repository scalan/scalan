package scalan.plugin

import java.io.File

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.io.{Path, PlainFile}
import scala.tools.nsc._
import scalan.meta.EntityManagement
import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstExtensions._
import scalan.meta.ScalanAstTransformers._
import scalan.util.CollectionUtil.TraversableOps
import scalan.util.FileUtil

object Assembler {
  val name = "scalanizer-assembler"
}

/** The component builds wrappers. */
//class Assembler(override val plugin: ScalanizerPlugin) extends ScalanizerComponent(plugin) {
//  import scalanizer._
//  import scalanizer.global._
//  val phaseName: String = Assembler.name
//
//  override def description: String = "Assemble virtualized units"
//  override val runsBefore = List("namer")
//  override val runsAfter: List[String] = List("parser")
//
//  def newPhase(prev: Phase) = new StdPhase(prev) {
//    def apply(unit: CompilationUnit) { }
//    override def run(): Unit = {
////      scalanizer.inform("copying ...")
////      val targetFile = "library-api/src/main/scala/scalan/Test.scala"
////      val sourceFile = "library-impl/src/main/resources/scalan/Test.scala"
////      if (!Path(targetFile).exists) {
////        FileUtil.copy(new File(sourceFile), new File(targetFile))
////        global.currentRun.compileLate(new PlainFile(Path(sourceFile)))
////      }
//    }
//  }
//
//}
