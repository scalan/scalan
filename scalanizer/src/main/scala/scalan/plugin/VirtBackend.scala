package scalan.plugin

import scalan.util.FileUtil
import scala.tools.nsc._

object VirtBackend {
  val name = "scalanizer-virt-backend"
}

/** Generating of Scala AST for wrappers. */
class VirtBackend(override val plugin: ScalanizerPlugin) extends ScalanizerComponent(plugin) {
  import scalanizer._
  import scalanizer.global._

  val phaseName: String = VirtBackend.name

  override def description: String = "Generating of Scala AST for virtualized cake."

  override val runsAfter = List(WrapBackend.name)

  def newPhase(prev: Phase) = new StdPhase(prev) {
    override def run(): Unit = {
      //      saveCombinedCake("scalanizer.linalgebra", "LinearAlgebra")
    }
    def apply(unit: CompilationUnit): Unit = ()
  }

//  def getCombinedCakeHome(namespace: String) = {
//    val namespacePath = namespace.split('.').mkString("/")
//    s"${snConfig.targetModuleFolder}/src/main/scala/$namespacePath/impl"
//  }

//  /** Puts all modules to the cakes <name>Dsl, <name>DslStd and <name>DslExp.
//    * Stores them into the file: <home>/impl/<name>Impl.scala */
//  def saveCombinedCake(namespace: String, name: String): Unit = {
//    implicit val genCtx = GenCtx(module = null, toRep = false)
//    val ns = genRefs(namespace.split('.').toList).asInstanceOf[RefTree]
//    val imports = List(q"import scalan._")
//    val absCake = q"trait ${TypeName(name + "Dsl")} extends Scalan with WrappersDsl with ColsDsl"
//    val stdCake =
//      q"""
//                  trait ${TypeName(name + "DslStd")}
//                    extends WrappersDslStd with ${TypeName(name + "Dsl")}
//                  """
//    val expCake =
//      q"""
//                  trait ${TypeName(name + "DslExp")}
//                    extends WrappersDslExp
//                    with ${TypeName(name + "Dsl")}
//                    with ColsDslExp
//                  """
//    val objectSE = q"object StagedEvaluation {..${imports ++ List(absCake, stdCake, expCake)}}"
//    val cake = PackageDef(ns,
//      List(PackageDef(
//        Ident(TermName("implOf" + name)),
//        List(
//          q"import wrappers._",
//          q"import scalanizer.collections.implOfCols.StagedEvaluation._",
//          objectSE)
//      )))
//    saveCombinedCake(namespace, name, showCode(cake))
//  }
//
//  def saveCombinedCake(namespace: String, name: String, cake: String): Unit = {
//    val cakeFile = FileUtil.file(getCombinedCakeHome(namespace), name + "Impl.scala")
//    cakeFile.mkdirs()
//    FileUtil.write(cakeFile, cake)
//  }
}
