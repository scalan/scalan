package scalan.plugin

import java.io.File

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.NoPosition
import scala.tools.nsc._
import scala.tools.nsc.plugins.{Plugin, PluginComponent, PluginLoadException}
import scalan.meta.{EntityManagement, TargetModuleConf, SourceModuleConf, ModuleConf}
import scalan.meta.ScalanAst.AstContext
import scalan.meta.scalanizer.{ScalanizerConfig, Scalanizer, ScalanizerState}
import scalan.util.FileUtil
import scalan.util.StringUtil.StringUtilExtensions

abstract class ScalanPlugin(val global: Global) extends Plugin { plugin =>
}

class ScalanizerPlugin(g: Global) extends ScalanPlugin(g) { plugin =>
  val scalanizer: Scalanizer[plugin.global.type] = new Scalanizer[plugin.global.type] {
    def getGlobal: plugin.global.type = plugin.global

    val snConfig: ScalanizerConfig = new ScalanizerPluginConfig
    val context: AstContext = new AstContext(snConfig.unitConfigs, this)
    val entityManagment: EntityManagement[plugin.global.type] = new EntityManagement(this)
    val snState: ScalanizerState[plugin.global.type] = new ScalanizerPluginState(this)

    def informModuleNameError(msg: String) = {
      val run = global.currentRun
      val fullMsg = s"Cannot get module name for current $run: $msg"
      throw new PluginLoadException(global.pluginDescriptions, fullMsg)
    }

    override lazy val moduleName: String = {
      import ModuleConf._
      val run = global.currentRun
      val anySource = run.compiledFiles.headOption
      anySource match {
        case Some(file) =>
          var name = List(SourcesDir, TestsDir).find(!FileUtil.extractModuleName(file, _).isNullOrEmpty)
          name.getOrElse(
            informModuleNameError(
              s"Source file is not in expected location sources directory ${ModuleConf.SourcesDir } or ${ModuleConf.TestsDir }"))
        case None =>
          informModuleNameError("there is no compiledFiles")
      }
    }
  }
  /** Visible name of the plugin */
  val name: String = "scalanizer"
  val srcPipeline = new SourceModulePipeline(scalanizer)
  val targetPipeline = new TargetModulePipeline(scalanizer)

  def getPipelineComponents(pipeline: ScalanizerPipeline[Global]) = {
    val res = ListBuffer[PluginComponent]()
    var after = pipeline.runAfter
    pipeline.steps.foreach { step =>
      val comp = step.asInstanceOf[pipeline.PipelineStep] match {
        case runStep: pipeline.RunStep =>
          pipeline.forRunComponent(after, runStep)
        case unitStep: pipeline.ForEachUnitStep =>
          pipeline.forEachUnitComponent(after, unitStep)
      }
      after = List(step.name)
      res.append(comp)
    }
    res.result()
  }

  /** The compiler components that will be applied when running this plugin */
  val components: List[PluginComponent] = {
    val res = ListBuffer[PluginComponent]()
    res.append(getPipelineComponents(srcPipeline): _*)
    res.append(getPipelineComponents(targetPipeline): _*)
    res.result()
  }
  /** The description is printed with the option: -Xplugin-list */
  val description: String = "Perform code virtualization for Scalan"

  /** Plugin-specific options without -P:scalanizer:  */
  override def init(options: List[String], error: String => Unit): Boolean = {
    var enabled = true
    options foreach {
      case "debug" => scalanizer.snConfig.withDebug(true)
      case "disabled" =>
        enabled = false
//      case o if o.startsWith(ModulePrefix) =>
//        _moduleName = o.stripPrefix(ModulePrefix)
//        val module = scalanizer.snConfig.getModule(_moduleName)
//        module match {
//          case sm: SourceModuleConf =>
//            srcPipeline.isEnabled = true
//          case tm: TargetModuleConf =>
//            targetPipeline.isEnabled = true
//        }
      case option => error("Option not understood: " + option)
    }
    enabled
  }

  /** A description of the plugin's options */
  override val optionsHelp = Some(
    "  -P:" + name + ":save     Save META boilerplate and virtualized code to a file.\n" +
        "  -P:" + name + ":read     Read META boilerplate and virtualized code from a file.\n" +
        "  -P:" + name + ":debug    Print debug information: final AST and etc.\n"
  )
}

object ScalanizerPlugin {
  /** Yields the list of Components to be executed in this plugin */
  def components(global: Global) = {
    val plugin = new ScalanizerPlugin(global)
    plugin.components
  }
}
