package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scalan.meta.EntityManagement
import scalan.meta.ScalanAst.AstContext
import scalan.meta.scalanizer.{ScalanizerConfig, Scalanizer, ScalanizerState}

abstract class ScalanPlugin(val global: Global) extends Plugin { plugin =>
}

class ScalanizerPlugin(g: Global) extends ScalanPlugin(g) { plugin =>
  val scalanizer: Scalanizer[plugin.global.type] = new Scalanizer[plugin.global.type] {
    def getGlobal: plugin.global.type = plugin.global
    val snConfig: ScalanizerConfig = new ScalanizerPluginConfig
    val context: AstContext = new AstContext(snConfig.unitConfigs, this)
    val entityManagment: EntityManagement[plugin.global.type] = new EntityManagement(this)
    val snState: ScalanizerState[plugin.global.type] = new ScalanizerPluginState(this)
  }

  /** Visible name of the plugin */
  val name: String = "scalanizer"

  /** The compiler components that will be applied when running this plugin */
  val components: List[PluginComponent] = List(
//      new Assembler(this)
     new WrapFrontend(this)
    , new WrapEnricher(this)
    , new WrapBackend(this)
    , new VirtFrontend(this)
//    , new VirtBackend(this)
//    , new CheckExtensions(this)
    , new FinalComponent(this)
    //  , new Debug(this)
  )

  /** The description is printed with the option: -Xplugin-list */
  val description: String = "Optimization through staging"

  var sourceModuleName = ""

  /** Plugin-specific options without -P:scalanizer:  */
  override def init(options: List[String], error: String => Unit): Boolean = {
    var enabled = true
    options foreach {
      case "debug" => scalanizer.snConfig.withDebug(true)
      case "disabled" =>
        enabled = false
      case o if o.startsWith("module=") =>
        sourceModuleName = o.stripPrefix("module=")
      case option => error("Option not understood: " + option)
    }
    enabled
  }

// 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90

  /** A description of the plugin's options */
  override val optionsHelp = Some(
    "  -P:"+ name +":save     Save META boilerplate and virtualized code to a file.\n"+
    "  -P:"+ name +":read     Read META boilerplate and virtualized code from a file.\n"+
    "  -P:"+ name +":debug    Print debug information: final AST and etc.\n"
  )
}

object ScalanizerPlugin {
  /** Yields the list of Components to be executed in this plugin */
  def components(global: Global) = {
    val plugin = new ScalanizerPlugin(global)
    plugin.components
  }
}
