package scalan.plugin

import scala.tools.nsc._
import scalan.meta.{SModuleBuilder, ScalanAstUtils}
import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstUtils._
import scalan.meta.ScalanAstTransformers.MetaAstTransformer

object WrapEnricher {
  val name = "scalanizer-enricher"
}

// TODO ScalanParsers is used only to get wrapperImpl. Move it somewhere?
/** Virtualization of type wrappers. */
class WrapEnricher(override val plugin: ScalanizerPlugin) extends ScalanizerComponent(plugin) {
  import scalanizer._
  import scalanizer.global._

  val phaseName: String = WrapEnricher.name

  override def description: String = "Virtualization of type wrappers."

  val runsAfter = List(WrapFrontend.name)

  val virtPipeline = new ModuleVirtualizationPipeline()(context)

  /** The phase prepares a wrapper for virtualization. */
  def newPhase(prev: Phase) = new StdPhase(prev) {
    override def run(): Unit = {
      import virtPipeline._
      import moduleBuilder._
      implicit val context = virtPipeline.context

      snState.transformWrappers { case (name, wrapperDescr) =>
        /** Transformations of Wrappers by adding of Elem, Cont and other things. */
        val pipeline = scala.Function.chain(Seq(
          preventNameConflict _,
          addWrappedValue _,
          addBaseToAncestors _,
          updateSelf _,
          repSynonym _,
          checkEntityCompanion _,
          constr2apply _,
          cleanUpClassTags _,
          preventNameConflict _,
          genEntityImpicits _,
          genMethodsImplicits _,
          defaultMethod _,
          defaultWrapperImpl _,
          replaceExternalTypeByWrapper _,
          /** Currently, inheritance of type wrappers is not supported.
            * Print warnings and remove ancestors. */
          filterAncestors _
        ))
        val enrichedModule = pipeline(wrapperDescr.module)

        wrapperDescr.copy(module = enrichedModule)
      }
    }

    def apply(unit: CompilationUnit): Unit = ()
  }

  /** Replaces external types by their wrappers. For example:
    * trait Col[A] { def arr: Array[A]; }
    * The external type Array is replaced by its wrapper WArray
    * trait Col[A] { def arr: WArray[A]; }
    * */
  def replaceExternalTypeByWrapper(module: SModuleDef)(implicit ctx: AstContext): SModuleDef = {
    class TypeInWrappersTransformer(name: String) extends External2WrapperTypeTransformer(name) {
      override def methodTransform(method: SMethodDef): SMethodDef = {
        if (method.name == "wrappedValue")
          method
        else super.methodTransform(method)
      }
      override def classArgTransform(classArg: SClassArg) = classArg
      override def entityAncestorTransform(ancestor: STypeApply): STypeApply = {
        if (ancestor.tpe.name == TypeWrapperDefName)
          ancestor
        else
          ancestor.copy(tpe = typeTransformer.traitCallTransform(ancestor.tpe))
      }
    }
    val wrappedModule = snState.externalTypes.foldLeft(module){(acc, externalTypeName) =>
      new TypeInWrappersTransformer(externalTypeName).moduleTransform(acc)
    }
    wrappedModule
  }

}