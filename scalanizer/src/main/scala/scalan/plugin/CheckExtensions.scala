package scalan.plugin

import scala.tools.nsc._

object CheckExtensions {
  val name = "scalanizer-check"
}

/** The component searches user's extensions traits */
class CheckExtensions(override val plugin: ScalanizerPlugin) extends ScalanizerComponent(plugin) {
  import scalanizer._
  import scalanizer.global._

  val phaseName: String = CheckExtensions.name
  override def description: String = "Find user's extensions"

  val runsAfter = List(WrapBackend.name)

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      val hasStagedObj = unit.body match {
        case q"package $ref0 { package $ref1 {..$topstats }}" =>
          topstats.exists {
            case q"object StagedEvaluation {..$stats}" => true
            case _ => false
          }
        case _ => false
      }
      if (!hasStagedObj)
        newTraverser().traverse(unit.body)
    }
  }

  def newTraverser(): Traverser = new ForeachTreeTraverser(checkExtTrait)

  def getTraitName(tpname: TypeName): String = tpname match {
    case TypeName(name) => name
    case _ => ""
  }

  def getCakeSliceName(traitName: String): String = {
    if (traitName.endsWith("Dsl")) traitName.stripSuffix("Dsl")
    else if (traitName.endsWith("DslStd")) traitName.stripSuffix("DslStd")
    else if (traitName.endsWith("DslExp")) traitName.stripSuffix("DslExp")
    else ""
  }

  def checkExtTrait(tree: Tree): Unit = tree match {
    case q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
      val traitName = getTraitName(tpname)
      val cakeName = getCakeSliceName(traitName)
      snState.subcakesOfModule get cakeName match {
        case Some(s) =>
          //print("Extension is found: " + traitName + " for the cake slice: " + cakeName)
          snState.subcakesOfModule(cakeName) -= traitName
          ()
        case None => ()
      }
    case _ => ()
  }
}

