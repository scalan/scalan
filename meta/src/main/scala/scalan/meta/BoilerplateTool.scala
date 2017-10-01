package scalan.meta

import com.typesafe.scalalogging.StrictLogging

class BoilerplateTool extends StrictLogging {
  def coreMainConfig(name: String, entityFile: String) =
    CodegenConfig(
      name = name, entityFile = entityFile,
      srcPath = "../core/src/main/scala",
      baseContextTrait = "" // used like this: trait ${module.name}Defs extends ${config.baseContextTrait.opt(t => s"$t with ")}${module.name} {
    )

  def coreTestConfig(name: String, entityFile: String) =
    CodegenConfig(
      name = name, entityFile = entityFile,
      srcPath = "../core/src/test/scala",
      baseContextTrait = "")

  lazy val viewsConfig           = coreMainConfig("views", "scalan/Views.scala")
  lazy val convertersConfig      = coreMainConfig("converters", "scalan/Converters.scala")
  lazy val exceptionsConfig      = coreMainConfig("exceptions", "scalan/util/Exceptions.scala")
  lazy val specializationsConfig = coreMainConfig("specializations", "scalan/dynamic/Specializations.scala")

  lazy val structKeysConfig = coreMainConfig("structKeys", "scalan/primitives/StructKeys.scala")
  lazy val structItemsConfig = coreMainConfig("structItems", "scalan/primitives/StructItems.scala")

  lazy val segmentsConfig        = coreTestConfig("segments", "scalan/common/Segments.scala")
  lazy val kindsConfig           = coreTestConfig("kinds", "scalan/common/Kinds.scala")
  lazy val metatestsConfig       = coreTestConfig("metatests", "scalan/common/MetaTests.scala")

  val allConfigs = List(
    viewsConfig, convertersConfig, exceptionsConfig, specializationsConfig,
    structKeysConfig, structItemsConfig,
    segmentsConfig, kindsConfig, metatestsConfig
  )

  val runGroups = allConfigs.map(c => c.name -> List(c)).toMap ++ // each individual config can be executed
    Map( // config groups can be declared and executed by name
      "all" -> allConfigs
    )

  def listGroups = runGroups.keySet.mkString(", ")

  def getRequestedConfigs(requestedGroups: Array[String]): Seq[CodegenConfig] =
    requestedGroups.flatMap { groupName => runGroups.getOrElse(groupName,
      sys.error(s"Unknown codegen config $groupName. Allowed values: $listGroups"))
    }.distinct

  def main(args: Array[String]) {
    val configs = getRequestedConfigs(args)

    if (configs.isEmpty) {
      logger.warn(s"BoilerplateTool run without specified config groups. Available: $listGroups")
    } else {
      val parsers = new Parsers(allConfigs)
      val em = new EntityManagement(parsers)
      for (c <- configs) {
        println(s"Processing ${c.srcPath}")
        em.generate(c.name)
        println(s"Ok\n")
      }
    }
  }
}

object BoilerplateToolRun extends BoilerplateTool {
}
