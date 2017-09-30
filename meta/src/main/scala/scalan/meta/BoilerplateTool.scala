package scalan.meta

import com.typesafe.scalalogging.StrictLogging

class BoilerplateTool extends StrictLogging {
  def coreMainConfig(name: String, entityFile: String) =
    CodegenConfig(
      name = name, entityFiles = List(entityFile),
      srcPath = "../core/src/main/scala",
      baseContextTrait = "" // used like this: trait ${module.name}Defs extends ${config.baseContextTrait.opt(t => s"$t with ")}${module.name} {
    )

  lazy val viewsConfig           = coreMainConfig("views", "scalan/Views.scala")
  lazy val convertersConfig      = coreMainConfig("converters", "scalan/Converters.scala")
  lazy val exceptionsConfig      = coreMainConfig("exceptions", "scalan/util/Exceptions.scala")
  lazy val specializationsConfig = coreMainConfig("specializations", "scalan/dynamic/Specializations.scala")

  lazy val coreTestsConfig = CodegenConfig(name = "coretests",
    srcPath = "../core/src/test/scala",
    entityFiles = List(
      "scalan/common/Segments.scala",
      "scalan/common/Kinds.scala",
      "scalan/common/MetaTests.scala"
    )
  )

  lazy val collectionsConfig = CodegenConfig(name = "collections",
    srcPath = "../collections/src/main/scala",
    entityFiles = List(
       "scalan/collections/HashSets.scala"
      , "scalan/collections/Seqs.scala"
      , "scalan/collections/MultiMap.scala"
      , "scalan/collections/BitSets.scala"
      , "scalan/collections/Collections.scala"
    )
  )

  lazy val laConfig = CodegenConfig(name = "la",
    srcPath = "../linear-algebra/src/main/scala",
    entityFiles = List(
        "scalan/linalgebra/Vectors.scala"
      , "scalan/linalgebra/Matrices.scala"
    )
  )

  lazy val effectsConfig = CodegenConfig(name = "effects",
    srcPath = "../effects/src/test/scala/",
    entityFiles = List(
      "scalan/monads/IOs.scala",
      "scalan/monads/Readers.scala",
      "scalan/monads/States.scala",
      "scalan/monads/FreeStates.scala",
      "scalan/monads/FreeMs.scala",
      "scalan/monads/Processes.scala",
      "scalan/monads/Frees.scala",
      "scalan/monads/Coproducts.scala",
      "scalan/monads/Interactions.scala",
      "scalan/monads/Auths.scala"
    )
  )

  lazy val graphsConfig = CodegenConfig(name = "graphs",
    srcPath = "../graphs/src/main/scala",
    entityFiles = List(
      "scalan/graphs/Graphs.scala",
      "scalan/graphs/Vertices.scala",
      "scalan/graphs/Edges.scala",
      "scalan/graphs/Fronts.scala"
    )
  )

  lazy val structsConfig = CodegenConfig(name = "structs",
    srcPath = "../core/src/main/scala",
    entityFiles = List(
      "scalan/primitives/StructKeys.scala",
      "scalan/primitives/StructItems.scala"
    ),
    baseContextTrait = "" // not defined means not declare
  )


  val configsMap = List(
    viewsConfig, convertersConfig, exceptionsConfig, coreTestsConfig, specializationsConfig, structsConfig,
    collectionsConfig, graphsConfig, effectsConfig, laConfig
    ).map(c => c.name -> List(c)).toMap ++
    Map(
    "allcore" -> List(viewsConfig, convertersConfig, exceptionsConfig, specializationsConfig, structsConfig, coreTestsConfig)
    )

  def getConfigs(args: Array[String]): Seq[CodegenConfig] =
    args.flatMap { arg => configsMap.getOrElse(arg,
      sys.error(s"Unknown codegen config $arg. Allowed values: ${configsMap.keySet.mkString(", ")}"))
    }.distinct

  def main(args: Array[String]) {
    val configs = getConfigs(args)

    if (configs.isEmpty) {
      logger.warn("BoilerplateTool run without configs")
    } else {
      for (c <- configs) {
        println(s"Processing ${c.srcPath}")
        new EntityManagement(c).generateAll()
        println(s"Ok\n")
      }
    }
  }
}

object BoilerplateToolRun extends BoilerplateTool {
}
