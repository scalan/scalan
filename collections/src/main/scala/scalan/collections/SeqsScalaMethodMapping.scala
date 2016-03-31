package scalan.collections

import scalan.compilation.language.MethodMappingDSL
import scalan.compilation.language.ScalaMapping._

trait SeqsScalaMethodMapping extends MethodMappingDSL {
  MapModuleScala[Seqs].types(
    MapTypeScala[Seqs#SSeqCompanion]().to("scala.collection.Seq").methods(
      MapMethodScala("apply", 'arr).to("toSeq").onArg('arr),
      MapMethodScala("single", 'elem).to("apply")
      // map empty?
    )
  )
}
