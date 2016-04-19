package scalan.collections

import scalan.compilation.language.MethodMappingDSL
import scalan.compilation.language.Scala._

trait SeqsScalaMethodMapping extends MethodMappingDSL {
  mapModule[Seqs].types(
    mapType[Seqs#SSeqCompanion]().to("scala.collection.Seq").methods(
      mapMethod("apply", 'arr).to("toSeq").onArg('arr),
      mapMethod("single", 'elem).to("apply")
      // map empty?
    )
  )
}
