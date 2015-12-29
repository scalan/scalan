package scalan.collections

import scalan.compilation.language.ScalaMapping._
import scalan.compilation.language.{ScalaMapping, MethodMappingDSL}

trait SeqsScalaMethodMapping extends MethodMappingDSL { self: { val scalan: SeqsDslExp } =>
  MapModuleScala("scalan.collections.Seqs").types(
    // TODO MapCompanionScala?
    MapTypeScala("SSeq").methods(
      // MapMethodScala("apply", 'i) -> ScalaFunc("(new AnyRef {def apply[T](arr: Array[T]):List[T] = arr.toList})")(false)
      MapMethodScala("single").to("scala.collection.Seq"),
      MapMethodScala("empty").to("scala.collection.Seq.empty")
    )
  )

//  new ScalaMappingDSL with MappingTags {
//
//    val scalan_collections_SSeq = {
//      val sseqClass = findDefinition(SSeq) match {
//        case Some(TableEntry(sym, rhs)) =>
//          rhs.getClass
//      }
//
//      new EType(Symbol(sseqClass.getName)) {
//        val apply = EMethod('apply)
//        val single = EMethod('single)
//        val empty = EMethod('empty)
//      }
//    }
//
//    val scala_collection_Seq = new ScalaLib() {
//      val arrayToList = ScalaFunc("(new AnyRef {def apply[T](arr: Array[T]):List[T] = arr.toList})")(false)
//      val single = ScalaFunc("scala.collection.Seq")(false)
//      val empty = ScalaFunc("Seq.empty")(false)
//    }
//
//    val mapping = new ScalaMapping {
//      val functionMap = Map( scalan_collections_SSeq.apply -> scala_collection_Seq.arrayToList
//        , scalan_collections_SSeq.empty -> scala_collection_Seq.empty
//        , scalan_collections_SSeq.single -> scala_collection_Seq.single)
//    }
//  }
}
