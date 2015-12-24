package scalan.collections

import scalan.compilation.language.{EType, EMethod, CoreMethodMappingDSL}

trait SeqsScalaMethodMapping extends CoreMethodMappingDSL { self: { val scalan: SeqsDslExp } =>
  import scala.language.reflectiveCalls
  import scalan._

  new ScalaMappingDSL with MappingTags {

    val scalan_collections_SSeq = {
      val sseqClass = findDefinition(SSeq) match {
        case Some(TableEntry(sym, rhs)) =>
          rhs.getClass
      }

      new EType(Symbol(sseqClass.getName)) {
        val apply = EMethod('apply)
        val single = EMethod('single)
        val empty = EMethod('empty)
      }
    }

    val scala_collection_Seq = new ScalaLib() {
      val arrayToList = ScalaFunc("(new AnyRef {def apply[T](arr: Array[T]):List[T] = arr.toList})")(false)
      val single = ScalaFunc("scala.collection.Seq")(false)
      val empty = ScalaFunc("Seq.empty")(false)
    }

    val mapping = new ScalaMapping {
      val functionMap = Map( scalan_collections_SSeq.apply -> scala_collection_Seq.arrayToList
        , scalan_collections_SSeq.empty -> scala_collection_Seq.empty
        , scalan_collections_SSeq.single -> scala_collection_Seq.single)
    }
  }
}
