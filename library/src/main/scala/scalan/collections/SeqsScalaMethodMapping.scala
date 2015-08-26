package scalan.collections

import scalan.{ScalanCommunityDslExp, CommunityMethodMappingDSL}

trait SeqsScalaMethodMapping extends CommunityMethodMappingDSL { self: { val scalan: ScalanCommunityDslExp } =>
  import scala.language.reflectiveCalls
  import scala.reflect.runtime.universe._
  import scalan._

  new ScalaMappingDSL with MappingTags {

    val scalan_collections_SSeq = {
      val sseqClass = findDefinition(SSeq) match {
        case Some(TableEntry(sym, rhs)) =>
          rhs.getClass
      }

      new ClassType(Symbol(sseqClass.getName)) {
        val apply = Method('apply, typeOf[Seq[_]], MethodArg(typeOf[Array[_]]))
        val single = Method('single, typeOf[Seq[_]])
        val empty = Method('empty, typeOf[Seq[_]])
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
