package scalan.collections

import scalan._

/** Contains tests for Elems which use Collections */
class MoreElemTests extends AbstractElemTests {
  it("Structs as type parameters are included in names") {
    val ctx = new Ctx with CollectionsDslExp
    import ctx._
    val structElem = structElement(Seq("a" -> IntElement, "b" -> DoubleElement))

    val collectionOfStructsElem = collectionElement(structElem)
    assert(collectionOfStructsElem.name == s"Collection[${structElem.name}]")

    val collectionOfcollectionsOfStructsElem = collectionElement(collectionOfStructsElem)
    assert(collectionOfcollectionsOfStructsElem.name == s"Collection[Collection[${structElem.name}]]")
  }

}
