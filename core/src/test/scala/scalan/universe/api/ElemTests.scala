package scalan.universe.api

import scala.language.reflectiveCalls
import scalan._
import scalan.common.{SegmentsDsl, KindsDsl}
import scalan.universe.api.UniverseUtils._

class ElemTests extends BaseCtxTests { suite =>
  class Ctx extends TestContext with SegmentsDsl with KindsDsl with TypesApi {
    val eInt      = element[Int]
    val eDouble   = element[Double]
    val eSegment  = element[Segment]
    val eInterval = element[Interval]
    val eSlice    = element[Slice]
    val eCentered = element[Centered]
//    val eCollectionInt = element[Collection[Int]]
    val tyCollectionInt = Entity("Collection")(eInt)
    val tyCollectionDouble = Entity("Collection")(eDouble)

    def genElems[A](n: Int, e: Elem[A], f: Elem[A] => Iterator[Elem[_]]): Set[Elem[_]] = {
      genTuples(List.fill(n)(e))(f).map(es => Elem.pairify(es.toIterator)).toSet
    }
  }

  test("EntityElem.parent") {
    val ctx = new Ctx
    import ctx._
    def testParent[A, B <: A](e: Elem[B], eParent: Option[Elem[A]]) = {
      e match {
        case ee: EntityElem[_] =>
          val p = ee.parent
          assert(p == eParent, s"Expected parent $eParent but was $p")
        case _ => sys.error(s"EntityElem expected but found $e")
      }
    }
    testParent(eInterval, Some(eSegment))
    testParent(eSlice, Some(eSegment))
    testParent(eCentered, Some(eSegment))
    testParent(eSegment, None)
//    testParent(element[SSeq[Int]], None)
//    testParent(element[SSeqImpl[Int]], Some(element[SSeq[Int]]))
//    testParent(element[PairCollection[Int, Double]], Some(element[Collection[(Int, Double)]]))
//    testParent(element[PairCollectionAOS[Int, Double]], Some(element[PairCollection[Int, Double]]))
  }

  test("getDescendants_Segment") {
    val ctx = new Ctx
    import ctx._
    val entitySegment = Entity(eSegment)
    val ds = entitySegment.subEntitiesIter.map(_.name).toSet
    assert(ds == Set("Interval", "Slice", "Centered"))
  }

  test("extendsTypes") {
    val ctx = new Ctx
    import ctx._
    def test[A <: Def[_]: Elem ](expected: Set[String]) = {
      val e = element[A]
      val ent = Entity(e)
      assertResult(expected)(ent.extendsTypes.map(_.name).toSet)
    }
    test[Segment](Set())
    test[Interval](Set("Segment"))

//    test[Collection[Int]](Set())
//    test[CollectionOverArray[Int]](Set("Collection[Item]"))
//    test[PairCollection[Int,Double]](Set("Collection[(A, B)]"))
//    test[PairCollectionSOA[Int,Double]](Set("PairCollection[A, B]"))
//    test[PairCollectionAOS[Int,Double]](Set("PairCollection[A, B]"))
  }

//  test("getDescendants_PairCollection") {
//    val ctx = new Ctx
//    import ctx._
//    val e = element[PairCollection[Int, Double]]
//    val ent = Entity(e)
//    val ds = ent.subEntitiesIter.map(_.name).toSet
//    assert(ds == Set("PairCollectionAOS", "PairCollectionSOA"))
//  }

//  test("unifyTypes_Entity") {
//    val ctx = new Ctx
//    import ctx._
//    def testUni[A: Elem](typeName: String, expected: Option[TypeArgSubst]) = {
//      val t1 = Entity(typeName).asType
//      val t2 = element[A]
//      val res = unifyTypes(t1, t2)
//      assertResult(expected)(res)
//    }
//    val coll = Entity("Collection")
//    val pairColl = Entity("PairCollection")
//    val argItem = coll.typeArg("Item")
//    val argA = pairColl.typeArg("A")
//    val argB = pairColl.typeArg("B")
//
//    testUni[Collection[Int]]("Collection", Some(Map(argItem.argName -> eInt)))
//    testUni[Collection[Collection[Int]]]("Collection", Some(Map(argItem.argName -> element[Collection[Int]])))
//    testUni[Collection[Int]]("Segment", None)
//    testUni[PairCollection[Int, Double]]("PairCollection", Some(Map(argA.argName -> eInt, argB.argName -> eDouble)))
//    testUni("PairCollection", Some(Map(argA.argName -> eInt)))(pairCollectionElement(eInt, argB))
//    testUni[PairCollection[Int, Collection[Double]]]("PairCollection", Some(Map(argA.argName -> eInt, argB.argName -> element[Collection[Double]])))
//  }

  test("unifyTypes") {
    val ctx: Ctx = new Ctx()
    import ctx._
    def testUni[A: Elem](t1: TypeDesc, expected: Option[Map[ArgElem, TypeDesc]]) = {
      val expectedSubst = expected.map(m => m.map { case (a, d) => (a.argName, d) })
      val t2 = element[A]
      val res = unifyTypes(t1, t2)
      assertResult(expectedSubst)(res)
    }
    val entColl = Entity("Collection")
    val entPairColl = Entity("PairCollection")
    val argItem = entColl.typeArg("Item")
    val argA = entPairColl.typeArg("A")
    val argB = entPairColl.typeArg("B")
    testUni[Int](eDouble, None)
    testUni[Int](eInt, Some(Map()))
    testUni[(Int, Double)](pairElement(argA, eDouble), Some(Map(argA -> eInt)))
    testUni[(Int, Double)](pairElement(argA, eInt), None)
    testUni[(Int, Double)](pairElement(argA, argB), Some(Map(argA -> eInt, argB -> eDouble)))
    testUni[(Int, (Double, Int))](pairElement(argA, pairElement(argB, eInt)), Some(Map(argA -> eInt, argB -> eDouble)))
//    testUni[Collection[(Int, Double)]](entColl(argItem.argName -> pairElement(argA, argB)), Some(Map(argA -> eInt, argB -> eDouble)))
    
    testUni[(Int | Double)](sumElement(argA, eDouble), Some(Map(argA -> eInt)))
    testUni[(Int | Double)](sumElement(argA, eInt), None)
    testUni[(Int | Double)](sumElement(argA, argB), Some(Map(argA -> eInt, argB -> eDouble)))
    testUni[(Int | (Double | Int))](sumElement(argA, sumElement(argB, eInt)), Some(Map(argA -> eInt, argB -> eDouble)))
//    testUni[Collection[(Int | Double)]](entColl(argItem.argName -> sumElement(argA, argB)), Some(Map(argA -> eInt, argB -> eDouble)))

    testUni[(Int => Double)](funcElement(argA, eDouble), Some(Map(argA -> eInt)))
    testUni[(Int => Double)](funcElement(argA, eInt), None)
    testUni[(Int => Double)](funcElement(argA, argB), Some(Map(argA -> eInt, argB -> eDouble)))
    testUni[(Int => (Double => Int))](funcElement(argA, funcElement(argB, eInt)), Some(Map(argA -> eInt, argB -> eDouble)))
//    testUni[Collection[(Int => Double)]](entColl(argItem.argName -> funcElement(argA, argB)), Some(Map(argA -> eInt, argB -> eDouble)))
  }

  test("directSpecs") {
    val ctx = new Ctx
    import ctx._
    def testSubtypes[A: Elem](expectedElems: Set[Elem[_]]) = {
      val eParent = element[A]
      val specs = eParent.directSpecsExclusive(QueryParams(false)).toSet
      assertResult(expectedElems)(specs)
    }
    val segmentImpls: Set[Elem[_]] = Set(eInterval, eSlice, eCentered)
    def fSeg(e: Elem[_]) = (Set(e) ++ segmentImpls).iterator
    val segmentImpls2 = genElems(2, eSegment, fSeg _).diff(Set(pairElement(eSegment, eSegment)))

    testSubtypes[Int](Set())
//    testSubtypes[Array[Int]](Set())
    testSubtypes[Segment](segmentImpls)
    testSubtypes[(Segment, Segment)](segmentImpls2)

//    def collImpls[A: Elem]: Set[Elem[_]] =
//      Set(element[CollectionOverArray[A]], element[CollectionOverList[A]], element[CollectionOverSeq[A]])
//    def fColl[A: Elem] = (Set(element[Collection[A]]) ++ collImpls[A]).iterator
//    def collImpls2[A: Elem] = {
//      val eColl = element[Collection[A]]
//      genElems(2, element[A], (e: Elem[A]) => fColl(e)).diff(Set(pairElement(eColl, eColl)))
//    }
//
//    testSubtypes[Collection[Int]](collImpls[Int])
//    testSubtypes[(Collection[Int], Collection[Int])](collImpls2[Int])
//
//    testSubtypes[Collection[(Int, Double)]](Set(element[PairCollection[Int, Double]]) ++ collImpls[(Int, Double)])
//    testSubtypes[(Collection[Int], Double)](collImpls[Int].map(e => pairElement(e, element[Double])))
//    testSubtypes[(Collection[Int], Array[Double])](collImpls[Int].map(e => pairElement(e, element[Array[Double]])))
  }

  test("isConcrete") {
    val ctx = new Ctx
    import ctx._
    def testIsConcrete[A: Elem](expected: Boolean) = {
      val e = element[A]
      assertResult(expected)(e.isConcrete)
    }
    testIsConcrete[Int](true)
//    testIsConcrete[Array[Int]](true)
//    testIsConcrete[Array[(Int, Double)]](true)
//    testIsConcrete[Array[(Collection[Int], Double)]](false)
//    testIsConcrete[Array[(CollectionOverArray[Int], Double)]](true)

    testIsConcrete[Segment](false)
//    testIsConcrete[Array[Segment]](false)
//    testIsConcrete[Array[(Segment, Double)]](false)
//    testIsConcrete[Array[(Collection[Segment], Double)]](false)
//    testIsConcrete[Array[(CollectionOverArray[Segment], Double)]](false)

    testIsConcrete[Interval](true)
//    testIsConcrete[Array[Interval]](true)
//    testIsConcrete[Array[(Interval, Double)]](true)
//    testIsConcrete[Array[(Collection[Interval], Double)]](false)
//    testIsConcrete[Array[(CollectionOverArray[Interval], Double)]](true)

//    testIsConcrete[Collection[Int]](false)
//    testIsConcrete[Collection[(Int, Double)]](false)
//    testIsConcrete[(Collection[Int], Double)](false)
//    testIsConcrete[(Collection[Int] | Double)](false)
//    testIsConcrete[CollectionOverArray[Int]](true)
//    testIsConcrete[CollectionOverArray[Array[Int]]](true)
//    testIsConcrete[CollectionOverArray[Collection[Int]]](false)
//    testIsConcrete[Collection[CollectionOverArray[Int]]](false)
//
//    testIsConcrete[PairCollection[Int, Double]](false)
//    testIsConcrete[PairCollectionAOS[Int, Double]](true)
  }

  test("allConcreteSpecs") {
    val ctx = new Ctx
    import ctx._
    def testAllSpecs[A: Elem](expected: Set[String]) = {
      val e = element[A]
      assertResult(expected)(e.allConcreteSpecs(QueryParams(false)).map(_.name).toSet)
    }
    testAllSpecs[Int](Set("Int"))
    testAllSpecs[(Int, Double)](Set("(Int, Double)"))
//    testAllSpecs[Array[Int]](Set("Array[Int]"))
//    testAllSpecs[(Array[Int], Double)](Set("(Array[Int], Double)"))
//    testAllSpecs[(Seq[Int], Double)](Set("(Seq[Int], Double)"))
//    testAllSpecs[(SSeq[Int], Double)](Set("(SSeqImpl[Int], Double)"))

    testAllSpecs[Segment](Set("Interval", "Slice", "Centered"))
    testAllSpecs[(Segment, Double)](Set("(Interval, Double)", "(Slice, Double)", "(Centered, Double)"))
//    testAllSpecs[Array[Segment]](Set("Array[Interval]", "Array[Slice]", "Array[Centered]"))

//    testAllSpecs[Collection[Int]](Set("CollectionOverArray[Int]", "CollectionOverList[Int]", "CollectionOverSeq[Int]"))
//    testAllSpecs[CollectionOverArray[Int]](Set("CollectionOverArray[Int]"))
//    testAllSpecs[CollectionOverArray[Segment]](Set())
//    testAllSpecs[PairCollection[Int, Double]](Set("PairCollectionSOA[Int, Double]", "PairCollectionAOS[Int, Double]"))
//
//    testAllSpecs[Collection[Segment]](Set(
//      "CollectionOverArray[Interval]", "CollectionOverList[Interval]", "CollectionOverSeq[Interval]",
//      "CollectionOverArray[Slice]", "CollectionOverList[Slice]", "CollectionOverSeq[Slice]",
//      "CollectionOverArray[Centered]", "CollectionOverList[Centered]", "CollectionOverSeq[Centered]"))
  }

  test("Elem.contains") {
    val ctx = new Ctx
    import ctx._
    def testContains[A:Elem,B: Elem](expected: Boolean = true) = {
      val tA = element[A]
      val tB = element[B]
      assertResult(expected)(tA.contains(tB))
    }
//    def testContainsType[A:Elem](tB: Type, expected: Boolean = true) = {
//      val tA = Type(element[A])
//      assertResult(expected)(tA.contains(tB))
//    }
    testContains[Int,Int]()
//    testContains[Array[Int],Int]()
//    testContains[Array[Array[Int]],Int]()
//    testContains[Array[Array[Int]],Array[Int]]()
    testContains[(Int,Double),Int]()
//    testContains[(Array[Int],Double),Int]()
//    testContains[Collection[Int],Int]()
//    testContains[(Collection[Int],Double),Int]()
//    testContains[(Collection[Int],Double),Collection[Int]]()
//    testContains[(Collection[Int],Double),(Collection[Int],Double)]()
//
//    testContains[Seq[Int],Int]()
//    testContains[Seq[Seq[Int]],Int]()
//    testContains[Seq[Seq[Int]],Seq[Int]]()
//    testContains[SSeq[Int],Int]()
//    testContains[SSeq[SSeq[Int]],Int]()
//    testContains[SSeq[SSeq[Int]],SSeq[Int]]()
//    testContains[Seq[Int],Double](false)
//    testContains[Seq[Seq[Int]],Double](false)
//    testContains[Seq[Seq[Int]],Seq[Double]](false)
//    testContains[SSeq[Int],Double](false)
//    testContains[SSeq[SSeq[Int]],Double](false)
//    testContains[SSeq[SSeq[Int]],SSeq[Double]](false)
//
//    testContains[SSeq[Throwable],Throwable]()
//    testContains[SSeq[SSeq[Throwable]],SSeq[Throwable]]()

    testContains[Int,Double](false)
//    testContains[Array[Int],Double](false)
//    testContains[Array[Array[Int]],Double](false)
//    testContains[Array[Array[Int]],Array[Double]](false)
    testContains[(Int,Double),(Double,Double)](false)
//    testContains[(Array[Int],Double),Array[Double]](false)
//    testContains[Collection[Int],Double](false)
//    testContains[(Collection[Int],Double),Collection[Double]](false)
//    testContains[(Collection[Int],Double),(Collection[Double],Double)](false)

//    val eArr = arrayElement(ArgElem("A"))
//    testContains()(element[Array[Int]], eArr)
//    testContains()(element[Array[Array[Int]]], eArr)
//    testContainsType[List[Array[Int]]](tArr)
//
//    val tSeq = BaseType("Seq", List(TypeArg("A")))
//    testContainsType[Seq[Int]](tSeq)
//    testContainsType[Seq[(Int,Double)]](tSeq)
//
//    val tCol = Entity("Collection").asType.asElem
//    testContains()(element[Collection[(Int,Double)]], tCol)
  }

//  test("Type.isLike") {
//    val ctx = new Ctx
//    import ctx._
//    def test[A:Elem](tB: TypeDesc, expected: Boolean = true) = {
//      val tA = element[A]
//      assertResult(expected)(tA.isLike(tB))
//    }
//    test[Array[Int]](arrayElement(ArgElem("A")))
//    test[List[Int]](ListType(TypeArg("A")))
//    val tSeq = BaseType("Seq", List(TypeArg("A")))
//    test[Seq[Int]](tSeq)
//    test[Seq[(Int,Double)]](tSeq)
//    val tCol = Entity("Collection").asType
//    test[Collection[(Int,Double)]](tCol)
//  }
}
