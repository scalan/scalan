package scalan.collections

import java.io.File
import java.lang.reflect.Method

import scalan._
import scalan.compilation.{GraphVizConfig, GraphVizExport}


class CollectionExamplesSuite extends BaseShouldTests {

  "when mixing trait" should "be constructed in Seq context" in {
    val ctx = new ScalanCtxSeq with ScalanCommunityDslSeq with CollectionExamples {}
  }

  it should "be constructed in Staged context" in {
    val ctx = new ScalanCtxExp with ScalanCommunityDslExp with CollectionExamples {}
  }

  "in seq context" should "execute functions" in {
    val ctx = new ScalanCtxSeq with ScalanCommunityDslSeq with CollectionExamples {}
    val in = Array((1,2f), (3,4f), (5,6f))
    val res = ctx.fromAndTo(in)
    res should be(in)
  }

  "in seq context 1" should "execute functions" in {
    val ctx = new ScalanCtxSeq with ScalanCommunityDslSeq with CollectionExamples {}
    val in = List(1,2,3,4,5,6)
    val res = ctx.listCollectionPairZipWith(in)
    res should be(Array(2,4,6,8,10,12))
  }

  def testMethod(name: String) = {
    val ctx = new ScalanCtxExp with ScalanCommunityDslExp with CollectionExamples with GraphVizExport {
      override def isInvokeEnabled(d: Def[_], m: Method) = true //HACK: invoke all domain methods if possible //TODO this is not how it should be specified
    }
    val f = ctx.getStagedFunc(name)
    ctx.emitDepGraph(f, new File(prefix, s"$name.dot"))(GraphVizConfig.default)
  }

  val whenStaged = "when staged"
  whenStaged should "fromArray" beArgFor { testMethod(_) }
  whenStaged should "fromArrayOfPairs" beArgFor { testMethod(_) }
  whenStaged should "fromAndTo" beArgFor { testMethod(_) }
  whenStaged should "mapped" beArgFor { testMethod(_) }
  whenStaged should "zippedMap" beArgFor { testMethod(_) }
  whenStaged should "mapped2" beArgFor { testMethod(_) }
  whenStaged should "splitMap" beArgFor { testMethod(_) }
  whenStaged should "splitMap2" beArgFor { testMethod(_) }
  whenStaged should "mapInc3Times" beArgFor { testMethod(_) }
  whenStaged should "splitMap3" beArgFor { testMethod(_) }
  whenStaged should "splitMapMap" beArgFor { testMethod(_) }
  whenStaged should "mapScalar" beArgFor { testMethod(_) }
  whenStaged should "expCollectionOverArraysInIf" beArgFor { testMethod(_) }
  whenStaged should "expCollectionOverListsInIf" beArgFor { testMethod(_) }
  whenStaged should "expCollectionOverArraysInIfSpec" beArgFor { testMethod(_) }
  whenStaged should "expPairCollectionSOAsInIf" beArgFor { testMethod(_) }
  whenStaged should "expPairCollectionSOAsInIfSpec" beArgFor { testMethod(_) }
  whenStaged should "expPairCollectionSOAsInIfDiffTypes" beArgFor { testMethod(_) }
  whenStaged should "expPairCollectionSOAsInIfDiffTypesSpec" beArgFor { testMethod(_) }
  whenStaged should "sumFold" beArgFor { testMethod(_) }
  whenStaged should "sumFoldSpec" beArgFor { testMethod(_) }
  whenStaged should "pairInIf" beArgFor { testMethod(_) }
  whenStaged should "pairInIfSpec" beArgFor { testMethod(_) }
  whenStaged should "nestedPairInIf" beArgFor { testMethod(_) }
  whenStaged should "nestedPairInIfSpec" beArgFor { testMethod(_) }
  whenStaged should "nestedListPairInIf" beArgFor { testMethod(_) }
  whenStaged should "nestedListPairInIfSpec" beArgFor { testMethod(_) }
  whenStaged should "listCollectionPairZipWith" beArgFor { testMethod(_) }
  whenStaged should "sort" beArgFor { testMethod(_) }
}
