package scalan.collection

import java.io.File
import java.lang.reflect.Method

import scalan._
import scalan.compilation.{GraphVizConfig, GraphVizExport}


class CollExamplesSuite extends BaseShouldTests {

  "when mixing trait" should "be constructed in Seq context" in {
      val ctx = new ScalanCommunityDslSeq with CollExamples {}
  }
  
  it should "be constructed in Staged context" in {
    val ctx = new ScalanCommunityDslExp with CollExamples {}
  }

  "in seq context" should "execute functions" in {
    val ctx = new ScalanCommunityDslSeq with CollExamples {}
    val in = Array((1,2f), (3,4f), (5,6f))
    val res = ctx.fromAndTo(in)
    res should be(in)
  }

  def testMethod(name: String) = {
    val ctx = new ScalanCommunityDslExp with CollExamples with GraphVizExport {
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
  whenStaged should "expBaseCollectionsInIf" beArgFor { testMethod(_) }
  whenStaged should "expBaseCollectionsInIfSpec" beArgFor { testMethod(_) }
  whenStaged should "expPairCollectionsInIf" beArgFor { testMethod(_) }
  whenStaged should "expPairCollectionsInIfSpec" beArgFor { testMethod(_) }
  whenStaged should "expPairCollectionsInIfDiffTypes" beArgFor { testMethod(_) }
  whenStaged should "expPairCollectionsInIfDiffTypesSpec" beArgFor { testMethod(_) }
  whenStaged should "sumFold" beArgFor { testMethod(_) }
  whenStaged should "sumFoldSpec" beArgFor { testMethod(_) }
  whenStaged should "pairInIf" beArgFor { testMethod(_) }
  whenStaged should "pairInIfSpec" beArgFor { testMethod(_) }
  whenStaged should "nestedPairInIf" beArgFor { testMethod(_) }
  whenStaged should "nestedPairInIfSpec" beArgFor { testMethod(_) }

}
