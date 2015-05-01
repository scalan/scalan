package scalan.effects

import scalan.examples.{InteractionsDslSeq, InteractionsDslExp}
import scalan.{ScalanCommunityDslExp, ScalanCommunityDslSeq, ScalanCommunitySeq}
import scalan.collections.{MultiMapsDslSeq}
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.it.BaseItTests

class EffectsItTests extends BaseItTests {
  trait EffectsRunner extends InteractExample {
    lazy val runInteract = fun {in: Rep[Int] => runApp(in) }
  }
  class EffectsExp extends EffectsRunner
                      with CommunityLmsCompilerScala with CoreBridge
                      with ScalanCommunityDslExp with InteractionsDslExp {
    val lms = new CommunityLmsBackend
  }
  class EffectsSeq extends EffectsRunner
                      with ScalanCommunitySeq with ScalanCommunityDslSeq
                      with MultiMapsDslSeq with InteractionsDslSeq

  val progSeq = new EffectsSeq
  val progStaged = new EffectsExp

  test("runInteract")  {
    //pending
    val in = 10
    compareOutputWithSequential(progStaged)(progSeq.runInteract, progStaged.runInteract, "runInteract", in)
  }

}
