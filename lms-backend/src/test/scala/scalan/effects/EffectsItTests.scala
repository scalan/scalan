package scalan.effects

import scalan.examples.{AuthenticationsDslExp, AuthenticationsDslSeq, InteractionsDslSeq, InteractionsDslExp}
import scalan.io.ConsoleCompiler
import scalan.monads.{MonadsDslExp, MonadsDslSeq}
import scalan.{ScalanCommunityDslExp, ScalanCommunityDslSeq, ScalanCommunitySeq}
import scalan.collections.{MultiMapsDslSeq}
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.it.BaseItTests

class EffectsItTests extends BaseItTests {

  trait InteractWrapper extends InteractExample {
    lazy val runInteract = fun {in: Rep[Int] =>
      val app = runApp
      app(in)._2
    }
  }

  trait EffectsExp extends CommunityLmsCompilerScala with CoreBridge
                      with ScalanCommunityDslExp
                      with ConsoleCompiler {
    val lms = new CommunityLmsBackend

  }
  trait EffectsSeq extends ScalanCommunitySeq with ScalanCommunityDslSeq
                      with MultiMapsDslSeq


  test("runInteract")  {
    val progSeq = new EffectsSeq with InteractWrapper with InteractionsDslSeq
    val progStaged = new EffectsExp with InteractWrapper with InteractionsDslExp
    //pending
    val in = 10
    compareOutputWithSequential(progStaged)(progSeq.runInteract, progStaged.runInteract, "runInteract", in)
  }

  trait CrossDomainWrapper extends CrossDomainExample {
    lazy val runCrossDomain = fun {in: Rep[Int] =>
      val app = runApp
      app(in)._2
    }
  }

  test("runCrossDomain")  {
    val progSeq = new EffectsSeq with CrossDomainWrapper
                  with InteractionsDslSeq with AuthenticationsDslSeq
    val progStaged = new EffectsExp with CrossDomainWrapper
                     with InteractionsDslExp with AuthenticationsDslExp
    //pending
    val in = 10
    val actual = getStagedOutputConfig(progStaged)(progStaged.runCrossDomain, "runCrossDomain", in, progStaged.defaultCompilerConfig)
  }

}
