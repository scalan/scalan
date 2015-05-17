package scalan.effects

import scalan.examples.{AuthenticationsDslExp, AuthenticationsDslSeq, InteractionsDslSeq, InteractionsDslExp}
import scalan.monads.MonadsDslExp
import scalan.primitives.EffectfulCompiler
import scalan.{ScalanCommunityDsl, ScalanCommunityDslExp, ScalanCommunityDslSeq, ScalanCommunitySeq}
import scalan.collections.{MultiMapsDslSeq}
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.it.BaseItTests

class EffectsItTests extends BaseItTests
{
  trait EffectsExp extends CommunityLmsCompilerScala with CoreBridge
                      with ScalanCommunityDslExp
                      with EffectfulCompiler {
    val lms = new CommunityLmsBackend

  }
  trait EffectsSeq extends ScalanCommunitySeq with ScalanCommunityDslSeq
                      with MultiMapsDslSeq

  test("runInteract")  {
    val progSeq = new EffectsSeq with InteractExample with InteractionsDslSeq
    val progStaged = new EffectsExp with InteractExample with InteractionsDslExp
    val in = 10
    val actual = getStagedOutputConfig(progStaged)(progStaged.runAppW, "runInteract", in, progStaged.defaultCompilerConfig)
  }

  test("runInteract2")  {
    val progSeq = new EffectsSeq with InteractExample with InteractionsDslSeq
    val progStaged = new EffectsExp with InteractExample with InteractionsDslExp
    val in = 10
    val actual = getStagedOutputConfig(progStaged)(progStaged.runApp2W, "runInteract2", in, progStaged.defaultCompilerConfig)
  }

  test("runCrossDomain")  {
    val progSeq = new EffectsSeq with CrossDomainExample
      with InteractionsDslSeq with AuthenticationsDslSeq
    val progStaged = new EffectsExp with CrossDomainExample
      with InteractionsDslExp with AuthenticationsDslExp
    val in = 10
    val actual = getStagedOutputConfig(progStaged)(progStaged.runAppW, "runCrossDomain", in, progStaged.defaultCompilerConfig)
  }

  trait IfBranchesExamples extends ScalanCommunityDsl {
    lazy val t1 = fun { (in: Rep[String]) =>
      IF (in.contains("abc")) THEN { console_printlnE(in) } ELSE { console_printlnE(in) }
    }
    lazy val t2 = fun { (in: Rep[String]) =>
      val input = console_readlineE()
      val user = IF (input !== (null: String)) { input } ELSE { "admin" }
      IF (in.contains(user)) THEN {
        console_printlnE(in)
      } ELSE {
        console_printlnE(in + "rejected")
      }
    }
  }

  test("ifBranches")  {
    val progStaged = new EffectsExp with IfBranchesExamples
    //pending
    val in = "abc"
    ///val actual = getStagedOutputConfig(progStaged)(progStaged.t1, "t1", in, progStaged.defaultCompilerConfig)
    val actual2 = getStagedOutputConfig(progStaged)(progStaged.t2, "t2", in, progStaged.defaultCompilerConfig)
  }

  test("zipArrayWithIndex")  {
    val progStaged = new EffectsExp with StateExamples with MonadsDslExp {
      val State = new State0Manager[Int]
    }
    //pending
    val in = Array(10.0, 20.0, 30.0)
    val res = getStagedOutputConfig(progStaged)(progStaged.zipArrayWithIndexW, "zipArrayWithIndex", in, progStaged.defaultCompilerConfig)
  }

  test("zipCollectionWithIndex")  {
    val progStaged = new EffectsExp with StateExamples with MonadsDslExp {
      val State = new State0Manager[Int]
    }
    //pending
    val in = Array(10.0, 20.0, 30.0)
    val res = getStagedOutputConfig(progStaged)(progStaged.zipCollectionWithIndexW, "zipCollectionWithIndex", in, progStaged.defaultCompilerConfig)
  }

  test("zipCollectionWithIndex2")  {
    val progStaged = new EffectsExp with StateExamples with MonadsDslExp {
      val State = new State0Manager[Int]
    }
    //pending
    val in = Array(10.0, 20.0, 30.0)
    val res = getStagedOutputConfig(progStaged)(progStaged.zipCollectionWithIndexW2, "zipCollectionWithIndex2", in, progStaged.defaultCompilerConfig)
  }

  test("zipCollectionWithIndex3")  {
    val progStaged = new EffectsExp with StateExamples with MonadsDslExp {
      val State = new State0Manager[Int]
    }
    //pending
    val in = Array(10.0, 20.0, 30.0)
    val res = getStagedOutputConfig(progStaged)(progStaged.zipCollectionWithIndexW3, "zipCollectionWithIndex3", in, progStaged.defaultCompilerConfig)
  }

}
