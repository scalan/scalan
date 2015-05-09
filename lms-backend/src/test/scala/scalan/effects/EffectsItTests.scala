package scalan.effects

import scalan.examples.{AuthenticationsDslExp, AuthenticationsDslSeq, InteractionsDslSeq, InteractionsDslExp}
import scalan.primitives.EffectfulCompiler
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
    lazy val runInteract2 = fun {in: Rep[Int] =>
      val app = runApp2
      app(in)._2
    }

    lazy val t4 = fun { (in: Rep[String]) =>
      IF (in.contains("abc")) THEN { console_printlnE(in) } ELSE { console_printlnE(in) }
    }

    lazy val t5 = fun { (in: Rep[String]) =>
      IF (in.contains(console_readlineE())) THEN { console_printlnE(in) } ELSE { console_printlnE(in) }
    }

    lazy val t6 = fun { (in: Rep[String]) =>
      val input = console_readlineE()
      val user = IF (input !== (null: String)) { input } ELSE { "admin" }
      IF (in.contains(user)) THEN {
        console_printlnE(in)
      } ELSE {
        console_printlnE(in + "rejected")
      }
    }
//    lazy val t6 = fun { (in: Rep[String]) =>
//      val input = console_readlineE()
//      val user = IF (input.length !== 0) { input } ELSE { "admin" }
//      IF (in.contains(user)) THEN { console_printlnE(in) } ELSE { console_printlnE(in) }
//    }
  }

  trait EffectsExp extends CommunityLmsCompilerScala with CoreBridge
                      with ScalanCommunityDslExp
                      with EffectfulCompiler {
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

  test("runInteract2")  {
    val progSeq = new EffectsSeq with InteractWrapper with InteractionsDslSeq
    val progStaged = new EffectsExp with InteractWrapper with InteractionsDslExp
    //pending
    val in = 10
    val actual = getStagedOutputConfig(progStaged)(progStaged.runInteract2, "runInteract2", in, progStaged.defaultCompilerConfig)
  }

  test("ifBranches")  {
    val progStaged = new EffectsExp with InteractWrapper with InteractionsDslExp
    //pending
    val in = "abc"
    ///val actual = getStagedOutputConfig(progStaged)(progStaged.t4, "t4", in, progStaged.defaultCompilerConfig)
    val actual5 = getStagedOutputConfig(progStaged)(progStaged.t5, "t5", in, progStaged.defaultCompilerConfig)
  }

  test("ifBranches2")  {
    val progStaged = new EffectsExp with InteractWrapper with InteractionsDslExp
    //pending
    val in = "abc"
    val actual = getStagedOutputConfig(progStaged)(progStaged.t6, "t6", in, progStaged.defaultCompilerConfig)
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
