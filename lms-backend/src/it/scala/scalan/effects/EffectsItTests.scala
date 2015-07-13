package scalan.effects

import scalan.compilation.lms.cxx.LmsCompilerCxx
import scalan.compilation.lms.uni.LmsCompilerUni
import scalan.examples.{AuthenticationsDslExp, AuthenticationsDslSeq, InteractionsDslSeq, InteractionsDslExp}
import scalan.it.lms.ItTestsUtilLmsCxx
import scalan.monads.MonadsDslExp
import scalan.primitives.EffectfulCompiler
import scalan.{ScalanCommunityDsl, ScalanCommunityDslExp, ScalanCommunityDslSeq, ScalanCommunitySeq}
import scalan.collections.{MultiMapsDslSeq}
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.compilation.lms.cxx.sharedptr.CommunityCxxShptrLmsBackend
import scalan.it.{ItTestsUtil, BaseItTests}

class EffectsItTests extends BaseItTests with ItTestsUtilLmsCxx
{
  trait EffectsExp extends CommunityLmsCompilerScala with CoreBridge
                      with ScalanCommunityDslExp
                      with EffectfulCompiler {
    val lms = new CommunityLmsBackend
  }

  trait EffectsExpUni extends LmsCompilerUni with CoreBridge
                      with ScalanCommunityDslExp
                      with EffectfulCompiler

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

  // TODO: Slow test
  ignore("runCrossDomain")  {
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

    val progStagedU = new EffectsExpUni with StateExamples with MonadsDslExp {
      val State = new State0Manager[Int]
    }
    val resU = getStagedOutputConfig(progStagedU)(progStagedU.zipArrayWithIndexW, "zipArrayWithIndex", in, progStagedU.defaultCompilerConfig)

    assert(res.sameElements(resU))
  }

  test("zipCollectionWithIndex")  {
    val progStaged = new EffectsExp with StateExamples with MonadsDslExp {
      val State = new State0Manager[Int]
    }
    //pending
    val in = Array(10.0, 20.0, 30.0)
    val res = getStagedOutputConfig(progStaged)(progStaged.zipCollectionWithIndexW, "zipCollectionWithIndex", in, progStaged.defaultCompilerConfig)

    val progStagedU = new EffectsExpUni with StateExamples with MonadsDslExp {
      val State = new State0Manager[Int]
    }
    val resU = getStagedOutputConfig(progStagedU)(progStagedU.zipCollectionWithIndexW, "zipCollectionWithIndex", in, progStagedU.defaultCompilerConfig)

    assert(res.sameElements(resU))
  }

  test("zipCollectionWithIndex2")  {
    val progStaged = new EffectsExp with StateExamples with MonadsDslExp {
      val State = new State0Manager[Int]
    }
    //pending
    val in = Array(10.0, 20.0, 30.0)
    val res = getStagedOutputConfig(progStaged)(progStaged.zipCollectionWithIndexW2, "zipCollectionWithIndex2", in, progStaged.defaultCompilerConfig)

    val progStagedU = new EffectsExpUni with StateExamples with MonadsDslExp {
      val State = new State0Manager[Int]
    }
    val resU = getStagedOutputConfig(progStagedU)(progStagedU.zipCollectionWithIndexW2, "zipCollectionWithIndex2", in, progStagedU.defaultCompilerConfig)

    assert(res.sameElements(resU))
  }

  test("zipCollectionWithIndex3")  {
    val progStaged = new EffectsExp with StateExamples with MonadsDslExp {
      val State = new State0Manager[Int]
    }
    //pending
    val in = Array(10.0, 20.0, 30.0)
    val res = getStagedOutputConfig(progStaged)(progStaged.zipCollectionWithIndexW3, "zipCollectionWithIndex3", in, progStaged.defaultCompilerConfig)

    val progStagedU = new EffectsExpUni with StateExamples with MonadsDslExp {
      val State = new State0Manager[Int]
    }
    val resU = getStagedOutputConfig(progStagedU)(progStagedU.zipCollectionWithIndexW3, "zipCollectionWithIndex3", in, progStagedU.defaultCompilerConfig)

    assert(res.sameElements(resU))
  }

  // TODO: Slow test, takes a very long time due to the problems with higher-kinded types
  ignore("zipCollectionWithIndex3_Free")  {
    val progStaged = new EffectsExp with StateExamples with MonadsDslExp {
      val State = new FreeStateManager[Int]
    }
    val in = Array(10.0, 20.0, 30.0)
    val res = getStagedOutputConfig(progStaged)(progStaged.zipCollectionWithIndexW3, "zipCollectionWithIndex3_Free", in, progStaged.defaultCompilerConfig)
  }
}

class EffectsJniItTests extends BaseItTests with ItTestsUtilLmsCxx {

  trait EffectsExpCxx extends LmsCompilerCxx  with StateExamples with MonadsDslExp
      with CoreBridge with JNIBridge with ScalanCommunityDslExp with EffectfulCompiler
  {
    val lms = new CommunityCxxShptrLmsBackend

    override val State = new State0Manager[Int]

    lazy val jniZipArrayWithIndexW = fun {arr: Rep[JNIType[Array[Double]]] =>
      JNI_Pack( zipArrayWithIndexW( JNI_Extract(arr) ) )
    }

    lazy val jniZipCollectionWithIndexW = fun {arr: Rep[JNIType[Array[Double]]] =>
      JNI_Pack( zipCollectionWithIndexW( JNI_Extract(arr) ) )
    }

    lazy val jniZipCollectionWithIndexW2 = fun {arr: Rep[JNIType[Array[Double]]] =>
      JNI_Pack( zipCollectionWithIndexW2( JNI_Extract(arr) ) )
    }

    lazy val jniZipCollectionWithIndexW3 = fun {arr: Rep[JNIType[Array[Double]]] =>
      JNI_Pack( zipCollectionWithIndexW3( JNI_Extract(arr) ) )
    }
  }

  test("jniZipArrayWithIndex") {
    val progcxx = new EffectsExpCxx {}
    generate(progcxx)(progcxx.jniZipArrayWithIndexW,"jniZipArrayWithIndex")(progcxx.defaultCompilerConfig)
  }
  test("jniZipCollectionWithIndex") {
    val progcxx = new EffectsExpCxx {}
    generate(progcxx)(progcxx.jniZipCollectionWithIndexW,"jniZipCollectionWithIndex")(progcxx.defaultCompilerConfig)
  }
  test("jniZipCollectionWithIndex2") {
    val progcxx = new EffectsExpCxx {}
    generate(progcxx)(progcxx.jniZipCollectionWithIndexW2,"jniZipCollectionWithIndex2")(progcxx.defaultCompilerConfig)
  }
  test("jniZipCollectionWithIndex3") {
    val progcxx = new EffectsExpCxx {}
    generate(progcxx)(progcxx.jniZipCollectionWithIndexW3,"jniZipCollectionWithIndex3")(progcxx.defaultCompilerConfig)
  }
}
