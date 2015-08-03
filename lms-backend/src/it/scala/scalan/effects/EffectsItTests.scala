package scalan.effects

import scalan.compilation.lms.cxx.LmsCompilerCxx
import scalan.compilation.lms.uni.{LmsBackendUni, LmsCompilerUni}
import scalan.examples.{AuthenticationsDslExp, AuthenticationsDslSeq, InteractionsDslSeq, InteractionsDslExp}
import scalan.it.lms.ItTestsUtilLmsCxx
import scalan.monads.MonadsDslExp
import scalan.primitives.EffectfulCompiler
import scalan._
import scalan.collections.{MultiMapsDslSeq}
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.compilation.lms.cxx.sharedptr.CommunityCxxShptrLmsBackend
import scalan.it.{ItTestsUtil, BaseItTests}

class EffectsItTests extends BaseItTests with ItTestsUtilLmsCxx
{

  trait EffectsSeq extends ScalanCommunitySeq with ScalanCommunityDslSeq
                      with MultiMapsDslSeq

  val progInteractScala = new CommunityLmsCompilerScala with CoreBridge with EffectfulCompiler {
    val scalan = new ScalanCommunityDslExp with InteractExample with InteractionsDslExp
    val lms = new CommunityLmsBackend
  }

  val progInteractUni = new LmsCompilerUni with CoreBridge with EffectfulCompiler {
    val scalan = new ScalanCommunityDslExp with InteractExample with InteractionsDslExp with JNIExtractorOpsExp
    val lms = new LmsBackendUni
  }

  test("runInteract")  {
    val progSeq = new EffectsSeq with InteractExample with InteractionsDslSeq
    val progStaged = progInteractScala
    val in = 10
    val actual = getStagedOutputConfig(progStaged)(progStaged.scalan.runAppW, "runInteract", in, progStaged.defaultCompilerConfig)
  }

  test("runInteract2")  {
    val progSeq = new EffectsSeq with InteractExample with InteractionsDslSeq
    val progStaged = progInteractScala
    val in = 10
    val actual = getStagedOutputConfig(progStaged)(progStaged.scalan.runApp2W, "runInteract2", in, progStaged.defaultCompilerConfig)
  }

  // TODO: Slow test
  ignore("runCrossDomain")  {
    val progSeq = new EffectsSeq with CrossDomainExample
      with InteractionsDslSeq with AuthenticationsDslSeq
    val progStaged =
      new CommunityLmsCompilerScala with CoreBridge with EffectfulCompiler {
        val scalan = new ScalanCommunityDslExp with CrossDomainExample with InteractionsDslExp with AuthenticationsDslExp
        val lms = new CommunityLmsBackend
      }
    val in = 10
    val actual = getStagedOutputConfig(progStaged)(progStaged.scalan.runAppW, "runCrossDomain", in, progStaged.defaultCompilerConfig)
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
    val progStaged = new CommunityLmsCompilerScala with CoreBridge with EffectfulCompiler {
      val scalan = new ScalanCommunityDslExp with IfBranchesExamples
      val lms = new CommunityLmsBackend
    }
    //pending
    val in = "abc"
    ///val actual = getStagedOutputConfig(progStaged)(progStaged.t1, "t1", in, progStaged.defaultCompilerConfig)
    val actual2 = getStagedOutputConfig(progStaged)(progStaged.scalan.t2, "t2", in, progStaged.defaultCompilerConfig)
  }

  val progState0Scala = new CommunityLmsCompilerScala with CoreBridge with EffectfulCompiler {
    val scalan = new ScalanCommunityDslExp with StateExamples with MonadsDslExp {
      val State = new State0Manager[Int]
    }
    val lms = new CommunityLmsBackend
  }

  val progState0Uni = new LmsCompilerUni with CoreBridge with EffectfulCompiler {
    val scalan = new ScalanCommunityDslExp with StateExamples with MonadsDslExp with JNIExtractorOpsExp {
      val State = new State0Manager[Int]
    }
    val lms = new LmsBackendUni
  }

  test("zipArrayWithIndex")  {
    val in = Array(10.0, 20.0, 30.0)
    val res = getStagedOutputConfig(progState0Scala)(progState0Scala.scalan.zipArrayWithIndexW, "zipArrayWithIndex", in, progState0Scala.defaultCompilerConfig)

    val resU = getStagedOutputConfig(progState0Uni)(progState0Uni.scalan.zipArrayWithIndexW, "zipArrayWithIndex", in, progState0Uni.defaultCompilerConfig)

    assert(res.sameElements(resU))
  }

  test("zipCollectionWithIndex")  {
    //pending
    val in = Array(10.0, 20.0, 30.0)
    val res = getStagedOutputConfig(progState0Scala)(progState0Scala.scalan.zipCollectionWithIndexW, "zipCollectionWithIndex", in, progState0Scala.defaultCompilerConfig)

    val resU = getStagedOutputConfig(progState0Uni)(progState0Uni.scalan.zipCollectionWithIndexW, "zipCollectionWithIndex", in, progState0Uni.defaultCompilerConfig)

    assert(res.sameElements(resU))
  }

  test("zipCollectionWithIndex2")  {
    //pending
    val in = Array(10.0, 20.0, 30.0)
    val res = getStagedOutputConfig(progState0Scala)(progState0Scala.scalan.zipCollectionWithIndexW2, "zipCollectionWithIndex2", in, progState0Scala.defaultCompilerConfig)

    val resU = getStagedOutputConfig(progState0Uni)(progState0Uni.scalan.zipCollectionWithIndexW2, "zipCollectionWithIndex2", in, progState0Uni.defaultCompilerConfig)

    assert(res.sameElements(resU))
  }

  test("zipCollectionWithIndex3")  {
    //pending
    val in = Array(10.0, 20.0, 30.0)
    val res = getStagedOutputConfig(progState0Scala)(progState0Scala.scalan.zipCollectionWithIndexW3, "zipCollectionWithIndex3", in, progState0Scala.defaultCompilerConfig)

    val resU = getStagedOutputConfig(progState0Uni)(progState0Uni.scalan.zipCollectionWithIndexW3, "zipCollectionWithIndex3", in, progState0Uni.defaultCompilerConfig)

    assert(res.sameElements(resU))
  }

  // TODO: Slow test, takes a very long time due to the problems with higher-kinded types
  ignore("zipCollectionWithIndex3_Free")  {
    val progStaged = new CommunityLmsCompilerScala with CoreBridge with EffectfulCompiler {
      val scalan = new ScalanCommunityDslExp with StateExamples with MonadsDslExp {
        val State = new FreeStateManager[Int]
      }
      val lms = new CommunityLmsBackend
    }

    val in = Array(10.0, 20.0, 30.0)
    val res = getStagedOutputConfig(progStaged)(progStaged.scalan.zipCollectionWithIndexW3, "zipCollectionWithIndex3_Free", in, progStaged.defaultCompilerConfig)
  }
}

class EffectsJniItTests extends BaseItTests with ItTestsUtilLmsCxx {

  class EffectsExpCxx extends ScalanCommunityDslExp with JNIExtractorOpsExp with StateExamples with MonadsDslExp
  {

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
  val progcxx = new LmsCompilerCxx with CoreBridge with JNIBridge with EffectfulCompiler {
    val scalan = new EffectsExpCxx
    val lms = new CommunityCxxShptrLmsBackend
  }

  test("jniZipArrayWithIndex") {
    generate(progcxx)(progcxx.scalan.jniZipArrayWithIndexW,"jniZipArrayWithIndex")(progcxx.defaultCompilerConfig)
  }
  test("jniZipCollectionWithIndex") {
    generate(progcxx)(progcxx.scalan.jniZipCollectionWithIndexW,"jniZipCollectionWithIndex")(progcxx.defaultCompilerConfig)
  }
  test("jniZipCollectionWithIndex2") {
    generate(progcxx)(progcxx.scalan.jniZipCollectionWithIndexW2,"jniZipCollectionWithIndex2")(progcxx.defaultCompilerConfig)
  }
  test("jniZipCollectionWithIndex3") {
    generate(progcxx)(progcxx.scalan.jniZipCollectionWithIndexW3,"jniZipCollectionWithIndex3")(progcxx.defaultCompilerConfig)
  }
}
