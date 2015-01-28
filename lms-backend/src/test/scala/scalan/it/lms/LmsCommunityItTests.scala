package scalan.it.lms

import scalan.community.{ScalanCommunityExp, ScalanCommunityDslExp}
import scalan.compilation.lms._
import scalan.compilation.lms.scala.{CommunityScalaLmsBackend, LmsCompilerScala}
import scalan.it.smoke.CommunitySmokeItTests

class LmsCommunityItTests extends CommunitySmokeItTests {

  class ProgCommunityExp extends ProgCommunity with ScalanCommunityExp with ScalanCommunityDslExp with LmsCompilerScala { self =>
    def makeBridge[A, B] = new CommunityBridge[A, B] {
      val scalan = self
      val lms = new CommunityScalaLmsBackend
    }
  }

  val progStaged = new ProgCommunityExp

  test("expBaseArrays") {
    val in = Array(Array(2, 3), Array(4, 5))
    compareOutputWithSequential(progStaged)(progSeq.expBaseArrays, progStaged.expBaseArrays, "expBaseArrays", in)
  }
}
