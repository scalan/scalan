package scalan.it.lms

import scalan.community.{ScalanCommunityExp, ScalanCommunityDslExp}
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.LmsCompilerScala
import scalan.it.smoke.CommunitySmokeItTests

class LmsCommunityItTests extends CommunitySmokeItTests {

  class ProgCommunityExp extends ProgCommunity with ScalanCommunityExp with ScalanCommunityDslExp with LmsCompilerScala { self =>
    def makeBridge = new CommunityBridge {
      val scalan = self
      val lms = new CommunityLmsBackend
    }
  }

  val progStaged = new ProgCommunityExp

  test("expBaseArrays") {
    val in = Array(Array(2, 3), Array(4, 5))
    compareOutputWithSequential(progStaged)(progSeq.expBaseArrays, progStaged.expBaseArrays, "expBaseArrays", in)
  }
}
