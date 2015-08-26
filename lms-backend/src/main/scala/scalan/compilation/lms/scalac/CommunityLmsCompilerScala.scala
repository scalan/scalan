package scalan.compilation.lms.scalac

import scalan.ScalanCommunityDslExp
import scalan.compilation.lms.{CommunityLmsBackend, CommunityBridgeScala}

class CommunityLmsCompilerScala[+ScalanCake <: ScalanCommunityDslExp](_scalan: ScalanCake) extends LmsCompilerScala(_scalan) with CommunityBridgeScala {
  val lms = new CommunityLmsBackend
}