package scalan.compilation.lms.scalac

import scalan.compilation.lms.{CommunityLmsBackend, CommunityBridgeScala}

abstract class CommunityLmsCompilerScala extends LmsCompilerScala with CommunityBridgeScala {
  val lms = new CommunityLmsBackend
}