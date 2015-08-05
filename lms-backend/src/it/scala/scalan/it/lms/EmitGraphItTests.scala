package scalan.it.lms

import org.scalatest.BeforeAndAfterAll

import scala.collection.mutable
import scala.language.reflectiveCalls
import scalan.compilation.lms._
import scalan.compilation.lms.cxx.sharedptr.CoreCxxShptrLmsBackend
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.it.smoke.CommunitySmokeItTests
import scalan.linalgebra.MatricesDslExp
import scalan.util.FileUtil
import scalan.{CommunityMethodMappingDSL, ScalanCommunityDslExp, ScalanCommunityExp, ScalanCtxExp}

/**
 * Created by adel on 4/10/15.
 */
class EmitGraphItTests extends CommunitySmokeItTests with BeforeAndAfterAll {

  trait Prog extends ProgCommunity {

    lazy val emptyIf = fun { (in: Rep[(Boolean, (Double, Double))]) => {
      val Pair(x, Pair(y, z)) = in
      IF(x) THEN y ELSE z
    }}

  }

  class ProgCommunityExp extends ProgCommunity with ScalanCommunityDslExp

  class ProgSeq extends ProgCommunitySeq with Prog  {}
  class ProgStaged extends ProgCommunityExp with Prog {}

  val progSeq = new ProgSeq
  val progStaged = new CommunityLmsCompilerScala with CommunityBridge with CommunityMethodMappingDSL {
    lazy val scalan = new ProgStaged
  }
  //val progGStaged = new ProgGStaged

  test("emptyIfTrue") {
    val in = (true, (5.0, 7.7))
    compareOutputWithSequential(progStaged)(progSeq.emptyIf, progStaged.scalan.emptyIf, "emptyIfTrue", in)
    //todo - open and check last (LMS) graph
  }


}

