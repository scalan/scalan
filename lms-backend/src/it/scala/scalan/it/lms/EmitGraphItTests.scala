package scalan.it.lms

import scalan.compilation.lms._
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.it.BaseItTests
import scalan._

/**
 * Created by adel on 4/10/15.
 */
class EmitGraphItTests extends BaseItTests {

  trait Prog extends ScalanDsl {

    lazy val emptyIf = fun { (in: Rep[(Boolean, (Double, Double))]) =>
      val Pair(x, Pair(y, z)) = in
      IF(x) THEN y ELSE z
    }

  }

  class ProgSeq extends ScalanCtxSeq with Prog
  class ProgStaged extends ScalanCommunityDslExp with Prog

  val progSeq = new ProgSeq
  val progStaged = new CommunityLmsCompilerScala(new ProgStaged) with CommunityBridge with CommunityMethodMappingDSL
  val defaultCompilers = compilers(progStaged)

  test("emptyIfTrue") {
    val in = (true, (5.0, 7.7))
    compareOutputWithSequential(_.emptyIf, "emptyIfTrue")(in)
    //todo - open and check last (LMS) graph
  }
}
