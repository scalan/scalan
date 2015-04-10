package scalan.it.lms

import org.scalatest.BeforeAndAfterAll

import scala.collection.mutable
import scala.language.reflectiveCalls
import scalan.compilation.lms._
import scalan.compilation.lms.cxx.sharedptr.CoreCxxShptrLmsBackend
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.linalgebra.MatricesDslExp
import scalan.util.FileUtil
import scalan.{CommunityMethodMappingDSL, ScalanCommunityDslExp, ScalanCommunityExp, ScalanCtxExp}

/**
 * Created by adel on 4/10/15.
 */
class EmitGraphItTests extends LmsCommunityItTests with BeforeAndAfterAll {

  trait Prog extends ProgCommunity {

    lazy val emptyIf = fun { (in: Rep[(Boolean, (Double, Double))]) => {
      val Pair(x, Pair(y, z)) = in
      IF(x) THEN y ELSE z
    }}

  }

  class ProgSeq extends ProgCommunitySeq with Prog  {}
  class ProgStaged extends ProgCommunityExp with  Prog {}

  override val progSeq = new ProgSeq
  override val progStaged = new ProgStaged
  //val progGStaged = new ProgGStaged

  test("emptyIfTrue") {
    val in = (true, (5.0, 7.7))
    compareOutputWithSequential(progStaged)(progSeq.emptyIf, progStaged.emptyIf, "emptyIfTrue", in)
    //todo - open and check last (LMS) graph
  }


}

/*
package scalan.it.lms

import org.scalatest.BeforeAndAfterAll

import scala.collection.mutable
import scala.language.reflectiveCalls
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.linalgebra.MatricesDslExp
import scalan.util.FileUtil
import scalan.{CommunityMethodMappingDSL, ScalanCommunityDslExp, ScalanCommunityExp, ScalanCtxExp}

class MethodCallItTests extends LmsCommunityItTests with BeforeAndAfterAll{

  override def beforeAll() = {
    FileUtil.deleteIfExist(prefix)
  }

  trait Prog extends ProgCommunity  {

    lazy val emptyIf = fun { (in: Rep[(Boolean, (Double, Double))] ) =>  {
      val Pair(x, Pair(y, z)) = in
      IF(x) THEN y ELSE z
    }}

    lazy val exceptionWithIfFalse = fun { (p: Rep[(Int, (SThrowable, SThrowable))]) => {
      val Pair(n, Pair(t1, t2)) = p
      IF(n>0) THEN t2.getMessage ELSE t1.initCause(t2).getMessage
    }
    }

    lazy val exceptionWithIfTrue = fun { (p: Rep[(Int, (SThrowable, SThrowable))]) => {
      //val Pair(n, Pair(t1, t2)) = p
      val Pair(n, Pair(t1, t2)) = p
      IF(n<=0) THEN t2.getMessage ELSE t1.initCause(t2).getMessage
    }
    }

    lazy val arrayLengthFun = fun { (v: Rep[Array[Int]] ) =>  {
      //v.arrayLength
      v.length
    }}

    lazy val arrayOneArg = fun { (in: Rep[(Array[Double], Int)] ) =>  {
      in._1.apply(in._2)
    }}

    lazy val easyMap = fun { (in: Rep[(Array[Double], (Double, Double))] ) =>  {
      val Pair(m0, Pair(vFrom, vTo)) = in
      val m:Rep[Array[Double]] = m0.map(x => x*x)
      m.reduce
    }}

    lazy val mapWithLambda = fun { (in: Rep[(Array[Double], (Double, Double))] ) =>  {
      val Pair(m0, Pair(vFrom, vTo)) = in
      def f(x:Rep[Double],y:Rep[Double], z:Rep[Double]): Rep[Double] =  {
        x*y+z
      }

      val m:Rep[Array[Double]] = m0.map(x => f(x, vFrom, vTo))

      m.reduce
    }}

    lazy val mapWithLambdaIf = fun { (in: Rep[(Array[Double], (Double, Double))] ) =>  {
      val Pair(m0, Pair(vFrom, vTo)) = in
      def f(x:Rep[Double],y:Rep[Double], z:Rep[Double]): Rep[Double] =  {
        IF(x===y) THEN z ELSE x
      }

      val m:Rep[Array[Double]] = m0.map(x => f(x, vFrom, vTo))

      m.reduce
    }}

    lazy val mapWithLambdaIfGt = fun { (in: Rep[(Array[Double], (Double, Double))] ) =>  {
      val Pair(m0, Pair(vFrom, vTo)) = in
      def f(x:Rep[Double],y:Rep[Double], z:Rep[Double]): Rep[Double] =  {
        IF(x<y) THEN x ELSE z
      }

      val m:Rep[Array[Double]] = m0.map(x => f(x, vFrom, vTo))

      m.reduce
    }}
  }

  class ProgSeq extends ProgCommunitySeq with Prog  {}
  class ProgStaged extends ProgCommunityExp with  Prog {}

  override val progSeq = new ProgSeq
  override val progStaged = new ProgStaged



  test("emptyIfTrue") {
    val in = (true, (5.0, 7.7))
    compareOutputWithSequential(progStaged)(progSeq.emptyIf, progStaged.emptyIf, "emptyIfTrue", in)
  }

  test("emptyIfFalse") {
    val in = (false, (5.0, 7.7))
    compareOutputWithSequential(progStaged)(progSeq.emptyIf, progStaged.emptyIf, "emptyIfFalse", in)
  }

 */
