package scalan.compilation.lms.arrays

import java.io.PrintWriter

import scala.lms.common._
import scalan.compilation.lms.BaseCodegen

trait FusionProg extends Arith with ArrayLoops with Print {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def test(x: Rep[Unit]) = {

    val constant = array(100) { i => 1 }

    val linear = array(100) { i => 2*i }

    val affine = array(100) { i => constant.at(i) + linear.at(i) }

    def square(x: Rep[Double]) = x*x
    def mean(x: Rep[Array[Double]]) = sum(x.length) { i => x.at(i) } / x.length
    def variance(x: Rep[Array[Double]]) = sum(x.length) { i => square(x.at(i)) } / x.length - square(mean(x))

    val data = affine

    val m = mean(data)
    val v = variance(data)

    print(m)
    print(v)
  }

}

trait FusionProg2 extends Arith with ArrayLoops with Print with OrderingOps {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def test(x: Rep[Unit]) = {

    def filter[T:Manifest](x: Rep[Array[T]])(p: Rep[T] => Rep[Boolean]) =
      arrayIf(x.length) { i => (p(x.at(i)), x.at(i)) }

    val range = array(100) { i => i }

    val odds = filter(range) { z => z > 50 }

    val res = sum(odds.length) { i => odds.at(i) }

    print(res)
  }

}



/*
  some thoughts on cse/gvn :

    - currently cse works fine for first-order, point-free things:
        val x = a + b
        val y = a + b
      will always be represented internally as
        val x = a + b
        val y = x

    - if bound variables are involved, cse no longer works:
        val a = array { i => 0 }
        val b = array { i => 0 }
      will create two separate objects:
        val a = array { i0 => 0 }
        val b = array { i1 => 0 }
      the same holds for lambdas.

    - this is due to the choice of representing bound vars using fresh symbols.
      alternatively we could use DeBruijn indices.

      however, some care would have to be taken in managing the indices:
        val a = array { i =>
          val b = array { j => f(j) }
          sum(b)
        }
      code motion will move b out of a ... but we know that only after looking at b's body

    - for now this is not really a problem because loop fusion will take
      care of duplicate loops (effectively lifting scalar cse to array cse)

    - another solution (as done by delite) is to wrap array { i => 0 }
      as ArrayZero(len) extends DeliteOP(array(len) { i => 0}).
      here, cse will be done on the case class representation
*/



class TestFusion extends FileDiffSuite {

  val prefix = "test7-"

  test("Fusion1") {
    withOutFile(prefix+"fusion1") {
      new FusionProg with ArithExp with ArrayLoopsExp with PrintExp { self =>
        val codegen = new BaseCodegen[self.type] with ScalaGenArrayLoops with ScalaGenArith with ScalaGenPrint { val IR: self.type = self }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion1")
  }

  test("Fusion2") {
    withOutFile(prefix+"fusion2") {
      // LoopsExp2 with ArithExp with PrintExp with BaseFatExp
      new FusionProg with ArithExp with ArrayLoopsFatExp with IfThenElseFatExp with PrintExp  { self =>
        override val verbosity = 1
        val codegen = new BaseCodegen[self.type] with ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint { val IR: self.type = self }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion2")
  }

  test("Fusion3") {
    withOutFile(prefix+"fusion3") {
      new FusionProg2 with ArithExp with ArrayLoopsFatExp with IfThenElseFatExp with PrintExp with IfThenElseExp with OrderingOpsExp  { self =>
        override val verbosity = 1
        val codegen = new BaseCodegen[self.type] with ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint
                          with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self;
          override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion3")
  }

  test("Fusion4") {
    withOutFile(prefix+"fusion4") {
      new FusionProg2 with ArithExp with ArrayLoopsFatExp with IfThenElseFatExp with PrintExp with IfThenElseExp with OrderingOpsExp  { self =>
        override val verbosity = 1
        val codegen = new BaseCodegen[self.type] with ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint
                          with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self;
          override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion4")
  }

}
