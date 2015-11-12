package scalan.compilation.lms.arrays

import java.io.PrintWriter

import scala.lms.common._

class TestMutation extends FileDiffSuite {

  val prefix = "test8-"

  trait DSL extends ArrayMutation with Arith with OrderingOps with Variables with IfThenElse with While with RangeOps with Print {
    def zeros(l: Rep[Int]) = array(l) { i => 0 }
    def mzeros(l: Rep[Int]) = zeros(l).mutable
    def infix_toDouble(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

    def test(x: Rep[Int]): Rep[Unit]
  }
  trait Impl extends DSL with ArrayMutationExp with ArithExp with OrderingOpsExp with VariablesExp
                     with IfThenElseExp with WhileExp with RangeOpsExp with PrintExp { self =>
    override val verbosity = 2
    val codegen = new ScalaGenArrayMutation with ScalaGenArith with ScalaGenOrderingOps
                      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenWhile with ScalaGenRangeOps
                      with ScalaGenPrint { val IR: self.type = self }
    codegen.emitSource(test, "Test", new PrintWriter(System.out))
  }

  test("Mutation1") {
    withOutFile(prefix+"mutation1") {
      // a write operation must unambigously identify the object being mutated
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          val vector1 = mzeros(100)
          val vector2 = mzeros(100)
          val a = if (x > 7) vector1 else vector2

          a.update(40,40) // error: not clear which object is mutated (vector1 or vector2)

          print(a.at(50))
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation1")
  }

  test("Mutation1b") {
    withOutFile(prefix+"mutation1b") {
      // a write operation must unambigously identify the object being mutated
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          val vector1 = mzeros(100)
          val vector2 = mzeros(100)
          val a = if (x > 7) vector1 else vector2

          val a2 = a.mutable
          a2.update(40,40) // ok: we have made a copy

          print(a2.at(50))
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation1b")
  }

  test("Mutation2") {
    withOutFile(prefix+"mutation2") {
      // an operation that might read from mutable data v will be serialized with all writes to v
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          val vector1 = mzeros(100)
          val vector2 = mzeros(100)
          val a = if (x > 7) vector1 else vector2

          val x0 = a.at(10)

          vector1.update(10,10) // must come after x0
          vector2.update(10,20) // must come after x0

          val x1 = a.at(10) // must come after both writes, no cse with x0

          print(x1-x0) // minus should not have effect dep
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation2")
  }


  test("Mutation3") {
    withOutFile(prefix+"mutation3") {
      // vars may not reference mutable objects
      trait Prog extends DSL with LiftVariables {
        def test(x: Rep[Int]) = {
          var a = zeros(100)
          val b = mzeros(100)
          for (i <- 0 until b.length) { // this is also a curious case: range creation must not be reflected
          val x1 = a.at(i)
            b.update(i,8)
            val x2 = a.at(i) // must be cse'd
            a = b // error: here we learn that reads on a would need to be serialized with b but it's too late...
          }
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation3")
  }

  test("Mutation3b") {
    withOutFile(prefix+"mutation3b") {
      // vars may not reference mutable objects
      trait Prog extends DSL with LiftVariables {
        def test(x: Rep[Int]) = {
          var a = zeros(100)
          val b = mzeros(100)
          for (i <- 0 until b.length) {
            val x1 = a.at(i)
            b.update(i,8)
            val x2 = a.at(i) // must be cse'd
            a = b.clone // ok: making a copy
          }
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation3b")
  }

  test("Mutation4") {
    withOutFile(prefix+"mutation4") {
      // mutable objects cannot be nested
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          val a = mzeros(100)
          val b = array(10) { i => a } // nested array
          val b1 = b.mutable // error: internal arrays are mutable on their own
          val x1 = b1.at(5).at(50)
          print(x1)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation4")
  }

  test("Mutation4b") {
    withOutFile(prefix+"mutation4b") {
      // mutable objects cannot be nested
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          val a = mzeros(100)
          val b = array(10) { i => a } // nested array
          val b1 = b.clone
          val b2 = b1.mutable // error: internal arrays are *still* mutable, despite shallow clone
          val x1 = b2.at(5).at(50)
          print(x1)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation4b")
  }

  test("Mutation4c") {
    withOutFile(prefix+"mutation4c") {
      // mutable objects cannot be nested
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          val a = mzeros(100)
          val b = array(10) { i => a.clone } // nested array
          val b1 = b.mutable // ok: internal arrays are immutable
          val x1 = b1.at(5).at(50)
          print(x1)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation4c")
  }

  test("Mutation5") {
    withOutFile(prefix+"mutation5") {
      // mutable objects cannot be nested
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          val a = zeros(100)
          val b = array(10) { i => a } // nested array
          val b1 = b.mutable

          val c = mzeros(20)
          b1.update(4,a) // ok: insert immutable array
          b1.update(5,c) // error: cannot insert mutable array

          c.update(50,50)
          val x1 = b1.at(5).at(50)
          print(x1)
        }
      }

      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation5")
  }

  test("Mutation6") {
    withOutFile(prefix+"mutation6") {
      // mutate nested object (within an immutable one)
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          val a = mzeros(100)
          val b = array(10) { i => a } // nested array
          val u = array(10) { i => zeros(100) }
          val c = if (x > 7) b else u

          val x1 = c.at(5).at(50)

          a.update(50,50)

          val x2 = c.at(5).at(50) // no cse, must serialize with update to a

          print(x2-x1)
        }
      }

      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation6")
  }

  test("Mutation7") {
    withOutFile(prefix+"mutation7") {
      // local variables of primitive type
      trait Prog extends DSL with LiftVariables {
        def test(x0: Rep[Int]) = {
          val x = x0.toDouble // avoid codegen for implicit convert
          var c = 0.0
          while (c < x) {
            c = c + 1
          }
          if (c < x)
            c = 8
          print(c)
        }
      }

      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation7")
  }

}
