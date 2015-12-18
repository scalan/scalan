package scalan.primitives

import scalan.{ScalanDslExp, BaseTests}

class RewriteSuite extends BaseTests {
  val scalan = new ScalanDslExp
  import scalan._
  // TODO will be a describe block
  {
    val rand1 = random(5)

    val rand2 = random(5)

    test("A random should be equal to itself") {
      (rand1 === rand1) shouldEqual toRep(true)
    }

    test("Different randoms shouldn't be equal") {
      (rand1 === rand2) should not be a[Const[_]]
    }
  }

  test("Constants should propagate") {
    (toRep(4) + 5 > toRep(1) * 3) shouldEqual toRep(true)
  }

  test("One-sided constant propagation") {
    val x = fresh[Boolean]

    (x && true) shouldEqual x

    (toRep(false) || x) shouldEqual x

    (x === false) shouldEqual !x

    val num = fresh[Double]

    (num * 1.0) shouldEqual num

    (toRep(0.0) * num) shouldEqual toRep(0.0)
  }
}