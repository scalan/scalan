package scalan.it.smoke

import scalan.{ScalanCtxSeq, ScalanCtxExp, ScalanDsl}
import scalan.it.BaseItTests
import scalan.compilation.Compiler

trait SmokeProg extends ScalanDsl {

  lazy val simpleArith = fun {x: Rep[Int] => x*x + 2}

  lazy val simpleArrGet = fun {in: Rep[(Array[Int], Int)] =>
    val arr = in._1
    val ind = in._2
    arr(ind)
  }
  lazy val simpleMap = fun {x: Rep[Array[Int]] =>
    val x1 = x.map {y:Rep[Int] => y+1}
    x1
  }
  lazy val simpleMapNested = fun {x: Rep[(Array[Array[Double]], Int)] =>
    val x1 = x._1.map {y:Rep[Array[Double]] => y(x._2)}
    x1
  }
  lazy val simpleZip = fun {x: Rep[Array[Int]] =>
    val x1 = x.map {y:Rep[Int] => y+2}
    x1 zip x
  }
  lazy val simpleZipWith = fun {x: Rep[Array[Int]] =>
    val x1 = x.map {y:Rep[Int] => y+3}
    val x2 = x1 zip x
    val x3 = x2.map {y:Rep[(Int,Int)] => y._1 * y._2}
    x3
  }

  lazy val simpleReduce = fun {x: Rep[Array[Int]] =>
    val x1 = x.reduce
    x1
  }
  lazy val mvMul = fun { in:Rep[(Array[Array[Int]], Array[Int])] =>
    val mat = in._1
    val vec = in._2
    val res = mat map {row: Rep[Array[Int]] =>
      val x1 = row zip vec
      val x2 = x1.map {y:Rep[(Int,Int)] => y._1 * y._2}
      x2.reduce
    }
    res
  }

  lazy val simpleIf = fun { in: Rep[(Array[Double], Double)] =>
    val res = IF (in._2 === 0.0) THEN { in._1 map (x => x/2.0) } ELSE { IF ( in._2 < 0.0) THEN { in._1 map (x => (x*(-1.0))/in._2) } ELSE {in._1 map (x => x/in._2) } }
    res.reduce
  }

  lazy val simpleSum = fun { x: Rep[Int] =>
    val l = x.asLeft[Unit].mapSum(_ + 7, identity)
    val r = x.asRight[Unit].mapSum(identity, _ + 3)
    (l, r)
  }

  lazy val sumOps = fun { x: Rep[Either[Unit, Int]] =>
    x.fold(_ => x.isLeft, _ => x.isRight)
  }

  lazy val optionOps = fun { x: Rep[Int] =>
    val d = 19
    val l = SOption.none[Int].map(_ + 3)
    val r = SOption.some(x).map(_ + 7).flatMap(x => SOption.some(x * 2))
    (l.getOrElse(d), r.getOrElse(d))
  }

  lazy val lambdaApply = fun2 { (x: Rep[Int], f: Rep[Int => Int]) =>
    f(x)
  }

  lazy val lambdaConst = fun { x: Rep[Int] =>
    val f = fun { x: Rep[Int] => true }
    SOption.some(f)
  }

  lazy val logicalOps = fun2 { (x: Rep[Boolean], y: Rep[Boolean]) =>
    val a = !x && y
    val b = x || !y
    (a, b)
  }

  lazy val arrayUpdate = fun {in: Rep[Array[Int]] => val f = {a:Rep[Array[Int]] => a.update(0,1)}; f(in)}

  lazy val filterCompound = fun { in: Rep[Array[(Int, (Int, Int))]] =>
    in.filter(x => x._1 >= 20 && x._2 >= x._1 && x._3 < 30)
  }

  lazy val aggregates = fun { in: Rep[Array[Int]] =>
    (in.min, in.max, in.sum, in.avg)
  }

  lazy val sortBy = fun { in: Rep[Array[(Int, Int)]] =>
    in.sortBy(fun { p => p._1})
  }

  lazy val reuseTest = fun { len: Rep[Int] =>
    val matrix: Rep[Array[Array[Int]]] = SArray.tabulate[Array[Int]](len) { n => SArray.tabulate[Int](n) { i => i}}
    matrix.map(row => row.reduce) zip matrix.map(row => row.reduce * 2)
  }

  lazy val arrayEmpty = fun { _:Rep[Int] => SArray.empty[Int]}

  lazy val arrayReplicate = fun {in:Rep[(Int,Double)] =>
    SArray.replicate(in._1, in._2)
  }

  lazy val emptyNestedUnitArray = fun {_ : Rep[Int] => array_empty[Array[Unit]]}


  lazy val pairIf = fun { in: Rep[(Int, Array[Int])] =>
    val rs = IF (in._1 > 0) THEN {
      val red = in._2.reduce
      (red + 1, red - 1)
    } ELSE {
      (0,0)
    }
    rs._1 + rs._2
  }

  lazy val arrayUpdateMany = fun { in: Rep[(Array[Int], (Array[Int], Array[Int]))] => array_updateMany(in._1, in._2, in._3) }

  lazy val listRangeFrom0 = fun { n: Rep[Int] => SList.rangeFrom0(n) }

  lazy val applyLambda2Array = fun {arr: Rep[Array[Int]] =>
    def isMatch(arr: Rep[Array[Int]]) = arr.length > 3
    def step(arr: Rep[Array[Int]]): Rep[Array[Int]] = arr map {a => a + 2}

    from(arr).until(isMatch)(step)
  }

  lazy val fillArrayBuffer = fun { in: Rep[Array[Int]] =>
    in.fold(ArrayBuffer.empty[Int], (state: Rep[ArrayBuffer[Int]], x: Rep[Int]) => state += x).toArray
  }

  lazy val makeArrayBuffer = fun { in: Rep[Array[Int]] =>
    in.fold(ArrayBuffer.make[Int]("testArrayBuffer"), (state: Rep[ArrayBuffer[Int]], x: Rep[Int]) => state += x).toArray
  }

  lazy val stringCompare = fun { in: Rep[(String, String)] =>
    val Pair(a,b) = in
    a.compare(b)
  }
  //    lazy val absTest = fun {x: Rep[Float] =>
  //      x.abs
  //    }
  //
  //    lazy val logTest = fun {x: Rep[(Double, Double)] =>
  //      Pair(Math.log(x._1), Math.log(x._2))
  //    }
  //
  //    // sums elements of an array in imperative way
  //    lazy val simpleLoop = fun {x: PA[Int] =>
  //      val s = loopUntil((0, 0)) (
  //      {s: Rep[(Int, Int)] => s._2 >= x.length},
  //      {s: Rep[(Int, Int)] => (s._1 + x(s._2), s._2 + 1): Rep[(Int, Int)]}
  //      )
  //      singleton(s._1)
  //    }
  //
  //    lazy val simpleFirstElement = fun {x: PA[Int] =>
  //      singleton(x(0))
  //    }
  //
  //    lazy val simpleLoop2 = fun {x: PA[Int] =>
  //      val res = loopUntil(0) (
  //        {s: Rep[Int] => s >= x.length},
  //        {s: Rep[Int] => s + 1}
  //      )
  //      singleton(res)
  //    }
  //
  //    // appends two arrays
  //    def appendPA[A:Elem](xs: PA[A], ys: PA[A]): PA[A] =
  //      tabulate(xs.length + ys.length) { i => IF (i < xs.length) {xs(i)} ELSE ys(i - xs.length)}
  //
  //    def appendPA2[A:Elem](xs: PA[A], ys: PA[A]): PA[A] = {
  //      val tmp = replicate(2, xs)
  //      val z: NA[A] = (tmp <<- (1, ys))
  //      z.values
  //    }
  //
  //
  //    lazy val simpleAppend = fun { x: PA[Int] =>
  //      appendPA(x, x)
  //    }
  //
  //    lazy val simpleAppend2 = fun { x: PA[Int] =>
  //      appendPA2(x, x)
  //    }
  //
  //    lazy val idPA = fun { x: PA[Int] => x}
  //
  //    lazy val curredPlusMap = fun { xs: PA[Int] => xs.map { x => curred(x)(x) }}
  //
  //    lazy val isEven = fun { x: Rep[Int] => (x % 2) === 0 }
  //
  //    lazy val filterEven = fun {xs: PA[Int] => xs.filter(x => isEven(x)) }
  //
  //    lazy val filterPositive = fun {xs: PA[Int] => xs.filter(x => x > 0)}
  //
  //    lazy val div2Reduce = fun {xs: PA[Int] =>
  //      implicit val DivBy2Monoid: Monoid[Rep[Int]] = new Monoid[Rep[Int]] {
  //        val zero: Rep[Int] = 0
  //        def append(x: Rep[Int], y: =>Rep[Int]): Rep[Int] = (x + y) % 2
  //        def opName = "myMonoid"
  //        def isInfix = true
  //      }
  //      val res = xs.reduce(DivBy2Monoid)
  //      singleton(res)
  //    }
  //
  //    lazy val simpleFor = fun {xs: PA[Int] =>
  //      for {x <- xs if x > 2} yield x
  //    }
  //
  //    lazy val reverse = fun { xs: PA[Int] =>
  //      loopUntil2(0, xs)(
  //        {(i, ys)  => i >= xs.length},
  //        { (i, ys) => (i + 1, ys <<- (i, xs(xs.length - 1 - i)))  }
  //      )._2
  //    }
  //
  //    lazy val toFromArray = fun { xs: PA[Int] =>
  //      fromArray(xs.toArray)
  //    }
  //
  //    lazy val flatMap1 = fun { xs: PA[Int] =>
  //      xs.flatMap{x: Rep[Int] => replicate(x, x)}
  //    }
  //
  //    lazy val flatMap2 = fun { xs: PA[Int] =>
  //      xs.flatMap{x: Rep[Int] => replicate(x, x).flatMap{x: Rep[Int] => replicate(x, x)}}
  //    }
  //
  //    lazy val pairToArray = fun { p: Rep[(Int, Int)] =>
  //      val a: PA[Int] = replicate(2, p._1)
  //      a <<- (1, p._2)
  //    }
  //
  //    lazy val doubleSwap = fun { p: Rep[(Double, Double)] =>
  //      Pair(p._2, p._1)
  //    }
  //
  //    lazy val doubleFloatArray = fun { p: PA[Float] =>
  //      p |+| p
  //    }
  //
  //    lazy val doubleArray = fun { p: PA[Double] =>
  //      p |+| p
  //    }
  //
  //    lazy val valuesOfNA = fun { p: NA[Int] => p.values}
  //    lazy val valuesOfNA2 = fun { p: Rep[PArray[PArray[PArray[Int]]]] => p.values.values}
  //
  //    def sVecDVecDot(sv: PA[(Int, Float)], dv: PA[Float]): Rep[Float] = {
  //      val p = unzip(sv)
  //      val v2 =p._2 |*| dv.backPermute(p._1)
  //      sum(v2)
  //    }
  //
  //    lazy val sMatDVecMul = fun {p : Rep[(PArray[PArray[(Int, Float)]], PArray[Float])] =>
  //      val m = p._1
  //      val v = p._2
  //      m.map {row => sVecDVecDot(row, v)}
  //    }
  //
  //
  //    lazy val fusion = fun {r: Rep[(PArray[Int], PArray[Int])] =>
  //      val a1 = r._1
  //      val a2 = a1.map{x => x * 2}
  //      val a3 = a2.map{x => x * 2}
  //      val a4 = a2.map{x => x + 3}
  //      val a5 = a1.zip(a3)
  //
  //      val a6 = a5.zip(a4)
  //      val a7 = a6.map{p => (p._1, p._2 + 13)}
  //
  //      a3.zip(a7)
  //    }
  //
  //    lazy val fusion1 = fun {a1: Rep[PArray[Int]] =>
  //      val a2 = a1.map{x => x * 2}
  //      val a3 = a2.map{x => x * 2}
  //      a3
  //    }
  //
  ////    lazy val expandScaledRangesFun = fun {in: Rep[(PArray[Int],Int)] =>
  ////      val Pair(is, scale) = in
  ////      expandScaledRanges(is, scale)
  ////    }
  //
}

/**
 *  Tests that very simple examples are run correctly
 */
abstract class SmokeItTests extends BaseItTests[SmokeProg](new ScalanCtxSeq with SmokeProg) {
  test("simpleArith") {
    compareOutputWithSequential(_.simpleArith)(2)
  }
  test("simpleArrGet") {
    val in = (Array(2,3), 1)
    compareOutputWithSequential(_.simpleArrGet)(in)
  }
  test("simpleMap") {
    compareOutputWithSequential(_.simpleMap)(Array(2,3))
  }
  test("simpleMapNested") {
    val in = (Array(Array(2.0,3.0), Array(3.0,4.0)), 1)
    compareOutputWithSequential(_.simpleMapNested)(in)
  }
  test("simpleZip") {
    compareOutputWithSequential(_.simpleZip)(Array(2,3))
  }
  test("simpleZipWith") {
    compareOutputWithSequential(_.simpleZipWith)(Array(2,3))
  }
  test("simpleReduce") {
    compareOutputWithSequential(_.simpleReduce)(Array(2,3))
  }
  test("mvMul") {
    val in = (Array(Array(2,3), Array(4,5)), Array(6,7))
    compareOutputWithSequential(_.mvMul)(in)
  }
  test("simpleIf") {
    val in = (Array(2.0,3.0), 4.0)
    compareOutputWithSequential(_.simpleIf)(in)
  }
  test("optionOps") {
    compareOutputWithSequential(_.optionOps)(7)
  }
  test("logicalOps") {
    val in = (true, false)
    compareOutputWithSequential(_.logicalOps)(in)
  }
  test("filterCompound") {
    val in = Array((11, (12, 13)), (21, (22, 23)), (31, (32, 33)))
    compareOutputWithSequential(_.filterCompound)(in)
  }
  test("reuseTest") {
    compareOutputWithSequential(_.reuseTest)(5)
  }
  test("arrayEmpty") {
    compareOutputWithSequential(_.arrayEmpty)(0)
  }
  test("arrayReplicate") {
    val in = (3, 3.14)
    compareOutputWithSequential(_.arrayReplicate)(in)
  }
  test("stringCompare") {
    val in = ("abc", "abc")
    compareOutputWithSequential(_.stringCompare)(in)
  }

//  val progStaged: Prog with ScalanCtxExp

//  test("test00simpleConst") {
//    val (in, out) = Array(0) -> Array(1)
//    progSeq.simpleConst(progSeq.PArray.fromArray(in)).arr should be(out)
//    checkRun(progSeq, progStaged)(progSeq.simpleConst, progStaged.simpleConst)("00simpleConst", progSeq.PArray.fromArray(in), progSeq.PArray.fromArray(out))
//  }

//
//  @Test def test01simpleMap {
//    val (in, out) = Array(1, 2, 3, 4, 5) -> Array(2, 3, 4, 5, 6)
//    Assert.assertArrayEquals(out, progSeq.simpleMap(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.simpleMap, progStaged.simpleMap)("01simpleMap", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//
//  @Test def test02simpleLoop {
//    val (in, out) = Array(1, 2, 3, 4, 5) -> Array(15)
//    Assert.assertArrayEquals(out, progSeq.simpleLoop(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.simpleLoop, progStaged.simpleLoop)("02simpleLoop", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  @Test def test03simpleFirstElement {
//    val (in, out) = Array(3, 4) -> Array(3)
//    Assert.assertArrayEquals(out, progSeq.simpleFirstElement(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.simpleFirstElement, progStaged.simpleFirstElement)("03simpleFirstElement", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//
//  @Test def test04simpleLoop {
//    val (in, out) = Array(1, 2, 3, 4) -> Array(4)
//    Assert.assertArrayEquals(out, progSeq.simpleLoop2(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.simpleLoop2, progStaged.simpleLoop2)("04simpleLoop", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  // fails since tabulate is not implemented in staging
//  @Test def test05simpleAppend {
//    val (in, out) = Array(1, 2) -> Array(1, 2, 1, 2)
//    Assert.assertArrayEquals(out, progSeq.simpleAppend(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.simpleAppend, progStaged.simpleAppend)("05simpleAppend", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  @Test def test06simpleAppend {
//    val (in, out) = Array(1, 2) -> Array(1, 2, 1, 2)
//    Assert.assertArrayEquals(out, progSeq.simpleAppend2(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.simpleAppend2, progStaged.simpleAppend2)("06simpleAppend", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  @Test def test07simpleId {
//    val (in, out) = Array(1, 2) -> Array(1, 2)
//    Assert.assertArrayEquals(out, progSeq.idPA(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.idPA, progStaged.idPA)("07simpleId", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  @Test def test08curredPlusMap {
//    val (in, out) = Array(1, 2) -> Array(2, 4)
//    Assert.assertArrayEquals(out, progSeq.curredPlusMap(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.curredPlusMap, progStaged.curredPlusMap)("08curredPlusMap", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  @Test def test09filterEven {
//    val (in, out) = Array(1, 2, 3, 4, 5, 6) -> Array(2, 4, 6)
//    Assert.assertArrayEquals(out, progSeq.filterEven(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.filterEven, progStaged.filterEven)("09filterEven", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  // fails - currently signed integers are not supported by runtime
//  @Test def test10filterPositive {
//    val (in, out) = Array(-1, -2, 0, 1, 2, -3, 3) -> Array(1,2,3)
//    Assert.assertArrayEquals(out, progSeq.filterPositive(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.filterPositive, progStaged.filterPositive)("10filterPositive", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  // fails - currently it is impossible to use your own monoid
//  @Test def test11reduceMonoid {
//    val (in, out) = Array(1, 2, 2) -> Array(1)
//    Assert.assertArrayEquals(out, progSeq.div2Reduce(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.div2Reduce, progStaged.div2Reduce)("11div2Reduce", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  @Test def test12for {
//    val (in, out) = Array(1, 2, 3, 4, 5) -> Array(3, 4, 5)
//    Assert.assertArrayEquals(out, progSeq.simpleFor(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.simpleFor, progStaged.simpleFor)("12for", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  @Test def test13reverse {
//    val (in, out) = Array(1, 2, 3, 4, 5) -> Array(5, 4, 3, 2, 1)
//    Assert.assertArrayEquals(out, progSeq.reverse(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.reverse, progStaged.reverse)("13reverse", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  // fails for staging
//  @Test def test14toFromArray {
//    val (in, out) = Array(1, 2, 3, 4, 5) -> Array(1, 2, 3, 4, 5)
//    Assert.assertArrayEquals(out, progSeq.toFromArray(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.toFromArray, progStaged.toFromArray)("14toFromArray", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  @Test def simple15flatMap1 {
//    val (in, out) = Array(1, 2, 3) -> Array(1, 2, 2, 3, 3, 3)
//    Assert.assertArrayEquals(out, progSeq.flatMap1(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.flatMap1, progStaged.flatMap1)("15flatMap", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  // compiled cpp program fails
//  @Test def simple16flatMap2 {
//    val (in, out) = Array(1, 2) -> Array(1, 2, 2, 2, 2)
//    Assert.assertArrayEquals(out, progSeq.flatMap2(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.flatMap2, progStaged.flatMap2)("16flatMap", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  // compiled cpp program fails
//  @Test def simple17flatMap2 {
//    val (in, out) = Array(1, 2, 3) -> Array(1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3)
//    Assert.assertArrayEquals(out, progSeq.flatMap2(progSeq.fromArray(in)).toArray)
//    checkRun(progSeq, progStaged)(progSeq.flatMap2, progStaged.flatMap2)("17flatMap", progSeq.fromArray(in), progSeq.fromArray(out))
//  }
//
//  @Test def simple18pairToArray {
//    val (in, out) = intPair -> Array(1, 2)
//    Assert.assertArrayEquals(out, progSeq.pairToArray(in).toArray)
//    checkRun(progSeq, progStaged)(progSeq.pairToArray, progStaged.pairToArray)("18pairToArray", in, progSeq.fromArray(out))
//  }
//
//  @Test def simple19valuesOfNA {
//    val (in, out) = nestedArray1 -> Array(1, 2, 3, 4, 5, 6)
//    Assert.assertArrayEquals(out, progSeq.valuesOfNA(in).toArray)
//    checkRun(progSeq, progStaged)(progSeq.valuesOfNA, progStaged.valuesOfNA)("19valuesOfNA", in, progSeq.fromArray(out))
//  }
//
//  @Test def simple20valuesOfNA2 {
//    val (in, out) = nestedArray3 -> Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
//    Assert.assertArrayEquals(out, progSeq.valuesOfNA2(in).toArray)
//    checkRun(progSeq, progStaged)(valuesOfNA2, progStaged.valuesOfNA2)("20valuesOfNA2", in, progSeq.fromArray(out))
//  }
//
//  @Test def simple21simpleLog {
//    val (in, out) = in1 -> out1
//    Assert.assertEquals(out._1, progSeq.logTest(in)._1, 0.01f)
//    Assert.assertEquals(out._2, progSeq.logTest(in)._2, 0.01f)
//    val stagedOut = testRun(progSeq, progStaged)(logTest, progStaged.logTest)("21simpleLog", in)
//    Assert.assertEquals(out._1, stagedOut._1, 0.01f)
//    Assert.assertEquals(out._2, stagedOut._2, 0.01f)
//  }
//
//  @Test def simple22doubleSwap {
//    val (in, out) = in2 -> out2
//    Assert.assertEquals(out._1, progSeq.doubleSwap(in)._1, 0.01)
//    Assert.assertEquals(out._2, progSeq.doubleSwap(in)._2, 0.01)
//    val stagedOut = testRun(progSeq, progStaged)(doubleSwap, progStaged.doubleSwap)("22doubleSwap", in)
//    Assert.assertEquals(out._1, stagedOut._1, 0.01)
//    Assert.assertEquals(out._2, stagedOut._2, 0.01)
//  }
//
//  @Test def simple23sMatMul {
//    val (in, out) = (smdv, null)
//    progSeq.sMatDVecMul(in)
//    val stagedOut = testRun(progSeq, progStaged)(sMatDVecMul, progStaged.sMatDVecMul)("23matMul", in)
//    //Assert.assertEquals(out._1, stagedOut._1, 0.01)
//    //Assert.assertEquals(out._2, stagedOut._2, 0.01)
//  }
//
//  @Test def simple24fusion {
//    val (in, out) = (pairsOfArrays, null)
//    progSeq.fusion(in)
//    val stagedOut = testRun(progSeq, progStaged)(fusion, progStaged.fusion)("24fusion", in)
//    //Assert.assertEquals(out._1, stagedOut._1, 0.01)
//    //Assert.assertEquals(out._2, stagedOut._2, 0.01)
//  }
//
//  @Test def simple25fusion {
//    val (in, out) = (intArray, null)
//    progSeq.fusion1(in)
//    val stagedOut = testRun(progSeq, progStaged)(fusion1, progStaged.fusion1)("25fusion", in)
//    //Assert.assertEquals(out._1, stagedOut._1, 0.01)
//    //Assert.assertEquals(out._2, stagedOut._2, 0.01)
//  }
//
////  @Test def simple26_expandScaledRanges {
////    val in = Pair(fromArray(Array(10, 20, 30)), 2)
////    val out = Array(20, 21, 40, 41, 60, 61)
////    Assert.assertArrayEquals(out, progSeq.expandScaledRangesFun(in).toArray)
////    checkRun(progSeq, progStaged)(expandScaledRangesFun, progStaged.expandScaledRangesFun)("simple26_expandScaledRanges", in, progSeq.fromArray(out))
////  }

}
