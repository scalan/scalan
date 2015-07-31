package scalan.it

import scalan.{BaseTests, TestContexts, BaseCtxTests}

abstract class BaseItTests extends BaseTests with ItTestsUtil {
/*
  var currentTestName = ""

  override def withFixture(test: NoArgTest) = {
    //todo lock currentTestName
    currentTestName = test.name
    super.withFixture(test)
  }

  def compareOutputWithSequential[A, B](back : scalan.compilation.Compiler)
                                       (fSeq : scala.Function1[A, B], f : back.Exp[A => B], input : A)
                                       (implicit comparator : (B, B) => Unit) =
  {
    val testName = currentTestName
    //todo unlock currentTestName
    super.compareOutputWithSequential(back)(fSeq, f, testName, input)
  }

*/
}

abstract class BaseCtxItTests extends BaseItTests with TestContexts