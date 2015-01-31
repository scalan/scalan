package scalan

import java.lang.reflect.Method

import scalan.util.FileUtil

// TODO get current test name programmatically
// see http://stackoverflow.com/questions/14831246/access-scalatest-test-name-from-inside-test
/**
 * Created by slesarenko on 18/01/15.
 */
abstract class TestContext(suite: TestsUtil, testName: String) extends ScalanCtxExp {
  override def isInvokeEnabled(d: Def[_], m: Method) = true
  override def shouldUnpack(e: ViewElem[_, _]) = true
  def emit(name: String, ss: Exp[_]*) =
    emitDepGraph(ss, FileUtil.file(suite.prefix, testName, s"$name.dot"))
}
