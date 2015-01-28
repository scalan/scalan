package scalan

import java.io.File
import java.lang.reflect.Method

/**
 * Created by slesarenko on 18/01/15.
 */
trait TestContext extends ScalanCtxExp {
  override def isInvokeEnabled(d: Def[_], m: Method) = true
  override def shouldUnpack(e: ViewElem[_, _]) = true
  val prefix: File
  val subfolder: String
  def emit(name: String, ss: Exp[_]*) =
    emitDepGraph(ss, new File(prefix + subfolder, s"/$name.dot"))
}
