package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.virtualization.lms.common.FunctionsExp
import scalan.compilation.lms.LmsBackendFacade

trait FunctionsExpExt extends FunctionsExp { self: LmsBackendFacade =>
  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = {
    e match {
      case Lambda(fu, x, y) =>
        Lambda(f(fu),f(x),f(y)).asInstanceOf[Def[A]]
      case _ =>
        super.mirrorDef(e,f)
    }
  }
}
