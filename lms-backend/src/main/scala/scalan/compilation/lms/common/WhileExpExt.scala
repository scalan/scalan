package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.virtualization.lms.common.{WhileExp, EffectExp, While}
import scalan.compilation.lms.LmsBackendFacade

trait WhileExpExt extends WhileExp { self: LmsBackendFacade =>
  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = {
    e match {
      case While(c,b) =>
        While(f(c),f(b)).asInstanceOf[Def[A]]
      case _ =>
        super.mirrorDef(e,f)
    }
  }
}