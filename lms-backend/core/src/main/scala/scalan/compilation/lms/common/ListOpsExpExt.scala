package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.lms.common.ListOpsExp
import scalan.compilation.lms.LmsBackendFacade

trait ListOpsExpExt extends ListOpsExp { self: LmsBackendFacade =>
  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = {
    e match {
      case ListPrepend(xs,a) =>
        ListPrepend(f(xs),f(a)).asInstanceOf[Def[A]]
      case ListCons(a,xs) =>
        ListCons(f(a),f(xs)).asInstanceOf[Def[A]]
      case ListFromSeq(xs) =>
        ListFromSeq(f(xs)).asInstanceOf[Def[A]]
      case _ =>
        super.mirrorDef(e,f)
    }
  }
}
