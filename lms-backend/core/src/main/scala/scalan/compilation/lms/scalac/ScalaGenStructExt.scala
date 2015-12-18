package scalan.compilation.lms.scalac

import scala.lms.common.ScalaGenStruct
import scala.reflect.RefinedManifest

trait ScalaGenStructExt extends ScalaGenStruct {
  import IR._

  override def remap[A](m: Manifest[A]) = m match {
    case rm: RefinedManifest[_] =>
      val className = structName(rm)
      encounteredStructs += className -> rm.fields
      className
    case _ => super.remap(m)
  }
}
