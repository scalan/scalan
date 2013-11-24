package scalan.common

import scala.language.{implicitConversions}

trait PimpedType[X] {
  val value: X
}

object PimpedType {
  implicit def UnwrapPimpedType[X](p: PimpedType[X]): X = p.value
}
