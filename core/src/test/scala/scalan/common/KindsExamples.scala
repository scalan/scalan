package scalan.common

import scalan.Scalan

trait KindsExamples extends Scalan with KindsDsl {
  type Id[A] = A
  lazy val t1 = fun { (in: Rep[Kind[Id,Int]]) => in }

  lazy val kindMap = fun { (in: Rep[Kind[Id,Int]]) => in.mapBy(fun {x => x + 1}) }
}
