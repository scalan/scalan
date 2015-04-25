
import scala.reflect.runtime.universe._
//import reflect.runtime.{universe => u }
//import u.{WeakTypeTag => WTT}

import scalan._
import scalan.common.{KindsDslExp, KindsExamples, KindsTests}

class T extends KindsExamples with KindsDslExp { }
val c = new T
import c._

val s = Return(10)
val e = s.elem
val tag = e.tag
val tpe = tag.tpe
val instanceElemMap = getElemsMapFromInstanceElem(e, tpe)
val scalaMethod = tpe.member(newTermName("mapBy")).asMethod
val returnType = scalaMethod.returnType.asSeenFrom(tpe, scalaMethod.owner).normalize
val TypeRef(_, sym, List(tpe1)) = returnType

