package scalan.compilation.lua

import scalan.ScalanDslExp
import scalan.compilation.Compiler

abstract class LuaCompiler[+ScalanCake <: ScalanDslExp](_scalan: ScalanCake) extends Compiler(_scalan) {
  import scalan._

  protected def toLuaValue(x: Any, eX: Elem[_]): String = eX match {
    case UnitElement => ""
    case BooleanElement => ""
//    case IntElement => ""
//    case DoubleElement => ""
//    case LongElement => ""
//    case FloatElement => ""
//    case StringElement => ""
//    case el: ArrayElem[_] => ""
//    case PairElem(eFst, eSnd) => ""
//    case StructElem(_, fields) => ""
    case _ => !!!(s"Can't convert $x of type ${eX.name} to a Lua value")
  }

  // should check type before conversion?
  protected def fromLuaValue[A](lv: Any, eA: Elem[A]): A = (eA match {
    case UnitElement => ()
    case BooleanElement => lv
//    case IntElement => lv
//    case DoubleElement => lv
//    case LongElement => lv
//    case FloatElement => lv
//    case StringElement => lv
//    case el: ArrayElem[_] => lv
//    case PairElem(eFst, eSnd) => lv
//    case StructElem(tag, elemFields) => lv
//    case _ => !!!(s"Can't convert LuaValue $lv to JVM value of type ${eA.name}")
  }).asInstanceOf[A]
}
