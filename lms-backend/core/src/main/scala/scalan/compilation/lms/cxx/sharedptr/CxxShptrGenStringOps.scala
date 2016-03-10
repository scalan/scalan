package scalan.compilation.lms.cxx.sharedptr

import scala.lms.common.StringOpsExp

trait CxxShptrGenStringOps extends CxxShptrCodegen {
  val IR: StringOpsExp
  import IR._

  headerFiles ++= Seq("boost/algorithm/string.hpp")

  override def remap[A](m: Manifest[A]) : String = {
    m match {
      case _ if m.runtimeClass == classOf[String] =>
        "std::string"
      case _ =>
        super.remap(m)
    }
  }

  override def quote(x: Exp[Any]) : String = x match {
    case c@Const(s: String) =>
      val str = "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n") + "\"" // TODO: more escapes?
      s"&$str"
    case _ =>
      super.quote(x)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StringPlus(s1,s2) if s1.tp.runtimeClass == classOf[String] && s2.tp.runtimeClass == classOf[String] =>
      emitValDef(sym, src"std::make_shared<${remap(sym.tp)}>((*$s1) + (*$s2))")
    case StringStartsWith(s1,s2) =>
      emitValDef(sym, src"boost::starts_with(*$s1,*$s2)")
    case StringEndsWith(s1,s2) =>
      emitValDef(sym, src"boost::ends_with(*$s1,*$s2)")
    case StringTrim(s) =>
      emitValDef(sym, src"boost::trim_copy(*$s)")
    case StringSplit(s, sep, Const(0)) =>
      val arr_t_str = remap(sym.tp)
      emitValDef(sym, src"std::make_shared<$arr_t_str>(boost::split($arr_t_str(), *$s, boost::is_any_of(*$sep)))")
//    case StringCharAt(s,i) =>
//      emitValDef(sym, "string_charAt(%s,%s)".format(quote(s), quote(i)))
    //case StringValueOf(a) =>
    case StringToDouble(s) =>
      emitValDef(sym, src"std::stod(*$s)")
    case StringToFloat(s) =>
      emitValDef(sym, src"std::stof(*$s)")
    case StringToInt(s) =>
      emitValDef(sym, src"std::stoi($s)")
    case StringCharAt(s,i) =>
      emitValDef(sym, src"$s[$i]")
    case StringSubstring(s,a,b) =>
      emitValDef(sym, src"$s.substr($a,$b)")
    case _ => super.emitNode(sym, rhs)
  }

}
