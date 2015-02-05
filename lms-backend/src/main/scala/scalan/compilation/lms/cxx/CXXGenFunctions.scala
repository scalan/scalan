package scalan.compilation.lms.cxx

import scala.virtualization.lms.common.{FunctionsExp, BaseGenFunctions, CLikeGenEffect}

trait CXXGenFunctions extends CLikeGenEffect with BaseGenFunctions with CXXCodegen {
  val IR: FunctionsExp
  import IR._

  override def remap[A](m: Manifest[A]): String = {
    m.runtimeClass match {
      case c if c == classOf[(_ => _)] =>
        val mX = m.typeArguments(0)
        val mY = m.typeArguments(1)
        s"std::function<${remap(norefManifest(mY))}(${remap(norefManifest(mX))})>"
      case _ =>
        super.remap(m)
    }
  }

  def emitLambdaDef(sym: Sym[Any], x: Exp[_], y: Block[Any]): Unit = {
    val z = getBlockResult(y)
    stream.println( s"${remap(norefManifest(sym.tp))} ${quote(sym)} = [&](${remap(x.tp)} ${quote(x)}) -> ${remap(z.tp)} {" )
    emitBlock(y)
    if (remap(z.tp) != "void")
      stream.println("return " + quote(z) + ";")
    stream.println("};")
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Lambda(fun, x, y) =>
      emitLambdaDef(sym, x, y)
    case Apply(fun, arg) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}