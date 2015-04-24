package scalan.compilation.lms.cxx.sharedptr

import scala.virtualization.lms.common.{BaseGenFunctions, CLikeGenEffect, FunctionsExp}

trait CxxShptrGenFunctions extends CLikeGenEffect with BaseGenFunctions with CxxShptrCodegen {
  val IR: FunctionsExp
  import IR._

  headerFiles ++= Seq("functional")

  override def remap[A](m: Manifest[A]): String = {
    m.runtimeClass match {
      case c if c == classOf[(_ => _)] =>
        val mX = m.typeArguments(0)
        val mY = m.typeArguments(1)
        s"std::function<${remap(mY)}(${remap(mX)})>"
      case _ =>
        super.remap(m)
    }
  }

  def emitLambdaDef(sym: Sym[Any], x: Exp[_], y: Block[Any]): Unit = {
    val z = getBlockResult(y)
//    stream.println( s"${remap(norefManifest(sym.tp))} ${quote(sym)} = [${freeInScope(scala.List(x), scala.List(z)).map(quote).mkString(",")}](${remap(x.tp)} ${quote(x)}) -> ${remap(z.tp)} {" )
//    stream.println( s"${remap(sym.tp)} ${quote(sym)} = [&](${remap(x.tp)} ${quote(x)}) -> ${remap(z.tp)} {" )
    val newXtp = toShptrManifest(x.tp)
    val newZtp = toShptrManifest(z.tp)
    emitValDef(quote(sym), manifest[auto_t], src"[=](${remap(newXtp)} ${quote(x)}) -> ${remap(newZtp)} {")
    emitBlock(y)
    if (remap(newZtp) != "void")
      stream.println("return " + quote(z) + ";")
    stream.println("};")
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Lambda(fun, x, y) =>
      emitLambdaDef(sym, x, y)
    case Apply(fun, arg) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}