package scalan.compilation.lms.cxx.sharedptr

import java.io.PrintWriter

import scala.lms.common.{CGenTupledFunctions, CLikeGenEffect, TupledFunctionsExp}

trait CxxShptrGenFunctions extends CLikeGenEffect with CGenTupledFunctions with CxxShptrCodegen {
  val IR: TupledFunctionsExp
  import IR._

  headerFiles ++= Seq("functional")

  override def remap[A](m: Manifest[A]): String = {
    m.runtimeClass match {
      case c if c == classOf[(_ => _)] =>
        val mX = m.typeArguments(0)
        val mY = m.typeArguments(1)
        src"std::function<$mY($mX)>"
      case _ =>
        super.remap(m)
    }
  }

//  def emitLambdaDef(sym: Sym[Any], x: Exp[_], y: Block[Any]): Unit = {
//    val z = getBlockResult(y)
////    stream.println( src"${norefManifest(sym.tp)} $sym = [${freeInScope(scala.List(x), scala.List(z))}](${x.tp} $x) -> ${z.tp} {" )
////    stream.println( src"${sym.tp} $sym = [&](${x.tp} $x) -> ${z.tp} {" )
//    val newXtp = toShptrManifest(x.tp)
//    val newZtp = toShptrManifest(z.tp)
//    emitValDef(quote(sym), manifest[auto_t], src"[=]($newXtp $x) -> $newZtp {")
//    emitBlock(y)
//    if (remap(newZtp) != "void")
//      stream.println(src"return $z;")
//    stream.println("};")
//  }

  // Otherwise wrong override ends up being used
  override def emitSource[A: Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) =
    super[CxxShptrCodegen].emitSource(args, body, className, out)

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Lambda(fun, x, y) =>
      if (!globals.exists(_.value == sym))
        super.emitNode(sym, rhs) // emitLambdaDef(sym, x, y) but [&] instead of [=]?
      else
        {} // already generated
    case _ => super.emitNode(sym, rhs)
  }
}