package scalan.compilation.lms.cxx.sharedptr

import scala.lms.common._

trait CxxShptrGenIfThenElse  extends CxxShptrCodegen with BaseGenIfThenElse with CLikeGenEffect {
  val IR: IfThenElseExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case IfThenElse(c,a,b) =>
        remap(sym.tp) match {
          case "void" =>
            stream.println("if (" + quote(c) + ") {")
            emitBlock(a)
            stream.println("} else {")
            emitBlock(b)
            stream.println("}")
          case _ =>
            if (isPrimitiveType(sym.tp))
              emitVarDecl(sym)
            stream.println("if (" + quote(c) + ") {")
            emitBlock(a)
            emitAssignment(sym, quote(getBlockResult(a)))
            stream.println("} else {")
            emitBlock(b)
            emitAssignment(sym, quote(getBlockResult(a)))
            stream.println("}")
        }
      /*
      val booll = remap(sym.tp).equals("void")
      if(booll) {
        stream.println("%s %s;".format(remap(sym.tp),quote(sym)))
        stream.println("if (" + quote(c) + ") {")
        emitBlock(a)
        stream.println("%s = %s;".format(quote(sym),quote(getBlockResult(a))))
        stream.println("} else {")
        emitBlock(b)
        stream.println("%s = %s;".format(quote(sym),quote(getBlockResult(b))))
        stream.println("}")
      }
      else {
        stream.println("if (" + quote(c) + ") {")
        emitBlock(a)
        stream.println("} else {")
        emitBlock(b)
        stream.println("}")
      }
      */
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait CxxShptrGenIfThenElseFat extends CxxShptrGenIfThenElse with CLikeGenFat with BaseGenIfThenElseFat {
  import IR._

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef) = rhs match {
    case SimpleFatIfThenElse(c,as,bs) =>
      //declare variables
      symList.foreach( {sym => emitVarDecl(sym)} )
      stream.println(src"if ($c) {")
      emitFatBlock(as)
      (symList zip as.map(getBlockResult)).foreach { p => emitAssignment(p._1, src"${p._2}") }
      stream.println("} else {")
      emitFatBlock(bs)
      (symList zip bs.map(getBlockResult)).foreach { p => emitAssignment(p._1, src"${p._2}") }
      stream.println("}")
    case _ => super.emitFatNode(symList, rhs)
  }
}
