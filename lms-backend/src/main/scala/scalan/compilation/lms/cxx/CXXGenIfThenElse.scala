package scalan.compilation.lms.cxx

import scala.virtualization.lms.common._

/**
 * Created by zotov on 2/5/15.
 */
trait CXXGenIfThenElse  extends CXXCodegen with BaseGenIfThenElse with CLikeGenEffect {
  val IR: IfThenElseExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case IfThenElse(c,a,b) =>
        //TODO: using if-else does not work
        remap(sym.tp) match {
          case "void" =>
            stream.println("if (" + quote(c) + ") {")
            emitBlock(a)
            stream.println("} else {")
            emitBlock(b)
            stream.println("}")
          case _ =>
            if (isPrimitiveType(sym.tp)) stream.println("%s %s;".format(remap(sym.tp),quote(sym)))
            stream.println("if (" + quote(c) + ") {")
            emitBlock(a)
            stream.println("%s = %s;".format(quote(sym),quote(getBlockResult(a))))
            stream.println("} else {")
            emitBlock(b)
            stream.println("%s = %s;".format(quote(sym),quote(getBlockResult(b))))
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

trait CXXGenIfThenElseFat extends CXXGenIfThenElse with CLikeGenFat with BaseGenIfThenElseFat {
  import IR._

  override def traverseStm(stm: Stm): Unit = {
    stm match {
      case TTP(lhs, mhs, rhs) =>
        rhs match {
          case SimpleFatIfThenElse(_, _, _) =>
            for( l <- lhs ) moveableSyms += l
          case _ =>
            ()
        }
      case _ =>
        ()
    }

    super.traverseStm(stm)
  }

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef) = rhs match {
    case SimpleFatIfThenElse(c,as,bs) =>
      //declare variables
      symList.foreach( {sym => emitVarDecl(sym)} )
      stream.println(s"if (${quote(c)}) {")
      emitFatBlock(as)
      (symList zip as.map(getBlockResult)).foreach( {p => emitAssignment(p._1, s"${quoteMove(p._2)}")} )
      stream.println("} else {")
      emitFatBlock(bs)
      (symList zip bs.map(getBlockResult)).foreach( {p => emitAssignment(p._1, s"${quoteMove(p._2)}")} )
      stream.println("}")
    case _ => super.emitFatNode(symList, rhs)
  }
}
