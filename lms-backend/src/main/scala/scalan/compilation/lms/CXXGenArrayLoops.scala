package scalan.compilation.lms

import scala.virtualization.lms.common._
import scala.virtualization.lms.epfl.test7.{ArrayLoopsExp, ArrayLoopsFatExp}

/**
 * Created by zotov on 11/25/14.
 */
trait CXXGenFatArrayLoopsFusionOpt extends CXXGenArrayLoopsFat with CGenIfThenElseFat with LoopFusionOpt {
  val IR: ArrayLoopsFatExp with IfThenElseFatExp
  import IR._

  override def unapplySimpleIndex(e: Def[Any]) = e match {
    case ArrayIndex(a, i) => Some((a,i))
    case _ => super.unapplySimpleIndex(e)
  }
  override def unapplySimpleDomain(e: Def[Int]): Option[Exp[Any]] = e match {
    case ArrayLength(a) => Some(a)
    case _ => super.unapplySimpleDomain(e)
  }

  override def unapplySimpleCollect(e: Def[Any]) = e match {
    case ArrayElem(Block(a)) => Some(a) //TODO: block??
    case _ => super.unapplySimpleCollect(e)
  }

  override def unapplySimpleCollectIf(e: Def[Any]) = e match {
    case ArrayIfElem(c,Block(a)) => Some((a,List(c))) //TODO: block?
    case _ => super.unapplySimpleCollectIf(e)
  }

  override def applyAddCondition(e: Def[Any], c: List[Exp[Boolean]]) = e match { //TODO: should c be list or not?
    case ArrayElem(a) if c.length == 1 => ArrayIfElem(c(0),a)
    case ReduceElem(a) if c.length == 1 => ReduceIfElem(c(0),a)
    case _ => super.applyAddCondition(e,c)
  }
}

trait CXXGenArrayLoops extends CLikeGenLoops with CXXCodegen {
  val IR: ArrayLoopsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case SimpleLoop(s,x,ArrayElem(y)) =>
      stream.println("val " + quote(sym) + " = LoopArray("+quote(s)+") { " + quote(x) + " => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")
    case SimpleLoop(s,x,ReduceElem(y)) =>
      stream.println("val " + quote(sym) + " = LoopReduce("+quote(s)+") { " + quote(x) + " => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")
    // TODO: conditional variants ...
    case SimpleLoop(s,x,FlattenElem(y)) =>
      stream.println("val " + quote(sym) + " = LoopFlatten("+quote(s)+") { " + quote(x) + " => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")
    case ArrayIndex(a,i) =>
      emitValDef(quote(sym), manifest[auto_t], quote(a) + "[" + quote(i) + "]")
    case ArrayLength(a) =>
      emitValDef(quote(sym), manifest[size_t], quote(a) + ".size()")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CXXGenArrayLoopsFat extends CXXGenArrayLoops with CLikeGenLoopsFat {
  val IR: ArrayLoopsFatExp
  import IR._

  override def emitFatNode(sym: List[Sym[Any]], rhs: FatDef) = rhs match {
    case SimpleFatLoop(s,x,rhs) =>
      for ((l,r) <- sym zip rhs) {
        r match {
          case ArrayElem(y) =>
            stream.println(s"std::vector<${remap(getBlockResult(y).tp)}> ${quote(l)}(${quote(s)});")
          case ReduceElem(y) =>
            stream.println(s"auto ${quote(l)} = ${remap(getBlockResult(y).tp)}();")
          case ArrayIfElem(c,y) =>
            stream.println(s"std::vector<${remap(getBlockResult(y).tp)}> ${quote(l)};")
          case ReduceIfElem(c,y) =>
            stream.println(s"auto ${quote(l)} = ${remap(getBlockResult(y).tp)}();")
//          case FlattenElem(y) =>
//            stream.println("var " + quote(l) + " = new ArrayBuilder[" + remap(getBlockResult(y).tp) + "]")
        }
      }

      val ii = x // was: x(i)
      stream.println(s"for( ${remap(ii.tp)} ${quote(ii)} = 0; ${quote(ii)} < ${quote(s)}; ++${quote(ii)} ) {")

      emitFatBlock(syms(rhs).map(Block(_))) // TODO: check this
      for ((l,r) <- sym zip rhs) {
        r match {
          case ArrayElem(y) =>
            val q = getBlockResult(y)
            if(moveableSyms.contains(q))
              moveableSyms += l

            stream.println(quote(l) + "["+quote(ii)+"] = " + quote(q) + ";")
          case ReduceElem(y) =>
            stream.println(quote(l) + " += " + quote(getBlockResult(y)) + ";")
          case ArrayIfElem(c,y) =>
            stream.println("if ("+quote(/*getBlockResult*/(c))+") " + quote(l) + ".push_back( " + quote(getBlockResult(y)) + " );")
          case ReduceIfElem(c,y) =>
            stream.println("if ("+quote(/*getBlockResult*/(c))+") " + quote(l) + " += " + quote(getBlockResult(y)) + ";")
//          case FlattenElem(y) =>
//            stream.println(quote(l) + " ++= " + quote(getBlockResult(y)))
        }
      }
      //      stream.println(quote(ii)+" += 1")
      stream.println("}")
    case _ => super.emitFatNode(sym, rhs)
  }
}
