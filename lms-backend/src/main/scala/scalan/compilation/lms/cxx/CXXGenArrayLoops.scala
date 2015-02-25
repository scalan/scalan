package scalan.compilation.lms.cxx

import scala.virtualization.lms.common._
import scala.virtualization.lms.epfl.test7.{ArrayLoopsExp, ArrayLoopsFatExp}

/**
 * Created by zotov on 11/25/14.
 */
trait CXXGenFatArrayLoopsFusionOpt extends CXXGenArrayLoopsFat with LoopFusionOpt {
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
      emitValDef(sym, quote(a) + "[" + quote(i) + "]")
    case ArrayLength(a) =>
      emitValDef(quote(sym), manifest[size_t], quote(a) + ".size()")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CXXGenArrayLoopsFat extends CXXGenArrayLoops with CLikeGenLoopsFat {
  val IR: ArrayLoopsFatExp
  import IR._

  override def traverseStm(stm: Stm): Unit = {
    stm match {
      case TTP(lhs,mhs,rhs) =>
        rhs match {
          case SimpleFatLoop(s, x, rhs) =>
            for ((l, r) <- lhs zip rhs) {
              r match {
                case ArrayElem(_) =>
                  moveableSyms += l
                case ReduceElem(_) =>
                  if(isMoveable(l))
                    moveableSyms += l
                case ArrayIfElem(_,_) =>
                  moveableSyms += l
                case ReduceIfElem(_,_) =>
                  if(isMoveable(l))
                    moveableSyms += l
                case _ =>
                  ()
              }
            }
          case _ =>
            ()
        }
      case _ =>
        ()
    }
    super.traverseStm(stm)
  }

  override def emitFatNode(sym: List[Sym[Any]], rhs1: FatDef) = rhs1 match {
    case SimpleFatLoop(s,x,rhs) =>
      stream.println(s"/*start: ${rhs1.toString}*/")
      for ((l,r) <- sym zip rhs) {
        r match {
          case ArrayElem(y) =>
            emitConstruct(l, quoteMove(s))
          case ReduceElem(y) =>
            stream.println(s"${remap(l.tp)} ${quote(l)} = ${remap(getBlockResult(y).tp)}();")
          case ArrayIfElem(c,y) =>
            emitVarDecl(l)
          case ReduceIfElem(c,y) =>
            stream.println(s"${remap(l.tp)} ${quote(l)} = ${remap(getBlockResult(y).tp)}();")
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
            stream.println(quote(l) + "["+quote(ii)+"] = " + quoteMove(getBlockResult(y)) + ";")
          case ReduceElem(y) =>
            stream.println(quote(l) + " += " + quote(getBlockResult(y)) + ";")
          case ArrayIfElem(c,y) =>
            stream.println("if ("+quote(/*getBlockResult*/(c))+") " + quote(l) + ".push_back( " + quoteMove(getBlockResult(y)) + " );")
          case ReduceIfElem(c,y) =>
            stream.println("if ("+quote(/*getBlockResult*/(c))+") " + quote(l) + " += " + quote(getBlockResult(y)) + ";")
//          case FlattenElem(y) =>
//            stream.println(quote(l) + " ++= " + quote(getBlockResult(y)))
        }
      }
      stream.println("}")
      stream.println(s"/*end: ${rhs1.toString}*/")
    case _ => super.emitFatNode(sym, rhs1)
  }
}
