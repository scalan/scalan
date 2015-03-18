package scalan.compilation.lms.cxx.sharedptr

import scala.virtualization.lms.common.{BaseGenListOps, CLikeGenEffect, ListOpsExp}
import scalan.compilation.lms.common.LstOpsExp

trait CxxShptrGenListOps extends CxxShptrCodegen with BaseGenListOps with CLikeGenEffect {
  val IR: ListOpsExp with LstOpsExp
  import IR._

  override def remap[A](m: Manifest[A]) : String = {
    m match {
      case _ if m.runtimeClass == classOf[List[_]] =>
        s"scalan::immutable_list<${remap(m.typeArguments(0))}>"
      case _ =>
        super.remap(m)
    }
  }

  override def quote(x: Exp[Any]) : String = x match {
    case c@Const(l: List[_]) =>
      l match {
        case Nil =>
          s"std::make_shared<${remap(c.tp)}>()"
        case _ =>
          s"std::make_shared<${remap(c.tp)}>(std::initializer_list<${remap(toShptrManifest(c.tp.typeArguments(0)))}>${l.mkString("{",",","}")})"
      }
    case _ =>
      super.quote(x)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ListNew(xs) =>
      emitConstruct(sym, src"std::initializer_list<${remap(sym.tp.typeArguments(0))}>{${(xs map {quote}).mkString(",")}}")
    case ListConcat(xs,ys) =>
      emitConstruct(sym, src"*$xs", src"*$ys")
    case ListCons(x, xs) =>
      emitConstruct(sym, src"$x", src"*$xs")
    case ListPrepend(l,e) =>
      emitConstruct(sym, src"$e", src"*$l")
    case ListHead(xs) =>
      emitValDef(sym, src"$xs->front()")
    case ListTail(xs) =>
      emitValDef(sym, src"$xs->pop_front()")
    case ListIsEmpty(xs) =>
      emitValDef(sym, src"$xs->empty()")
//    case ListFromSeq(xs) =>
//      emitValDef(sym, src"List($xs: _*)")
//    case ListMkString(xs) => emitValDef(sym, src"$xs.mkString")
//    case ListMkString2(xs,s) => emitValDef(sym, src"$xs.mkString($s)")
    case ListMap(l,x,blk) =>
      val blkres = getBlockResult(blk);
      gen"/*start: ${rhs.toString} */"
      emitConstruct(sym)
      gen"if( !$l->empty() ) {"
      val v = fresh(sym.tp.typeArguments(0))
      gen"{"
      emitValDef(x, src"$l->front()")
      emitBlock(blk)
      val q = fresh(sym.tp)
      emitConstruct(q, src"$sym->push_front($blkres)")
      gen"$sym = $q;"
      gen"}"
      gen"{"
      val cur = fresh(sym.tp)
      val ll = fresh(sym.tp)
      emitValDef(cur, src"$sym")
      gen"for(auto it = ++($l->begin()); it != $l->end(); ++it) {"
      emitValDef(x, "*it");
      emitBlock(blk)
      emitValDef(ll, src"$cur")
      val q1 = fresh(cur.tp)
      emitConstruct(q1, src"std::initializer_list<${remap(blkres.tp)}>{$blkres}")
      gen"""$cur = $q1;
           |$ll->set_tail(*$cur);
           |$sym->set_len($sym->length() + 1);
           |}"""
      gen"}"
      gen"}"//if( !$l->empty() )
      gen"/*end: ${rhs.toString} */"

//    case ListFlatMap(l, x, b) =>
//      gen"""val $sym = $l.flatMap { $x =>
//                                        |${nestedBlock(b)}
//          |$b
//          |}"""
//    case ListFilter(l, x, b) =>
//      gen"""val $sym = $l.filter { $x =>
//                                       |${nestedBlock(b)}
//          |$b
//          |}"""
//    case ListSortBy(l,x,blk) =>
//      gen"""val $sym = $l.sortBy { $x =>
//                                       |${nestedBlock(blk)}
//          |$blk
//          |}"""
    case ListToArray(l) =>
      emitConstruct(sym, src"$l->cbegin()", src"$l->cend()")

//    case ListToSeq(l) => emitValDef(sym, src"$l.toSeq")

    case _ =>
      super.emitNode(sym, rhs)
  }
}
