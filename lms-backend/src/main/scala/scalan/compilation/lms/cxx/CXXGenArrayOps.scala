package scalan.compilation.lms.cxx

import scala.virtualization.lms.common.{ArrayOpsExp, BaseGenArrayOps}

trait CXXGenArrayOps extends BaseGenArrayOps with CXXCodegen {
  val IR: ArrayOpsExp
  import IR._

  val ARRAY_LITERAL_MAX_SIZE = 1000

  override def traverseStm(stm: Stm): Unit = {
    stm match {
      case TP(sym, rhs) =>
        rhs match {
          case ArrayNew(_) =>
            moveableSyms += sym
          case Reflect(ArrayNew(_),_,_) =>
            moveableSyms += sym
          case _ =>
            ()
        }
      case _ =>
        ()
    }

    super.traverseStm(stm)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayLength(x) => emitValDef(sym, src"$x.size()")
    case ArrayApply(x,n) => emitValDef(sym, src"$x[$n]")
    case ArrayUpdate(x,n,y) => stream.println(src"$x[$n] = $y; /*${rhs.toString }*/")
//    case ArraySlice(x,s,e) =>
//      val tp=remap(x.tp.typeArguments(0))
//      emitValDef(sym, src"({ size_t sz=sizeof("+tp+")*($e-$s); "+tp+"* r = ("+tp+"*)malloc(sz); memcpy(r,(("+tp+"*)$x)+$s,sz); r; })")
    case a@ArrayNew(Const(0)) =>
      stream.println(s"${remap(norefManifest(sym.tp))} ${quote(sym)}; /*${a}*/");
    case a@ArrayNew(n) =>
      stream.println(s"${remap(norefManifest(sym.tp))} ${quote(sym)}(${quote(n)}); /*${a}*/");
//    case e@ArrayFromSeq(xs) => {
//      emitData(sym, xs)
//      emitValDef(sym,
//        if(xs.size > ARRAY_LITERAL_MAX_SIZE) {
//          /* def append(i: Int) = {
//            val start = i*ARRAY_LITERAL_MAX_SIZE
//            val end = Math.min((i+1)*ARRAY_LITERAL_MAX_SIZE, xs.size)
//            val size = end - start
//            "def x" + sym.id + "_" + i + "=Array(" + (start until end).map{xs(_)} + ")\nArray.copy(x" + sym.id + "_" + i + ",0,buf," + start + "," + size + ")\n"
//          }
//          val numBlocks = Math.ceil(xs.size / ARRAY_LITERAL_MAX_SIZE).intValue
//          "{val buf=new Array[" + remap(e.mt) + "](" + xs.size + ")\n" + ((0 until numBlocks).map(append)).mkString("\n") + "buf}" */
//          "{import scala.io.Source;(Source.fromFile(\"" + symDataPath(sym) + "\").getLines.map{Integer.parseInt(_)}).toArray}"
//        }
//        else {
//          "Array(" + (xs map quote).mkString(",") + ")"
//        }
//      )
//    }
    case afrch@ArrayForeach(a,x,block) =>
//      stream.println(s"")
      gen"""{/*start: ${sym} = ${afrch.toString}*/
           |size_t len = ${a}.size();
           |for(size_t i = 0; i < len; ++i) {"""
            emitValDef( x, s"${quote(a)}[i]" )
            emitBlock(block)
      gen"""}
           |}/*end: ${sym} = ${afrch.toString}*/"""
//      stream.println(s"")
    case ArrayCopy(src,srcPos,dest,destPos,len) =>
      stream.println(s"{/*start: ${rhs.toString}*/")
      stream.println(src"const auto srcBegin = ${src}.begin();")
      stream.println(src"const auto destBegin = ${dest}.begin();")
      stream.println(src"std::copy(srcBegin+${srcPos}, srcBegin+${srcPos}+${len}, destBegin+${destPos});")
      emitValDef(sym, src"$dest")
      stream.println(s"}/*end: ${rhs.toString}*/")
//    case a@ArraySort(x) =>
//      gen"""val $sym = {
//                      |val d = new Array[${remap(a.m)}]($x.length)
//                                                            |System.arraycopy($x, 0, d, 0, $x.length)
//                                                                                               |scala.util.Sorting.quickSort(d)
//                                                                                               |d
//                                                                                               |}"""
//    case n@ArrayMap(a,x,blk) =>
//      gen"""// workaround for refinedManifest problem
//           |val $sym = {
//                      |val out = ${n.array}
//          |val in = $a
//          |var i = 0
//          |while (i < in.length) {
//          |val $x = in(i)
//                   |${nestedBlock(blk)}
//          |out(i) = $blk
//          |i += 1
//          |}
//          |out
//          |}"""
//    case ArrayToSeq(a) => emitValDef(sym, src"$a.toSeq")
    case _ => super.emitNode(sym, rhs)
  }
}