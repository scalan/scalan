package scalan
package compilation
package lms
package cxx
package sharedptr

import scala.virtualization.lms.common.{BaseGenArrayOps, ArrayOpsExp}
import scala.virtualization.lms.epfl.test7.ArrayLoopsExp
import scala.virtualization.lms.epfl.test8.ArrayMutationExp
import scala.virtualization.lms.internal.Expressions

trait CxxShptrGenArrayOps extends CxxShptrCodegen with BaseGenArrayOps {
  val IR: Expressions with ArrayOpsExp with ArrayLoopsExp with ArrayMutationExp
  import IR._

  headerFiles ++= Seq("vector", "algorithm")

  override def remap[A](m: Manifest[A]) : String = {
    m match {
      case _ if m.runtimeClass.isArray =>
        val mA = m.typeArguments(0)
        src"std::vector<${mA}>"
      case _ =>
        super.remap(m)
    }
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
//    case ArrayIndex(xs, i) =>
//      emitValDef(sym, src"(*$xs)[$i]")
    case ArrayApply(xs,i) =>
      emitValDef(sym, src"(*$xs)[$i]")
    case ArrayLength(x) =>
      emitValDef(sym, src"$x->size()")
    case ArrayUpdate(x,n,y) =>
      stream.println(src"(*$x)[$n] = $y; /*${rhs.toString }*/")
    //    case ArraySlice(x,s,e) =>
    //      val tp=remap(x.tp.typeArguments(0))
    //      emitValDef(sym, src"({ size_t sz=sizeof("+tp+")*($e-$s); "+tp+"* r = ("+tp+"*)malloc(sz); memcpy(r,(("+tp+"*)$x)+$s,sz); r; })")
    case a@ArrayNew(Const(0)) =>
      emitConstruct(sym)
    case a@ArrayNew(n) =>
      emitConstruct(sym, src"$n")
    case ArrayMutable(a) =>
      emitValDef(sym, src"$a /* clone */")
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
    case ArrayForeach(a,x,block) =>
      //      stream.println(s"")
      val len = s"${quote(sym)}_len"
      val i = s"${quote(sym)}_i"
      gen"""{/*start: ${sym} = ${rhs.toString}*/
           |size_t $len = $a->size();
           |for(size_t $i = 0; $i < $len; ++$i) {"""
      emitValDef( x, src"(*$a)[$i]" )
      emitBlock(block)
      gen"""}
           |}/*end: ${sym} = ${rhs.toString}*/"""
    //      stream.println(s"")
    case ArrayCopy(src,srcPos,dest,destPos,len) =>
      stream.println(s"{/*start: ${rhs.toString}*/")
      stream.println(src"const auto srcBegin = ${src}->begin();")
      stream.println(src"const auto destBegin = ${dest}->begin();")
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

trait CxxShptrGenArrayOpsBoost extends CxxShptrGenArrayOps {
  val IR : Expressions with ArrayOpsExp with ArrayLoopsExp with ArrayMutationExp
  import IR._

  headerFiles ++= Seq("boost/container/vector.hpp")

  override def remap[A](m: Manifest[A]) : String = {
    m match {
      case _ if m.runtimeClass.isArray =>
        val mA = m.typeArguments(0)
        src"boost::container::vector<${mA}>"
      case _ =>
        super.remap(m)
    }
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@ArrayNew(n) =>
      emitConstruct(sym, src"$n", "boost::container::default_init")
    case _ =>
      super.emitNode(sym, rhs)
  }
}
