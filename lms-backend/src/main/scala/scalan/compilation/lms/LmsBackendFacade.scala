package scalan.compilation.lms

import java.io.PrintWriter
import scala.reflect.SourceContext
import scalan.compilation.lms.common.{ScalaGenEitherOps, EitherOpsExp}
import scala.reflect.{RefinedManifest, SourceContext}
import scala.virtualization.lms.internal.{GraphTraversal, NestedBlockTraversal, GenericCodegen, Expressions}
import virtualization.lms.common._
import virtualization.lms.epfl.test7._

trait LmsBackendFacade extends ListOpsExp with NumericOpsExp with RangeOpsExp with PrimitiveOpsExp
with EqualExp with BooleanOpsExp with TupleOpsExp with ArrayLoopsFatExp  with OrderingOpsExp with IfThenElseFatExp with CastingOpsExp with JNILmsOpsExp {
  with EqualExp with BooleanOpsExp with TupleOpsExp with ArrayLoopsFatExp  with OrderingOpsExp with IfThenElseFatExp
  with CastingOpsExp with EitherOpsExp {
  /*type RepD[T] = Rep[T]
  */
  def arrayGet[A:Manifest](a: Exp[Array[A]], i: Exp[Int]):Exp[A] = {
    a.at(i)
  }
  def arrayGather[A:Manifest](a: Exp[Array[A]], idxs: Exp[Array[Int]]): Exp[Array[A]] = {
    array(idxs.length)(i => a.at(idxs.at(i)))
  }
  def arrayLength[A:Manifest](a: Exp[Array[A]]): Exp[Int] = {
    a.length
  }
  def tuple[A:Manifest,B:Manifest](a:Exp[A], b: Exp[B]): Exp[(A,B)] = {
    Tuple2(a,b)
  }

  def sumLeft[A: Manifest, B: Manifest](a: Exp[A]): Exp[Either[A, B]] = make_left[A, B](a)
  def sumRight[A: Manifest, B: Manifest](b: Exp[B]): Exp[Either[A, B]] = make_right[A, B](b)
  def tuple[A:Manifest, B:Manifest](a:Exp[A], b: Exp[B]): Exp[(A,B)] = (a, b)
  def first[A:Manifest, B:Manifest](tup: Exp[(A,B)]): Exp[A] = {
    tup._1
  }
  def second[A:Manifest, B:Manifest](tup: Exp[(A,B)]): Exp[B] = {
    tup._2
  }
  def opPlus[A:Numeric:Manifest](a: Exp[A], b: Exp[A]): Exp[A] = { a + b }
  def opMinus[A:Numeric:Manifest](a: Exp[A], b: Exp[A]): Exp[A] = { a - b }
  def opMult[A:Numeric:Manifest](a:Exp[A], b:Exp[A]): Exp[A] = { a*b }
  def opDiv[A:Numeric:Manifest](a:Exp[A], b:Exp[A]): Exp[A] = { a/b }
  def opMod(a: Exp[Int], b: Exp[Int]): Exp[Int] = a % b
  def opEq[A:Manifest](a:Exp[A], b:Exp[A]): Exp[Boolean] = { equals(a,b)}
  def opNeq[A:Manifest](a:Exp[A], b:Exp[A]): Exp[Boolean] = { notequals(a,b)}

  def LT[A:Manifest](left: Exp[A], right: Exp[A])(implicit ord:Ordering[A]) = {
    left < right
  }
  def LTEQ[A:Manifest](left: Exp[A], right: Exp[A])(implicit ord:Ordering[A]) = {
    left <= right
  }
  def block[A: Manifest, B: Manifest](left: Exp[A], right: Exp[B]) = {
    val l = left
    right
  }
  def GT[A:Manifest](left: Exp[A], right: Exp[A])(implicit ord:Ordering[A]) = {
    left > right
  }
  def GTEQ[A:Manifest](left: Exp[A], right: Exp[A])(implicit ord:Ordering[A]) = {
    left >= right
  }
  def Max[A:Manifest](left: Exp[A], right: Exp[A])(implicit ord:Ordering[A]) = {
    left.max(right)
  }
  def Min[A:Manifest](left: Exp[A], right: Exp[A])(implicit ord:Ordering[A]) = {
    left.min(right)
  }

  def mapArray[A:Manifest, B:Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[B]) : Exp[Array[B]] = {
    array(a.length)(i => f(a.at(i)))
  }

  def filterArray[A:Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[Boolean]) : Exp[Array[A]] = {
    arrayIf(a.length) { i => (f(a.at(i)), a.at(i)) }
  }
  def replicate[A:Manifest](length: Exp[Int], v: Exp[A]) : Exp[Array[A]]= {
    array(length)(i => v)
  }
  def indexRangeD(length: Exp[Int]) : Exp[Array[Int]] = {
    array(length)(i => i)
  }
  /* Reduce only for summation! */
  def reduce[A:Manifest] (a: Exp[Array[A]]) : Exp[A] = {
    sum(a.length){i => a.at(i).AsInstanceOf[Double]}.AsInstanceOf[A]
  }

  def newArray[A:Manifest](length: Rep[Int]): Rep[Array[A]] = NewArray[A](length)

  def opZipWith[A:Manifest, B:Manifest, R:Manifest]( f:(Rep[A], Rep[B]) => Rep[R], a:Exp[Array[A]], b:Exp[Array[B]]) : Exp[Array[R]] = {
    array(a.length)(i => f(a.at(i), b.at(i)) )
  }

  def opZip[A:Manifest, B:Manifest]( a:Exp[Array[A]], b:Exp[Array[B]]) : Exp[Array[(A,B)]] = {
    array[(A,B)](a.length)(i => (a.at(i),b.at(i)) )
  }
  def opDotProductSV[A:Manifest](i1: Exp[Array[Int]], v1: Exp[Array[A]], i2: Exp[Array[Int]], v2: Exp[Array[A]]) : Exp[A] = {
    array_dotProductSparse(i1,v1, i2, v2)
  }
  
  def strideArray[A: Manifest](xs: Exp[Array[A]], start: Exp[Int], length: Exp[Int], stride: Exp[Int]) =
    array(length) { i =>
      xs.at(start + i * stride)
    }

  def ifThenElse[A:Manifest](cond: Exp[Boolean], iftrue: () => Exp[A], iffalse: () => Exp[A]) = {
    if (cond) iftrue() else iffalse()
  }

  //def printlnD(s: Exp[Any])  = println(s)
  def unitD[T:Manifest](x: T) = unit[T](x)
  /*
  def mkStringD[A:Manifest](a: Exp[DeliteArray[A]]) : Exp[String] = {
    a mkString unitD(" ")
  }  */

  def array_dotProductSparse[A:Manifest](idxs1: Rep[Array[Int]], vals1: Rep[Array[A]], idxs2: Rep[Array[Int]], vals2: Rep[Array[A]]) = {
    ArrayDotProdSparse(idxs1, vals1, idxs2, vals2)
  }

  case class ArrayDotProdSparse[A:Manifest](idxs1: Exp[Array[Int]], vals1: Exp[Array[A]], idxs2: Exp[Array[Int]], vals2: Exp[Array[A]]) extends Def[A] {
    val m = manifest[A]
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case ArrayDotProdSparse(i1, v1,i2, v2) => array_dotProductSparse(f(i1), f(v1), f(i2), f(v2))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??

}

class LmsBackend extends LmsBackendFacade { self =>

  val codegen = new ScalaGenEffect with ScalaGenArrayOps with ScalaGenListOps with ScalaGenNumericOps
    with ScalaGenPrimitiveOps with ScalaGenEqual with ScalaGenEitherOps with ScalaGenOrderingOps with ScalaGenBooleanOps with ScalaGenStruct
    with ScalaGenTupleOps with ScalaGenFatArrayLoopsFusionOpt with ScalaGenIfThenElseFat with LoopFusionOpt
    with ScalaGenCastingOps {
      val IR: self.type = self
      override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true

      private def isTuple(name: String) =
        name.startsWith("Tuple2") || name.startsWith("Tuple3") || name.startsWith("Tuple4") || name.startsWith("Tuple5")

      override def remap[A](m: Manifest[A]) = if (isTuple(m.runtimeClass.getSimpleName)) {
        m.toString
      } else {
        super.remap(m)
      }

      override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
        case Struct(ClassTag(name), elems) if isTuple(name) =>
          emitValDef(sym, "(" + elems.map(e => quote(e._2)).mkString(",") + ")")
        case ds @ ArrayDotProdSparse(idxs1, vals1, idxs2, vals2) =>
          // TODO use proper source quasiquoter
          stream.println("// generating dot product")
          stream.println("val " + quote(sym) + " ={")
          stream.println("\tval idxs1 = " + quote(idxs1))
          stream.println("\tval idxs2 = " + quote(idxs2))
          stream.println("\tval vals1 = " + quote(vals1))
          stream.println("\tval vals2 = " + quote(vals2))
          stream.println("\tvar i1 = 0")
          stream.println("\tvar i2 = 0")
          stream.println("\tvar out:" + remap(ds.m) + " = 0")
          stream.println("\twhile (i1 < idxs1.length && i2 < idxs2.length) {")
          stream.println("\t\tval ind1 = idxs1(i1)")
          stream.println("\t\tval ind2 = idxs2(i2)")
          stream.println("\t\tif (ind1 == ind2) { ")
          stream.println("\t\t\tout += vals1(i1) * vals2(i2)")
          stream.println("\t\t\ti1+=1")
          stream.println("\t\t\ti2+=1")
          stream.println("\t\t} else if (ind1 < ind2 ) {")
          stream.println("\t\t\ti1+=1")
          stream.println("\t\t} else {")
          stream.println("\t\t\ti2+=1")
          stream.println("\t\t}")
          stream.println("\t}")
          stream.println("\tout")
          stream.println("}")
        case _ => super.emitNode(sym, rhs)
      }
  } // codegen



  val codegenCXX = new CLikeGenNumericOps
    with CLikeGenEqual
    with CLikeGenArrayOps
    with CLikeGenPrimitiveOps
    with CXXGenStruct
    with CXXGenJNIExtractor
    with CXXGenFatArrayLoopsFusionOpt
    with LoopFusionOpt
    with CXXFatCodegen
    with CXXCodegen
//    with ExportLMSGraph
  {

    override val IR: self.type = self

    override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true

    override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
      if( moveableSyms.contains(sym) )
        emitValDef(quote(sym), norefManifest(sym.tp), rhs)
      else
        emitValDef(quote(sym), sym.tp, rhs)
    }

    override def quote(x: Exp[Any]) = x match {
      case sym: Sym[_] =>
        if( moveableSyms.contains(sym) && rightSyms.contains(sym) )
          s"std::move(${super.quote(sym)})"
        else super.quote(sym)
      case _ =>
        super.quote(x)
    }

    override def emitValDef(sym: String, tpe: Manifest[_], rhs: String): Unit = {
      if (remap(tpe) != "void")
        stream.println(remapWithRef(tpe) + " " + sym + " = " + rhs + ";")
    }

    override def remapWithRef[A](m: Manifest[A]): String = {
      m.runtimeClass match {
        case c if c == classOf[Noref[_]] =>
          remap(m.typeArguments(0))
        case c if c == classOf[size_t] =>
          remap(m)
        case _ =>
          super.remapWithRef(m)
      }
    }
    override def addRef() = "&"

    def remapResult[A](m: Manifest[A]): String = {
      m.runtimeClass match {
        case c if c.isArray =>
          val itemType = m.typeArguments(0)
          val ts = remap  (itemType)
          s"std::vector<${ts}>"
        case _ =>
          remap(m)
      }
    }

    override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = {
      rhs match {
        case RepAsInstanceOf(sy, t1, t2) if t1 != t2 =>
          emitValDef(sym, s"static_cast<${remap(t2)}>(${quote(sy)})")
        case RepAsInstanceOf(sy, _, _) =>
          emitValDef(sym, quote(sy))
        case _ => super.emitNode(sym, rhs)
      }
    }

    override def emitFatNode(sym: List[Sym[Any]], rhs: FatDef) = {
      super.emitFatNode(sym, rhs)
    }

    override def emitSource[A: Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {
      val sA = remapResult(manifest[A])

      //      val staticData = getFreeDataBlock(body)

      withStream(out) {
        stream.println(
          "#include <vector>\n" +
          "#include <tuple>\n" +
          "#include <cstdlib>\n" +
          "#include <jni-array-wrapper.hpp>\n" +
          "/*****************************************\n" +
          "  Emitting Generated Code                  \n" +
          "*******************************************/")
        emitFileHeader()

        val indargs = (0 until args.length) zip args;
//        val InputTypes = indargs.map( p => s"InputType${p._1.toString}" )
//        stream.println( s"template<${InputTypes.map( t => "class " + t).mkString(", ")}>" )
//        stream.println(s"${sA} apply(${indargs.map( p => s"InputType${p._1.toString}& ${quote(p._2)}").mkString(", ")} ) {")
//        for( (t, (i, arg)) <- InputTypes zip indargs ) {
//          stream.println(s"// ${t}: ${remap(arg.tp)}")
//        }
        val has = indargs.map(p => p._2.tp.runtimeClass)
                         .contains( classOf[JNIType[_]] )
        val jniEnv = if(has) "JNIEnv* env, " else ""
        stream.println(s"${sA} apply(${jniEnv}${indargs.map( p => s"${remap(p._2.tp)} ${quote(p._2)}").mkString(", ")} ) {")

        emitBlock(body)
        stream.println(s"return ${quote(getBlockResult(body))};")

        stream.println("}")
        stream.println("/*****************************************\n" +
                       "  End of Generated Code                  \n" +
                       "*******************************************/")
      }

      Nil
    }
  } // codegenCXX
}
