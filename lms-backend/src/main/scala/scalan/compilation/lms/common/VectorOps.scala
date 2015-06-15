package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scalan.compilation.lms.cxx.sharedptr.CxxShptrCodegen

trait VectorOps extends Base {

  def array_dotProductSparse[A:Manifest](idxs1: Rep[Array[Int]], vals1: Rep[Array[A]], idxs2: Rep[Array[Int]], vals2: Rep[Array[A]]): Rep[A]
  def array_binarySearch[A:Manifest](idx: Rep[Int], idxs: Rep[Array[Int]]): Rep[Int]
  def array_randomGaussian[A:Manifest](a: Rep[Double], e: Rep[Double], arr: Rep[Array[Double]]): Rep[Array[Double]]
}


trait VectorOpsExp extends VectorOps with BaseExp {

  case class ArrayDotProdSparse[A:Manifest](idxs1: Exp[Array[Int]], vals1: Exp[Array[A]], idxs2: Exp[Array[Int]], vals2: Exp[Array[A]]) extends Def[A] {
    val m = manifest[A]
  }

  case class ArrayBinarySearch[A:Manifest](idx: Exp[Int], idxs: Exp[Array[Int]]) extends Def[Int] {
    val m = manifest[Int]
  }

  case class ArrayRandomGaussian[A:Manifest](a: Exp[Double], e: Exp[Double], xs: Exp[Array[Double]]) extends Def[Array[Double]] {
    val m = manifest[Double]
  }

  def array_dotProductSparse[A:Manifest](idxs1: Rep[Array[Int]], vals1: Rep[Array[A]], idxs2: Rep[Array[Int]], vals2: Rep[Array[A]]) =
    ArrayDotProdSparse(idxs1, vals1, idxs2, vals2)

  def array_binarySearch[A:Manifest](idx: Rep[Int], idxs: Rep[Array[Int]]): Rep[Int] =
    ArrayBinarySearch(idx, idxs)

  def array_randomGaussian[A:Manifest](a: Rep[Double], e: Rep[Double], arr: Rep[Array[Double]]): Rep[Array[Double]] =
    ArrayRandomGaussian(a, e, arr)

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case ArrayDotProdSparse(i1, v1,i2, v2) => array_dotProductSparse(f(i1), f(v1), f(i2), f(v2))(mtype(manifest[A]))
    case ArrayBinarySearch(i, is) => array_binarySearch(f(i), f(is))(mtype(manifest[Int])).asInstanceOf[Exp[A]] // TODO: is this hack valid?
    case ArrayRandomGaussian(a, d, arr) => array_randomGaussian(f(a), f(d), f(arr))(mtype(manifest[Double])).asInstanceOf[Exp[A]] // TODO: is this hack valid?
    case _ => super.mirror(e,f)
  }
}

trait ScalaGenVectorOps extends ScalaGenBase {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
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
    case ds @ ArrayBinarySearch(i, is) =>
      // TODO use proper source quasiquoter
      stream.println("// generating Binary Search in array")
      stream.println("val " + quote(sym) + " = {")
      stream.println("\tval out:" + remap(ds.m) + " = java.util.Arrays.binarySearch(" + quote(is) + ", " + quote(i) + ")")
      stream.println("\tout")
      stream.println("}")
    case ds @ ArrayRandomGaussian(a, d, arr) =>
      // TODO use proper source quasiquoter
      stream.println("// generating Random Gaussian values in array")
      stream.println("val " + quote(sym) + " = " + quote(arr) + ".map { _ => util.Random.nextGaussian() * " +
        quote(d) + ".asInstanceOf[Double] + " + quote(a) + ".asInstanceOf[Double] }")
    case _ => super.emitNode(sym, rhs)
  }

}

trait CxxShptrGenVectorOps extends CxxShptrCodegen {
  val IR: VectorOpsExp
  import IR._

  headerFiles ++= Seq("scalan/algorithm.hpp")

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ds @ ArrayDotProdSparse(idxs1, vals1, idxs2, vals2) =>
      // TODO use proper source quasiquoter
      stream.println("// generating dot product")
      emitConstruct(sym)
      stream.println("{")
      stream.println(s"\tauto idxs1 = ${quote(idxs1)};")
      stream.println(s"\tauto idxs2 = ${quote(idxs2)};")
      stream.println(s"\tauto vals1 = ${quote(vals1)};")
      stream.println(s"\tauto vals2 = ${quote(vals2)};")
      stream.println("\tsize_t i1 = 0;")
      stream.println("\tsize_t i2 = 0;")
      stream.println("\twhile (i1 < idxs1->size() && i2 < idxs2->size()) {")
      stream.println("\t\tauto ind1 = (*idxs1)[i1];")
      stream.println("\t\tauto ind2 = (*idxs2)[i2];")
      stream.println("\t\tif (ind1 == ind2) { ")
      stream.println(s"\t\t\t${quote(sym)} += (*vals1)[i1] * (*vals2)[i2];")
      stream.println("\t\t\ti1+=1;")
      stream.println("\t\t\ti2+=1;")
      stream.println("\t\t} else if (ind1 < ind2 ) {")
      stream.println("\t\t\ti1+=1;")
      stream.println("\t\t} else {")
      stream.println("\t\t\ti2+=1;")
      stream.println("\t\t}")
      stream.println("\t};")
      stream.println("}")
    case ds @ ArrayBinarySearch(i, is) =>
      emitValDef(sym, src"scalan::binary_search($is->begin(), $is->end(), $i);")
    case _ => super.emitNode(sym, rhs)
  }

}
