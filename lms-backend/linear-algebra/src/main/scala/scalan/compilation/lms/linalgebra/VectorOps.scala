package scalan.compilation.lms.linalgebra

import scala.lms.common._
import scala.lms.internal.Transforming
import scala.reflect.SourceContext
import scalan.compilation.lms.LmsBackendFacade
import scalan.compilation.lms.cxx.sharedptr.CxxShptrCodegen

trait VectorOps extends Variables {

  def array_dotProductSparse[A: Numeric: Manifest](idxs1: Rep[Array[Int]], vals1: Rep[Array[A]], idxs2: Rep[Array[Int]], vals2: Rep[Array[A]]): Rep[A]
}

trait VectorOpsExp extends VectorOps with EffectExp with VariablesExp with Transforming { self: LmsBackendFacade =>

  case class ArrayDotProdSparse[A](idxs1: Exp[Array[Int]], vals1: Exp[Array[A]], idxs2: Exp[Array[Int]], vals2: Exp[Array[A]])
                                  (implicit val num: Numeric[A], val m: Manifest[A]) extends Def[A]

  def array_dotProductSparse[A: Numeric: Manifest](idxs1: Rep[Array[Int]], vals1: Rep[Array[A]], idxs2: Rep[Array[Int]], vals2: Rep[Array[A]]) =
    ArrayDotProdSparse(idxs1, vals1, idxs2, vals2)

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case dp @ ArrayDotProdSparse(i1, v1,i2, v2) => array_dotProductSparse(f(i1), f(v1), f(i2), f(v2))(dp.num, mtype(dp.m))
    case _ => super.mirror(e,f)
  }
}

trait ScalaGenVectorOps extends ScalaGenBase {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ds @ ArrayDotProdSparse(idxs1, vals1, idxs2, vals2) =>
      // existence of implicit Numeric[A] ensures we can have + and *
      stream.println(
        src"""
           |// generating dot product
           |val $sym = {
           |  val idxs1 = $idxs1
           |  val idxs2 = $idxs2
           |  val vals1 = $vals1
           |  val vals2 = $vals2
           |  var i1 = 0
           |  var i2 = 0
           |  var out: ${ds.m} = 0
           |  while (i1 < idxs1.length && i2 < idxs2.length) {
           |    val ind1 = idxs1(i1)
           |    val ind2 = idxs2(i2)
           |    if (ind1 == ind2) {
           |      out += vals1(i1) * vals2(i2)
           |      i1 += 1
           |      i2 += 1
           |    } else if (ind1 < ind2) {
           |      i1 += 1
           |    } else {
           |      i2 += 1
           |    }
           |  }
           |  out
           |}
           """.stripMargin)
    case _ => super.emitNode(sym, rhs)
  }

}

trait CxxShptrGenVectorOps extends CxxShptrCodegen {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ds @ ArrayDotProdSparse(idxs1, vals1, idxs2, vals2) =>
      // += and * operators are assumed to be defined, since A is a numeric type
      stream.println("// generating dot product")
      emitConstruct(sym)
      stream.println(
        src""" {
           |  auto idxs1 = $idxs1;
           |  auto idxs2 = $idxs2;
           |  auto vals1 = $vals1;
           |  auto vals2 = $vals2;
           |  size_t i1 = 0;
           |  size_t i2 = 0;
           |  while (i1 < idxs1->size() && i2 < idxs2->size()) {
           |    auto ind1 = (*idxs1)[i1];
           |    auto ind2 = (*idxs2)[i2];
           |    if (ind1 == ind2) {
           |      $sym += (*vals1)[i1] * (*vals2)[i2];
           |      i1 += 1;
           |      i2 += 1;
           |    } else if (ind1 < ind2) {
           |      i1 += 1;
           |    } else {
           |      i2 += 1;
           |    }
           |  };
           |}
           """.stripMargin)
    case _ => super.emitNode(sym, rhs)
  }

}
