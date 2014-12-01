package scalan.compilation.lms

import scala.reflect.SourceContext
import virtualization.lms.common._
import virtualization.lms.epfl.test7._
//import virtualization.lms.epfl.test7.ArrayLoopsFatExp
//import virtualization.lms.epfl.test7.ScalaGenArrayLoopsFat

//{ScalaGenArrayLoopsFat, ArrayLoopsExp}
import scala.Tuple2

trait LmsBackendFacade extends ListOpsExp with NumericOpsExp with RangeOpsExp with PrimitiveOpsExp
with EqualExp with BooleanOpsExp with TupleOpsExp with ArrayLoopsFatExp with IfThenElseFatExp with CastingOpsExp {
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

  def ifThenElse[A:Manifest](cond: Exp[Boolean], iftrue: => Exp[A], iffalse: => Exp[A]) = {
    if (cond) iftrue else iffalse
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

trait LmsFunction[A,B] extends LmsBackendFacade { self =>
  def apply(x: Rep[A]): Rep[B]
  
  val codegen = new ScalaGenEffect with ScalaGenArrayOps with ScalaGenListOps with ScalaGenNumericOps
    with ScalaGenPrimitiveOps with ScalaGenEqual with ScalaGenBooleanOps with ScalaGenStruct
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
  }
}
