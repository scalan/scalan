package scalan.compilation.lms

import scala.virtualization.lms.internal.GenericCodegen
import scalan.compilation.lms.common.{VectorOpsExp, ScalaGenVectorOps, ScalaGenEitherOps, EitherOpsExp}
import virtualization.lms.common._
import virtualization.lms.epfl.test7._

trait LmsBackend extends BaseExp { self =>

  type Codegen <: GenericCodegen {
    val IR: self.type
  }

  def codegen: Codegen
}

trait LmsBackendFacade extends ListOpsExp with NumericOpsExp with RangeOpsExp with PrimitiveOpsExp
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
}

class CoreLmsBackend extends LmsBackend with LmsBackendFacade { self =>

  trait Codegen extends ScalaGenFunctions with ScalaGenStruct with ScalaGenArrayOps with ScalaGenListOps with ScalaGenNumericOps
    with ScalaGenPrimitiveOps with ScalaGenEqual with ScalaGenEitherOps with ScalaGenOrderingOps with ScalaGenBooleanOps
    with ScalaGenTupleOps with ScalaGenFatArrayLoopsFusionOpt with ScalaGenIfThenElseFat with LoopFusionOpt
    with ScalaGenCastingOps {

    val IR: self.type = self
    override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true

    private def isTuple(name: String) =
      name.startsWith("Tuple2") || name.startsWith("Tuple3") || name.startsWith("Tuple4") || name.startsWith("Tuple5")

    override def remap[A](m: Manifest[A]) =
      if (isTuple(m.runtimeClass.getSimpleName)) m.toString
      else super.remap(m)

    override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
      case Struct(ClassTag(name), elems) if isTuple(name) =>
        emitValDef(sym, "(" + elems.map(e => quote(e._2)).mkString(",") + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }

  val codegen = new Codegen {}
}

class CommunityLmsBackend extends CoreLmsBackend with VectorOpsExp { self =>

  override val codegen = new Codegen with ScalaGenVectorOps {
    override val IR: self.type = self
  }
}
