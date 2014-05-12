package scalan.codegen.lms
//package util

import virtualization.lms.common._
import virtualization.lms.epfl.test7._
//import virtualization.lms.epfl.test7.ArrayLoopsFatExp
//import virtualization.lms.epfl.test7.ScalaGenArrayLoopsFat

//{ScalaGenArrayLoopsFat, ArrayLoopsExp}
import scala.Tuple2

trait LMSBackendFacade extends ArrayOpsExp with ListOpsExp with NumericOpsExp with RangeOpsExp with PrimitiveOpsExp
with EqualExp with BooleanOpsExp with TupleOpsExp with ArrayLoopsFatExp with IfThenElseFatExp {
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
  def opEq[A:Manifest](a:Exp[A], b:Exp[A]): Exp[Boolean] = { equals(a,b)}
  def opNeq[A:Manifest](a:Exp[A], b:Exp[A]): Exp[Boolean] = { notequals(a,b)}

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
    sum(a.length){i => a.at(i)}
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
  //def printlnD(s: Exp[Any])  = println(s)
  def unitD[T:Manifest](x: T) = unit[T](x)
  /*
  def mkStringD[A:Manifest](a: Exp[DeliteArray[A]]) : Exp[String] = {
    a mkString unitD(" ")
  }  */
}

trait LMSFunction[A,B] extends LMSBackendFacade { self =>
  def test(x: Rep[A]): Rep[B]
  val codegen = new ScalaGenEffect with ScalaGenArrayOps with ScalaGenListOps with ScalaGenNumericOps
    with ScalaGenPrimitiveOps with ScalaGenEqual with ScalaGenBooleanOps with ScalaGenStruct
    with ScalaGenFatArrayLoopsFusionOpt with ScalaGenIfThenElseFat with LoopFusionOpt { 
      val IR: self.type = self
      override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
  }
}
