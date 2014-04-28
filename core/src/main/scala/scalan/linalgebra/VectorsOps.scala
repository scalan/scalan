package scalan.linalgebra

import scalan.ScalanDsl
import scalan.ScalanSeqImplementation
import scalan.ScalanStaged
import scalan.arrays.PArraysDsl
import scalan.common.Default

trait VectorsOps { scalan: VectorsDsl =>
  trait VectorOps[T] {
    def length: Rep[Int]
    implicit def elem: Elem[T]
  }
  
  trait VectorCompanionOps extends TypeFamily1[Vector] {
    def defaultOf[T: Elem] = DenseVector.defaultOf[T]
  }
  
  trait DenseVectorOps[T] extends VectorOps[T] {
    def coords: PA[T]
    def length = coords.length
  }
  
  trait DenseVectorCompanionOps extends ConcreteClass1[DenseVector] {
    def defaultOf[T: Elem] = Default.defaultVal(DenseVector(element[PArray[T]].defaultRepValue))
  }
  
  trait SparseVectorOps[T] extends VectorOps[T] {
    def sparseCoords: PA[(Int, T)]
  }
  
  trait SparseVectorCompanionOps extends ConcreteClass1[SparseVector] {
    def defaultOf[T: Elem] = Default.defaultVal(SparseVector(element[PArray[(Int, T)]].defaultRepValue, intElement.defaultRepValue))
  }
}

trait VectorsDsl extends ScalanDsl with VectorsAbs with VectorsOps with PArraysDsl { }

trait VectorsDslSeq extends VectorsDsl with VectorsSeq with ScalanSeqImplementation

trait VectorsDslExp extends VectorsDsl with VectorsExp with ScalanStaged