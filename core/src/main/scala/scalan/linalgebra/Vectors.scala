package scalan.linalgebra

import scalan._
import scalan.common.Default
import scalan.arrays.PArrays

trait Vectors extends VectorsOps with PArrays { scalan: VectorsDsl =>
  type Vec[T] = Rep[Vector[T]]
  
  trait Vector[T] extends UserType[Vector[T]] with VectorOps[T] {}
  
  abstract class DenseVector[T](val coords: Rep[PArray[T]])(implicit val elem: Elem[T]) extends Vector[T] with DenseVectorOps[T]
  
  abstract class SparseVector[T](val nonZeroIndices: Rep[Array[Int]], val nonZeroValues: Rep[PArray[T]], val length: Rep[Int])(implicit val elem: Elem[T]) extends Vector[T] with SparseVectorOps[T]
}