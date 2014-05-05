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
    def dot(other: Vec[T])(implicit n: Numeric[T], m: RepMonoid[T]): Rep[T]
  }
  
  trait VectorCompanionOps extends TypeFamily1[Vector] {
    def defaultOf[T: Elem] = DenseVector.defaultOf[T]
  }
  
  trait DenseVectorOps[T] extends VectorOps[T] {
    def coords: PA[T]
    def length = coords.length
    def dot(other: Vec[T])(implicit n: Numeric[T], m: RepMonoid[T]) = matchVec[T, T](other) {
      dv => dotPA(coords, dv.coords)(n, m, elem)
    } {
      sv =>
        dotPA(coords(sv.nonZeroIndices), sv.nonZeroValues)(n, m, elem)
    }
  }

  def dotPA[T](xs: PA[T], ys: PA[T])(implicit n: Numeric[T], m: RepMonoid[T], e: Elem[T]) = {
    xs.zip(ys).map {
      case Pair(x, y) =>
        x * y
    }.reduce
  }
  
  trait DenseVectorCompanionOps extends ConcreteClass1[DenseVector] {
    def defaultOf[T: Elem] = Default.defaultVal(DenseVector(element[PArray[T]].defaultRepValue))
  }
  
  trait SparseVectorOps[T] extends VectorOps[T] {
    def nonZeroIndices: Arr[Int]
    def nonZeroValues: PA[T]
    def dot(other: Vec[T])(implicit n: Numeric[T], m: RepMonoid[T]) = matchVec[T, T](other) {
      dv => dv.dot(this.asInstanceOf[SparseVector[T]])
    } {
      sv => dotSparse(this.asInstanceOf[SparseVector[T]], sv)
    }
  }

  trait SparseVectorCompanionOps extends ConcreteClass1[SparseVector] {
    def defaultOf[T: Elem] = Default.defaultVal(SparseVector(element[Array[Int]].defaultRepValue, element[PArray[T]].defaultRepValue, intElement.defaultRepValue))
  }

  def dotSparse[T](xs: Rep[SparseVector[T]], ys: Rep[SparseVector[T]])(implicit n: Numeric[T], m: RepMonoid[T]): Rep[T] =
    !!!("Must implement dotSparse")
}

trait VectorsDsl extends ScalanDsl with VectorsAbs with VectorsOps with PArraysDsl {
  def matchVec[T, R](vec: Vec[T])(dense: Rep[DenseVector[T]] => Rep[R])(sparse: Rep[SparseVector[T]] => Rep[R]): Rep[R]
}

trait VectorsDslSeq extends VectorsDsl with VectorsSeq with ScalanSeqImplementation {
  def matchVec[T, R](vec: Vec[T])(dense: Rep[DenseVector[T]] => Rep[R])(sparse: Rep[SparseVector[T]] => Rep[R]): Rep[R] =
    vec match {
      case dv: DenseVector[_] => dense(dv)
      case sv: SparseVector[_] => sparse(sv)
    }

//  override def dotSparse[T](xs: Rep[SparseVector[T]], ys: Rep[SparseVector[T]])(implicit n: Numeric[T], m: RepMonoid[T]): Rep[T] =
}

trait VectorsDslExp extends VectorsDsl with VectorsExp with ScalanStaged {
  def matchVec[T, R](vec: Vec[T])(dense: Rep[DenseVector[T]] => Rep[R])(sparse: Rep[SparseVector[T]] => Rep[R]): Rep[R] =
    vec.elem.asInstanceOf[Elem[_]] match {
      case _: DenseVectorElem[_] => dense(vec.asRep[DenseVector[T]])
      case _: SparseVectorElem[_] => sparse(vec.asRep[SparseVector[T]])
    }
}