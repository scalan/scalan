package tests.scalan.linalgebra

import scalan.linalgebra.MatricesDsl

trait LinearAlgebraExamples extends MatricesDsl {
  lazy val ddmvm = fun { p: Rep[(Array[Array[Int]], Array[Int])] => 
    val Pair(m, v) = p
    val matrix = RowMajorMatrix(PArray(m.map(fun { r: Arr[Int] => DenseVector(PArray(r)) })))
    val vector = DenseVector(PArray(v))
    (matrix * vector).coords.arr
  }
  
  lazy val dsmvm = fun { p: Rep[(Array[Array[Int]], Array[Int])] => 
    val Pair(m, v) = p
    val matrix = RowMajorMatrix(PArray(m.map(fun { r: Arr[Int] => DenseVector(PArray(r)) })))
    val vector = SparseVector(PArray(v))
    (matrix * vector).coords.arr
  }
  
  lazy val sdmvm = fun { p: Rep[(Array[Array[Int]], Array[Int])] => 
    val Pair(m, v) = p
    val matrix = RowMajorSparseMatrix(PArray(m.map(fun { r: Arr[Int] => SparseVector(PArray(r)) })))
    val vector = DenseVector(PArray(v))
    (matrix * vector).coords.arr
  }
  
  lazy val ssmvm = fun { p: Rep[(Array[Array[Int]], Array[Int])] => 
    val Pair(m, v) = p
    val matrix = RowMajorSparseMatrix(PArray(m.map(fun { r: Arr[Int] => SparseVector(PArray(r)) })))
    val vector = SparseVector(PArray(v))
    (matrix * vector).coords.arr
  }
}