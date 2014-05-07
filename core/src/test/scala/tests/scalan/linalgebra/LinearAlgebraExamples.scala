package tests.scalan.linalgebra

import scalan.linalgebra.MatricesDsl

trait LinearAlgebraExamples extends MatricesDsl {
  lazy val ddmvm = fun { p: Rep[(Array[Array[Int]], Array[Int])] => 
    val Pair(m, v) = p
    val matrix = RowMajorMatrix(PArray(m.map(fun { r: Arr[Int] => DenseVector(PArray(r)) })))
    val vector = DenseVector(PArray(v))
    (matrix * vector).coords.arr
  }
  
  lazy val dsmvm = fun { p: Rep[(Array[Array[Int]], (Array[Int], (Array[Int], Int)))] => 
    val Tuple(m, vIs, vVs, vL) = p
    val matrix = RowMajorMatrix(PArray(m.map(fun { r: Arr[Int] => DenseVector(PArray(r)) })))
    val vector = SparseVector(vIs, PArray(vVs), vL)
    (matrix * vector).coords.arr
  }
  
  lazy val sdmvm = fun { p: Rep[(Array[(Array[Int], (Array[Int], Int))], Array[Int])] => 
    val Pair(m, v) = p
    val matrix = RowMajorSparseMatrix(PArray(m.map(fun { r: Rep[(Array[Int], (Array[Int], Int))] => SparseVector(r._1, PArray(r._2), r._3) })))
    val vector = DenseVector(PArray(v))
    (matrix * vector).coords.arr
  }
  
  lazy val ssmvm = fun { p: Rep[(Array[(Array[Int], (Array[Int], Int))], (Array[Int], (Array[Int], Int)))] => 
    val Tuple(m, vIs, vVs, vL) = p
    val matrix = RowMajorSparseMatrix(PArray(m.map(fun { r: Rep[(Array[Int], (Array[Int], Int))] => SparseVector(r._1, PArray(r._2), r._3) })))
    val vector = SparseVector(vIs, PArray(vVs), vL)
    (matrix * vector).coords.arr
  }
}