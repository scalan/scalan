package scalan.linalgebra

import scalan.ScalanCommunityDsl

trait LinearAlgebraExamples extends MatricesDsl { self: ScalanCommunityDsl =>
  def mvm[T](matrix: Matrix[T], vector: Vector[T])(implicit eT: Elem[T], n: Numeric[T]): Vector[T] =
    matrix.rows.map { r => r.dot(vector) }

  lazy val ddmvm = fun { p: Rep[(Array[Array[Double]], Array[Double])] =>
    val Pair(m, v) = p
    val matrix: Matrix[Double] = RowMajorMatrix(PArray(m.map { r: Arr[Double] => DenseVector(PArray(r)) }))
    val vector: Vector[Double] = DenseVector(PArray(v))
    (matrix * vector).coords.arr
  }

  lazy val dsmvm = fun { p: Rep[(Array[Array[Double]], (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val matrix: Matrix[Double] = RowMajorMatrix(PArray(m.map { r: Arr[Double] => DenseVector(PArray(r)) }))
    val vector: Vector[Double] = SparseVector(vIs, PArray(vVs), vL)
    (matrix * vector).coords.arr
  }

  lazy val sdmvm = fun { p: Rep[(Array[(Array[Int], (Array[Double], Int))], Array[Double])] =>
    val Pair(m, v) = p
    val matrix: Matrix[Double] = RowMajorSparseMatrix(PArray(m.map { r: Rep[(Array[Int], (Array[Double], Int))] => SparseVector(r._1, PArray(r._2), r._3) }))
    val vector: Vector[Double] = DenseVector(PArray(v))
    (matrix * vector).coords.arr
  }

  lazy val ssmvm = fun { p: Rep[(Array[(Array[Int], (Array[Double], Int))], (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val matrix: Matrix[Double] = RowMajorSparseMatrix(PArray(m.map { r: Rep[(Array[Int], (Array[Double], Int))] => SparseVector(r._1, PArray(r._2), r._3) }))
    val vector: Vector[Double] = SparseVector(vIs, PArray(vVs), vL)
    (matrix * vector).coords.arr
  }

  lazy val fdmvm = fun { p: Rep[((Array[Double], Int), Array[Double])] =>
    val Pair(m, v) = p
    val matrix: Matrix[Double] = RowMajorFlatMatrix(PArray(m._1), m._2)
    val vector: Vector[Double] = DenseVector(PArray(v))
    (matrix * vector).coords.arr
  }

  lazy val fsmvm = fun { p: Rep[((Array[Double], Int), (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val matrix: Matrix[Double] = RowMajorFlatMatrix(PArray(m._1), m._2)
    val vector: Vector[Double] = SparseVector(vIs, PArray(vVs), vL)
    (matrix * vector).coords.arr
  }

  lazy val ddmvm0 = fun { p: Rep[(Array[Array[Double]], Array[Double])] =>
    val Pair(m, v) = p
    val matrix: Matrix[Double] = RowMajorMatrix(PArray(m.map { r: Arr[Double] => DenseVector(PArray(r)) }))
    val vector: Vector[Double] = DenseVector(PArray(v))
    mvm(matrix, vector).coords.arr
  }

  lazy val dsmvm0 = fun { p: Rep[(Array[Array[Double]], (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val matrix: Matrix[Double] = RowMajorMatrix(PArray(m.map { r: Arr[Double] => DenseVector(PArray(r)) }))
    val vector: Vector[Double] = SparseVector(vIs, PArray(vVs), vL)
    mvm(matrix, vector).coords.arr
  }

  lazy val sdmvm0 = fun { p: Rep[(Array[(Array[Int], (Array[Double], Int))], Array[Double])] =>
    val Pair(m, v) = p
    val matrix: Matrix[Double] = RowMajorSparseMatrix(PArray(m.map { r: Rep[(Array[Int], (Array[Double], Int))] => SparseVector(r._1, PArray(r._2), r._3) }))
    val vector: Vector[Double] = DenseVector(PArray(v))
    mvm(matrix, vector).coords.arr
  }

  lazy val ssmvm0 = fun { p: Rep[(Array[(Array[Int], (Array[Double], Int))], (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val matrix: Matrix[Double] = RowMajorSparseMatrix(PArray(m.map { r: Rep[(Array[Int], (Array[Double], Int))] => SparseVector(r._1, PArray(r._2), r._3) }))
    val vector: Vector[Double] = SparseVector(vIs, PArray(vVs), vL)
    mvm(matrix, vector).coords.arr
  }

  lazy val fdmvm0 = fun { p: Rep[((Array[Double], Int), Array[Double])] =>
    val Pair(m, v) = p
    val matrix: Matrix[Double] = RowMajorFlatMatrix(PArray(m._1), m._2)
    val vector: Vector[Double] = DenseVector(PArray(v))
    mvm(matrix, vector).coords.arr
  }

  lazy val fsmvm0 = fun { p: Rep[((Array[Double], Int), (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val matrix: Matrix[Double] = RowMajorFlatMatrix(PArray(m._1), m._2)
    val vector: Vector[Double] = SparseVector(vIs, PArray(vVs), vL)
    mvm(matrix, vector).coords.arr
  }

  lazy val ddmmm = fun { p: Rep[(Array[Array[Double]], Array[Array[Double]])] =>
    val Pair(m1, m2) = p
    val matrix1 = RowMajorMatrix(PArray(m1.map { r: Arr[Double] => DenseVector(PArray(r)) }))
    val matrix2 = RowMajorMatrix(PArray(m2.map { r: Arr[Double] => DenseVector(PArray(r)) }))
    (matrix1 * matrix2).rows.arr.map(_.coords.arr)
  }

  lazy val ssmmm = fun { p: Rep[(Array[(Array[Int], (Array[Double], Int))], Array[(Array[Int], (Array[Double], Int))])] =>
    val Pair(m1, m2) = p
    val matrix1 = RowMajorSparseMatrix(PArray(m1.map { r => SparseVector(r._1, PArray(r._2), r._3) }))
    val matrix2 = RowMajorSparseMatrix(PArray(m2.map { r => SparseVector(r._1, PArray(r._2), r._3) }))
    (matrix1 * matrix2).rows.arr.map(_.coords.arr)
  }

  lazy val ffmmm = fun { p: Rep[((Array[Double], Int), (Array[Double], Int))] =>
    val Pair(m1, m2) = p
    val matrix1 = RowMajorFlatMatrix(PArray(m1._1), m1._2)
    val matrix2 = RowMajorFlatMatrix(PArray(m2._1), m2._2)
    (matrix1 * matrix2).rows.arr.map(_.coords.arr)
  }
}
