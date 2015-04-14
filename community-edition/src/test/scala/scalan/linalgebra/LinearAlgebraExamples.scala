package scalan.linalgebra

import scalan.ScalanCommunityDsl

trait LinearAlgebraExamples extends MatricesDsl { self: ScalanCommunityDsl =>
  def mvm[T](matrix: Matrix[T], vector: Vector[T])(implicit eT: Elem[T], n: Numeric[T]): Vector[T] =
    DenseVector(matrix.rows.mapBy( fun{ r => r dot vector }))

  lazy val ddmvm = fun { p: Rep[(Array[Array[Double]], Array[Double])] =>
    val Pair(m, v) = p
    val width = m(0).length
    val matrix: Matrix[Double] = RowMajorSparseMatrix(Collection(m.map { r: Arr[Double] => DenseVector(Collection(r)) }), width)
    val vector: Vector[Double] = DenseVector(Collection(v))
    (matrix * vector).items.arr
  }

  lazy val ddmvmList = fun { p: Rep[(List[Array[Double]], Array[Double])] =>
    val Pair(m, v) = p
    val width = m(0).length
    val matrix: Matrix[Double] = RowMajorSparseMatrix(ListCollection(m.mapBy( fun { r: Arr[Double] => DenseVector(Collection(r)) })), width)
    val vector: Vector[Double] = DenseVector(Collection(v))
    (matrix * vector).items.arr
  }

  lazy val dsmvm = fun { p: Rep[(Array[Array[Double]], (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val width = m(0).length
    val matrix: Matrix[Double] = RowMajorSparseMatrix(Collection(m.map { r: Arr[Double] => DenseVector(Collection(r)) }), width)
    val vector: Vector[Double] = SparseVector(Collection(vIs), Collection(vVs), vL)
    (matrix * vector).items.arr
  }

  lazy val sdmvm = fun { p: Rep[(Array[(Array[Int], (Array[Double], Int))], Array[Double])] =>
    val Pair(m, v) = p
    val width = m(0)._3
    val matrix: Matrix[Double] = RowMajorSparseMatrix(Collection(m.map {
      r: Rep[(Array[Int], (Array[Double], Int))] =>
      SparseVector(Collection(r._1), Collection(r._2), r._3)}), width)
    val vector: Vector[Double] = DenseVector(Collection(v))
    (matrix * vector).items.arr
  }

  lazy val ssmvm = fun { p: Rep[(Array[(Array[Int], (Array[Double], Int))], (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val width = m(0)._3
    val matrix: Matrix[Double] = RowMajorSparseMatrix(Collection(m.map {
      r: Rep[(Array[Int], (Array[Double], Int))] =>
        SparseVector(Collection(r._1), Collection(r._2), r._3) }), width)
    val vector: Vector[Double] = SparseVector(Collection(vIs), Collection(vVs), vL)
    (matrix * vector).items.arr
  }

  lazy val fdmvm = fun { p: Rep[((Array[Double], Int), Array[Double])] =>
    val Pair(m, v) = p
    val matrix: Matrix[Double] = RowMajorNestedMatrix(Collection(m._1), m._2)
    val vector: Vector[Double] = DenseVector(Collection(v))
    (matrix * vector).items.arr
  }

  lazy val fsmvm = fun { p: Rep[((Array[Double], Int), (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val matrix: Matrix[Double] = RowMajorNestedMatrix(Collection(m._1), m._2)
    val vector: Vector[Double] = SparseVector(Collection(vIs), Collection(vVs), vL)
    (matrix * vector).items.arr
  }

  lazy val ddmvm0 = fun { p: Rep[(Array[Array[Double]], Array[Double])] =>
    val Pair(m, v) = p
    val width = m(0).length
    val matrix: Matrix[Double] = RowMajorSparseMatrix(Collection(m.map { r: Arr[Double] => DenseVector(Collection(r)) }), width)
    val vector: Vector[Double] = DenseVector(Collection(v))
    mvm(matrix, vector).items.arr
  }

  lazy val dsmvm0 = fun { p: Rep[(Array[Array[Double]], (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val width = m(0).length
    val matrix: Matrix[Double] = RowMajorSparseMatrix(Collection(m.map { r: Arr[Double] => DenseVector(Collection(r)) }), width)
    val vector: Vector[Double] = SparseVector(Collection(vIs), Collection(vVs), vL)
    mvm(matrix, vector).items.arr
  }

  lazy val sdmvm0 = fun { p: Rep[(Array[(Array[Int], (Array[Double], Int))], Array[Double])] =>
    val Pair(m, v) = p
    val width = m(0)._3
    val matrix: Matrix[Double] = RowMajorSparseMatrix(Collection(m.map {
      r: Rep[(Array[Int], (Array[Double], Int))] =>
        SparseVector(Collection(r._1), Collection(r._2), r._3) }), width)
    val vector: Vector[Double] = DenseVector(Collection(v))
    mvm(matrix, vector).items.arr
  }

  lazy val ssmvm0 = fun { p: Rep[(Array[(Array[Int], (Array[Double], Int))], (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val width = m(0)._3
    val matrix: Matrix[Double] = RowMajorSparseMatrix(Collection(m.map {
      r: Rep[(Array[Int], (Array[Double], Int))] =>
        SparseVector(Collection(r._1), Collection(r._2), r._3) }), width)
    val vector: Vector[Double] = SparseVector(Collection(vIs), Collection(vVs), vL)
    mvm(matrix, vector).items.arr
  }

  lazy val fdmvm0 = fun { p: Rep[((Array[Double], Int), Array[Double])] =>
    val Pair(m, v) = p
    val matrix: Matrix[Double] = RowMajorNestedMatrix(Collection(m._1), m._2)
    val vector: Vector[Double] = DenseVector(Collection(v))
    mvm(matrix, vector).items.arr
  }

  lazy val fsmvm0 = fun { p: Rep[((Array[Double], Int), (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val matrix: Matrix[Double] = RowMajorNestedMatrix(Collection(m._1), m._2)
    val vector: Vector[Double] = SparseVector(Collection(vIs), Collection(vVs), vL)
    mvm(matrix, vector).items.arr
  }

  lazy val ddmmm = fun { p: Rep[(Array[Array[Double]], Array[Array[Double]])] =>
    val Pair(m1, m2) = p
    val width1 = m1(0).length
    val width2 = m2(0).length
    val matrix1 = RowMajorSparseMatrix(Collection(m1.map { r: Arr[Double] => DenseVector(Collection(r)) }), width1)
    val matrix2 = RowMajorSparseMatrix(Collection(m2.map { r: Arr[Double] => DenseVector(Collection(r)) }), width2)
    (matrix1 * matrix2).rows.arr.map(_.items.arr)
  }

  lazy val ssmmm = fun { p: Rep[(Array[(Array[Int], (Array[Double], Int))], Array[(Array[Int], (Array[Double], Int))])] =>
    val Pair(m1, m2) = p
    val width1 = m1(0)._3
    val width2 = m2(0)._3
    val matrix1 = RowMajorSparseMatrix(Collection(m1.map { r => SparseVector(Collection(r._1), Collection(r._2), r._3) }), width1)
    val matrix2 = RowMajorSparseMatrix(Collection(m2.map { r => SparseVector(Collection(r._1), Collection(r._2), r._3) }), width2)
    (matrix1 * matrix2).rows.arr.map(_.items.arr)
  }

  lazy val ffmmm = fun { p: Rep[((Array[Double], Int), (Array[Double], Int))] =>
    val Pair(m1, m2) = p
    val matrix1 = RowMajorNestedMatrix(Collection(m1._1), m1._2)
    val matrix2 = RowMajorNestedMatrix(Collection(m2._1), m2._2)
    (matrix1 * matrix2).rows.arr.map(_.items.arr)
  }

  lazy val dotWithAbstractElem = fun { in: Rep[(Int, Array[(Int, Double)])] =>
    def RandomMatrix(numRows: IntRep, numColumns: IntRep): Matrix[Double] = {
      val n = numRows * numColumns
      val coll = Collection.replicate(n, 0.0)
      RowMajorNestedMatrix(coll, numColumns)
    }

    val Tuple(width, arrFlat) = in
    val nUsers = arrFlat.length
    val mP = RandomMatrix(nUsers, width)
    val rowsColl = mP.rows
    val coll = (rowsColl zip rowsColl).map {
      case Tuple(vR, vP) =>
        val abstractVectorElem = vP.selfType1
        assert(abstractVectorElem.getClass == classOf[AbstractVectorElem[_, _]])
        // dot is implemented with a pattern match, remove or change this test if this changes
        val mQReduced = rowsColl map { row => row dot vP }
        mQReduced
    }
    coll.arr.map(_.arr)
  }

  lazy val funSimpleSum = fun { in: Rep[(Array[Array[Int]], Array[Int])] =>
    val Tuple(nestedArr, vecArr) = in
    val rows = Collection( nestedArr.map( arr => DenseVector(Collection(arr))))
    val dv = DenseVector(Collection(vecArr))

    val newRows =  rows.map { v =>
      val resV = IF(v.length === dv.length) THEN {
        dv +^ v
      } ELSE {
        v
      }
      resV //.convertTo[DenseVector[Int]]
    }
    newRows.flatMap { v => v.items}.arr
  }
}
