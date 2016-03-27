package scalan.linalgebra

trait LinearAlgebraExamples extends MatricesDsl {

  def vvm[T](v1: Vec[T], v2: Vec[T])(implicit eT: Elem[T], n: Numeric[T]): Vec[T] = v1 *^ v2
  def vva[T](v1: Vec[T], v2: Vec[T])(implicit eT: Elem[T], n: Numeric[T]): Vec[T] = v1 +^ v2

  lazy val dd_vvm = fun { p: Rep[(Array[Double], Array[Double])] =>
    val Pair(arr1, arr2) = p
    val (items1, items2) = (Collection(arr1), Collection(arr2))
    vvm(DenseVector(items1), DenseVector(items2)).items.arr
  }
  lazy val ds_vvm = fun { p: Rep[(Array[Double], (Array[(Int, Double)], Int))] =>
    val Pair(arr1, Pair(arr2, len)) = p
    val (items1, items2) = (Collection(arr1), Collection(arr2))
    vvm(DenseVector(items1), SparseVector(items2.as, items2.bs, len)).items.arr
  }
  lazy val sd_vvm = fun { p: Rep[((Array[(Int, Double)], Int), Array[Double])] =>
    val Pair(Pair(arr1, len), arr2) = p
    val (items1, items2) = (Collection(arr1), Collection(arr2))
    vvm(SparseVector(items1.as, items1.bs, len), DenseVector(items2)).items.arr
  }
  lazy val ss_vvm = fun { p: Rep[((Array[(Int, Double)], Int), (Array[(Int, Double)], Int))] =>
    val Pair(Pair(arr1, len1), Pair(arr2, len2)) = p
    val (items1, items2) = (Collection(arr1), Collection(arr2))
    vvm(SparseVector(items1.as, items1.bs, len1), SparseVector(items2.as, items2.bs, len2)).items.arr
  }

  lazy val dd_vva = fun { p: Rep[(Array[Double], Array[Double])] =>
    val Pair(arr1, arr2) = p
    val (items1, items2) = (Collection(arr1), Collection(arr2))
    vva(DenseVector(items1), DenseVector(items2)).items.arr
  }
  lazy val ds_vva = fun { p: Rep[(Array[Double], (Array[(Int, Double)], Int))] =>
    val Pair(arr1, Pair(arr2, len)) = p
    val (items1, items2) = (Collection(arr1), Collection(arr2))
    vva(DenseVector(items1), SparseVector(items2.as, items2.bs, len)).items.arr
  }
  lazy val sd_vva = fun { p: Rep[((Array[(Int, Double)], Int), Array[Double])] =>
    val Pair(Pair(arr1, len), arr2) = p
    val (items1, items2) = (Collection(arr1), Collection(arr2))
    vva(SparseVector(items1.as, items1.bs, len), DenseVector(items2)).items.arr
  }
  lazy val ss_vva = fun { p: Rep[((Array[(Int, Double)], Int), (Array[(Int, Double)], Int))] =>
    val Pair(Pair(arr1, len1), Pair(arr2, len2)) = p
    val (items1, items2) = (Collection(arr1), Collection(arr2))
    vva(SparseVector(items1.as, items1.bs, len1), SparseVector(items2.as, items2.bs, len2)).items.arr
  }

  def mvm[T](matrix: Matr[T], vector: Vec[T])(implicit eT: Elem[T], n: Numeric[T]): Vec[T] =
    DenseVector(matrix.rows.mapBy( fun{ r => r dot vector }))

  lazy val vvm = fun { p: Rep[(Collection[Double], Collection[Double])] =>
    val Pair(items1, items2) = p
    val v1 = DenseVector(items1)
    val v2 = DenseVector(items2)
    v1.dot(v2)
  }

  lazy val aamvm = fun { p: Rep[(Matrix[Double], Vector[Double])] =>
    val Pair(m, v) = p
    mvm(m, v)
  }

  lazy val ddmvm = fun { p: Rep[(Array[Array[Double]], Array[Double])] =>
    val Pair(m, v) = p
    val width = m(0).length
    val matrix: Matr[Double] = CompoundMatrix(Collection(m.map { r: Arr[Double] => DenseVector(Collection(r)) }), width)
    val vector: Vec[Double] = DenseVector(Collection(v))
    (matrix * vector).items.arr
  }

  lazy val ddmvmList = fun { p: Rep[(List[Array[Double]], Array[Double])] =>
    val Pair(m, v) = p
    val width = m(0).length
    val matrix: Matr[Double] = CompoundMatrix(CollectionOverList(m.mapBy( fun { r: Arr[Double] => DenseVector(Collection(r)) })), width)
    val vector: Vec[Double] = DenseVector(Collection(v))
    (matrix * vector).items.arr
  }

  lazy val dsmvm = fun { p: Rep[(Array[Array[Double]], (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val width = m(0).length
    val matrix: Matr[Double] = CompoundMatrix(Collection(m.map { r: Arr[Double] => DenseVector(Collection(r)) }), width)
    val vector: Vec[Double] = SparseVector(Collection(vIs), Collection(vVs), vL)
    (matrix * vector).items.arr
  }

  lazy val sdmvm = fun { p: Rep[(Array[(Array[Int], (Array[Double], Int))], Array[Double])] =>
    val Pair(m, v) = p
    val width = m(0)._3
    val matrix: Matr[Double] = CompoundMatrix(Collection(m.map {
      r: Rep[(Array[Int], (Array[Double], Int))] =>
      SparseVector(Collection(r._1), Collection(r._2), r._3)}), width)
    val vector: Vec[Double] = DenseVector(Collection(v))
    (matrix * vector).items.arr
  }

  lazy val ssmvm = fun { p: Rep[(Array[(Array[Int], (Array[Double], Int))], (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val width = m(0)._3
    val matrix: Matr[Double] = CompoundMatrix(Collection(m.map {
      r: Rep[(Array[Int], (Array[Double], Int))] =>
        SparseVector(Collection(r._1), Collection(r._2), r._3) }), width)
    val vector: Vec[Double] = SparseVector(Collection(vIs), Collection(vVs), vL)
    (matrix * vector).items.arr
  }

  lazy val fdmvm = fun { p: Rep[((Array[Double], Int), Array[Double])] =>
    val Pair(m, v) = p
    val matrix: Matr[Double] = DenseFlatMatrix(Collection(m._1), m._2)
    val vector: Vec[Double] = DenseVector(Collection(v))
    (matrix * vector).items.arr
  }

  lazy val fsmvm = fun { p: Rep[((Array[Double], Int), (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val matrix: Matr[Double] = DenseFlatMatrix(Collection(m._1), m._2)
    val vector: Vec[Double] = SparseVector(Collection(vIs), Collection(vVs), vL)
    (matrix * vector).items.arr
  }

  lazy val cdmvm = fun { p: Rep[((Double, (Int, Int)), Array[Double])] =>
    val Pair(m, v) = p
    val matrix: Matr[Double] = ConstMatrix(m._1, m._2, m._3)
    val vector: Vec[Double] = DenseVector(Collection(v))
    (matrix * vector).items.arr
  }

  lazy val csmvm = fun { p: Rep[((Double, (Int, Int)), (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val matrix: Matr[Double] = ConstMatrix(m._1, m._2, m._3)
    val vector: Vec[Double] = SparseVector(Collection(vIs), Collection(vVs), vL)
    (matrix * vector).items.arr
  }

  lazy val dcmvm = fun { p: Rep[(Array[Array[Double]], (Double, Int))] =>
    val Pair(m, v) = p
    val width = m(0).length
    val matrix: Matr[Double] = CompoundMatrix(Collection(m.map { r: Arr[Double] => DenseVector(Collection(r)) }), width)
    val vector: Vec[Double] = ConstVector(v._1, v._2)
    (matrix * vector).items.arr
  }

  lazy val ccmvm = fun { p: Rep[((Double, (Int, Int)), (Double, Int))] =>
    val Pair(m, v) = p
    val matrix: Matr[Double] = ConstMatrix(m._1, m._2, m._3)
    val vector: Vec[Double] = ConstVector(v._1, v._2)
    (matrix * vector).items.arr
  }

  lazy val dgdmvm = fun { p: Rep[(Array[Double], Array[Double])] =>
    val Pair(m, v) = p
    val matrix: Matr[Double] = DiagonalMatrix(Collection(m))
    val vector: Vec[Double] = DenseVector(Collection(v))
    (matrix * vector).items.arr
  }

  lazy val dgsmvm = fun { p: Rep[(Array[Double], (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val matrix: Matr[Double] = DiagonalMatrix(Collection(m))
    val vector: Vec[Double] = SparseVector(Collection(vIs), Collection(vVs), vL)
    (matrix * vector).items.arr
  }

  lazy val dgcmvm = fun { p: Rep[(Array[Double], (Double, Int))] =>
    val Pair(m, v) = p
    val matrix: Matr[Double] = DiagonalMatrix(Collection(m))
    val vector: Vec[Double] = ConstVector(v._1, v._2)
    (matrix * vector).items.arr
  }

  lazy val ddmvm0 = fun { p: Rep[(Array[Array[Double]], Array[Double])] =>
    val Pair(m, v) = p
    val width = m(0).length
    val matrix: Matr[Double] = CompoundMatrix(Collection(m.map { r: Arr[Double] => DenseVector(Collection(r)) }), width)
    val vector: Vec[Double] = DenseVector(Collection(v))
    mvm(matrix, vector).items.arr
  }

  lazy val dsmvm0 = fun { p: Rep[(Array[Array[Double]], (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val width = m(0).length
    val matrix: Matr[Double] = CompoundMatrix(Collection(m.map { r: Arr[Double] => DenseVector(Collection(r)) }), width)
    val vector: Vec[Double] = SparseVector(Collection(vIs), Collection(vVs), vL)
    mvm(matrix, vector).items.arr
  }

  lazy val sdmvm0 = fun { p: Rep[(Array[(Array[Int], (Array[Double], Int))], Array[Double])] =>
    val Pair(m, v) = p
    val width = m(0)._3
    val matrix: Matr[Double] = CompoundMatrix(Collection(m.map {
      r: Rep[(Array[Int], (Array[Double], Int))] =>
        SparseVector(Collection(r._1), Collection(r._2), r._3) }), width)
    val vector: Vec[Double] = DenseVector(Collection(v))
    mvm(matrix, vector).items.arr
  }

  lazy val ssmvm0 = fun { p: Rep[(Array[(Array[Int], (Array[Double], Int))], (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val width = m(0)._3
    val matrix: Matr[Double] = CompoundMatrix(Collection(m.map {
      r: Rep[(Array[Int], (Array[Double], Int))] =>
        SparseVector(Collection(r._1), Collection(r._2), r._3) }), width)
    val vector: Vec[Double] = SparseVector(Collection(vIs), Collection(vVs), vL)
    mvm(matrix, vector).items.arr
  }

  lazy val fdmvm0 = fun { p: Rep[((Array[Double], Int), Array[Double])] =>
    val Pair(m, v) = p
    val matrix: Matr[Double] = DenseFlatMatrix(Collection(m._1), m._2)
    val vector: Vec[Double] = DenseVector(Collection(v))
    mvm(matrix, vector).items.arr
  }

  lazy val fsmvm0 = fun { p: Rep[((Array[Double], Int), (Array[Int], (Array[Double], Int)))] =>
    val Tuple(m, vIs, vVs, vL) = p
    val matrix: Matr[Double] = DenseFlatMatrix(Collection(m._1), m._2)
    val vector: Vec[Double] = SparseVector(Collection(vIs), Collection(vVs), vL)
    mvm(matrix, vector).items.arr
  }

  lazy val ddmmm = fun { p: Rep[(Array[Array[Double]], Array[Array[Double]])] =>
    val Pair(m1, m2) = p
    val width1 = m1(0).length
    val width2 = m2(0).length
    val matrix1 = CompoundMatrix(Collection(m1.map { r: Arr[Double] => DenseVector(Collection(r)) }), width1)
    val matrix2 = CompoundMatrix(Collection(m2.map { r: Arr[Double] => DenseVector(Collection(r)) }), width2)
    (matrix1 * matrix2).rows.arr.map(_.items.arr)
  }

  lazy val ssmmm = fun { p: Rep[(Array[(Array[Int], (Array[Double], Int))], Array[(Array[Int], (Array[Double], Int))])] =>
    val Pair(m1, m2) = p
    val width1 = m1(0)._3
    val width2 = m2(0)._3
    val matrix1 = CompoundMatrix(Collection(m1.map { r => SparseVector(Collection(r._1), Collection(r._2), r._3) }), width1)
    val matrix2 = CompoundMatrix(Collection(m2.map { r => SparseVector(Collection(r._1), Collection(r._2), r._3) }), width2)
    (matrix1 * matrix2).rows.arr.map(_.items.arr)
  }

  lazy val ffmmm = fun { p: Rep[((Array[Double], Int), (Array[Double], Int))] =>
    val Pair(m1, m2) = p
    val matrix1 = DenseFlatMatrix(Collection(m1._1), m1._2)
    val matrix2 = DenseFlatMatrix(Collection(m2._1), m2._2)
    (matrix1 * matrix2).rows.arr.map(_.items.arr)
  }

  lazy val ccmmm = fun { p: Rep[((Double, (Int, Int)), (Double, (Int, Int)))] =>
    val Pair(m1, m2) = p
    val matrix1 = ConstMatrix(m1._1, m1._2, m1._3)
    val matrix2 = ConstMatrix(m2._1, m2._2, m2._3)
    (matrix1 * matrix2).rows.arr.map(_.items.arr)
  }

  lazy val cfmmm = fun { p: Rep[((Double, (Int, Int)), (Array[Double], Int))] =>
    val Pair(m1, m2) = p
    val matrix1 = ConstMatrix(m1._1, m1._2, m1._3)
    val matrix2 = DenseFlatMatrix(Collection(m2._1), m2._2)
    (matrix1 * matrix2).rows.arr.map(_.items.arr)
  }

  lazy val dgfmmm = fun { p: Rep[(Array[Double], (Array[Double], Int))] =>
    val Pair(m1, m2) = p
    val matrix1 = DiagonalMatrix(Collection(m1))
    val matrix2 = DenseFlatMatrix(Collection(m2._1), m2._2)
    (matrix1 * matrix2).rows.arr.map(_.items.arr)
  }

  lazy val dgdgmmm = fun { p: Rep[(Array[Double], Array[Double])] =>
    val Pair(m1, m2) = p
    val matrix1 = DiagonalMatrix(Collection(m1))
    val matrix2 = DiagonalMatrix(Collection(m2))
    (matrix1 * matrix2).rows.arr.map(_.items.arr)
  }

  lazy val dotWithAbstractElem = fun { in: Rep[(Int, Array[(Int, Double)])] =>
    def RandomMatrix(numRows: IntRep, numColumns: IntRep): Matr[Double] = {
      val n = numRows * numColumns
      val coll = Collection.replicate(n, 0.0)
      DenseFlatMatrix(coll, numColumns)
    }

    val Tuple(width, arrFlat) = in
    val nUsers = arrFlat.length
    val mP = RandomMatrix(nUsers, width)
    val rowsColl = mP.rows
    val coll = (rowsColl zip rowsColl).map {
      case Tuple(vR, vP) =>
        val vectorElem = vP.selfType1
        //assert(vectorElem.getClass == classOf[VectorElem[_, _]])
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

  lazy val applySparseVector = fun { in: Rep[(Array[(Int, Double)], (Int, Int))] =>
    val Tuple(a, n, i) = in
    val coll = CollectionOverArray(a)
    val vec = SparseVector(coll, n)
    val res = vec(i).toInt
    res
  }

  lazy val jArrTrain2x2 = Array(Array((0, 5.0), (1, 3.0)), Array((1, 4.0)))

  lazy val transpose = fun { in: Rep[(Array[(Int, Double)], (Array[(Int, Int)], Int))] =>
    val Tuple(arrFlat, segsArr, nItems) = in
    val nColl: NColl[(Int, Double)] = NestedCollectionFlat(Collection(arrFlat), PairCollectionAOS.fromArray(segsArr))
    val mR: Matr[Double] = CompoundMatrix.fromNColl(nColl, nItems)
    mR.transpose.rows.map({v => v.nonZeroItems.arr}).arr
  }

  lazy val funRandom = fun {in: Rep[Double] =>
    val one = toRep(1.0)
    val thousand = toRep(1000.0)
    val res = Collection.replicate(toRep(10000), one).map(v => Thunk { random(in) } ).arr
    res // TODO: no reduce for Thunk, also backend fails in Math.abs()
    /*val average = (thousand * res.reduce / res.length.toDouble + toRep(0.5)).toInt.toDouble / thousand
    val stddev = (thousand * Math.sqrt(res.map(v => (v - average) * (v - average)).reduce / res.length.toDouble) +
      toRep(0.5)).toInt.toDouble / thousand
    println("average: " + average)
    println("stddev:  " + stddev)
    (Math.sqrt(average * average) < one / thousand, Math.sqrt((stddev - in) * (stddev - in)) < one / thousand)*/
  }

  lazy val funRandomArray = fun {in: Rep[Int] =>
    val one = toRep(1.0)
    val thousand = toRep(1000.0)
    val arr = Collection.replicate(in, one).arr
    val res = array_randomGaussian(toRep(0.0), one, arr)
    val average = (thousand * res.reduce / res.length.toDouble + toRep(0.5)).toInt.toDouble / thousand
    val stddev = (thousand * Math.sqrt(res.map(v => (v - average) * (v - average)).reduce / res.length.toDouble) +
      toRep(0.5)).toInt.toDouble / thousand
    println("average: " + average)
    println("stddev:  " + stddev)
    // TODO: backend fails in Math.abs()
    (Math.sqrt(average * average) < one / thousand, Math.sqrt((stddev - one) * (stddev - one)) < one / thousand)
  }

  lazy val funZipMapViewBoth = fun {in: Rep[Int] =>
    val collVec = Collection.replicate(in, in.toDouble).map(v => DenseVector(Collection.replicate(in, v)))
    val res = (collVec zip collVec).map { case Pair(v1, v2) => (v1 +^ v2).reduce}.reduce
    res
  }

  lazy val funZipMapViewLeft = fun {in: Rep[Int] =>
    val collVec = Collection.replicate(in, in.toDouble).map(v => DenseVector(Collection.replicate(in, v)))
    val res = (collVec zip Collection.replicate(in, in.toDouble)).map { case Pair(v1, v2) => (v1 +^ v2).reduce}.reduce
    res
  }

  lazy val funZipMapViewRight = fun {in: Rep[Int] =>
    val collVec = Collection.replicate(in, in.toDouble).map(v => DenseVector(Collection.replicate(in, v)))
    val res = (Collection.replicate(in, in.toDouble) zip collVec).map { case Pair(v1, v2) => (v2 +^ v1).reduce}.reduce
    res
  }

  def getNArrayWithSegmentsFromJaggedArray(jaggedArray: Array[Array[(Int, Double)]]) = {
    //val arr = jaggedArray.flatMap(v => v)
    val arrI = jaggedArray.flatMap(v => v.map(_._1))
    val arrV = jaggedArray.flatMap(v => v.map(_._2))
    val lens = jaggedArray.map(i => i.length)
    val offs = lens.scanLeft(0)((x, y) => x + y).take(lens.length)
    (arrI zip arrV, offs zip lens)
  }

  lazy val collReplicateFilter = fun { xs: Rep[Array[Int]] =>
    val c = Collection(xs)
    val cs = SArray.replicate(5, c)
    val csFilt = cs.filter(coll => coll.length > 0)
    csFilt.map(coll => coll.arr)
  }

  lazy val sumConvertSum = fun { xs: Rep[Array[Double]] =>
    val vX = denseVector(xs)
    val vA = vX +^ vX
    val vB = vA.convertTo[SparseVector[Double]]
    val vC = vA +^ vB
    vC.reduce
  }

  lazy val denseVector = fun { p: Rep[Array[Double]] =>
    DenseVector(Collection(p))
  }
  lazy val constVector = fun { p: Rep[(Double, Int)] =>
    ConstVector(p._1, p._2)
  }
  lazy val sparseVector = fun { p: Rep[(Array[Int], (Array[Double], Int))] =>
    val Tuple(is, vs, n) = p
    SparseVector(Collection(is), Collection(vs), n)
  }
  lazy val sparseVector1 = fun { p: Rep[(Array[(Int, Double)], Int)] =>
    val Tuple(arr, n) = p
    SparseVector1(Collection(arr), n)
  }

  lazy val flatMatrix = fun { p: Rep[(Array[Double], Int)] =>
    DenseFlatMatrix(Collection(p._1), p._2)
  }
  lazy val constMatrix = fun { p: Rep[(Double, (Int, Int))] =>
    ConstMatrix(p._1, p._2, p._3)
  }

  lazy val diagonalMatrix = fun { p: Rep[Array[Double]] =>
    DiagonalMatrix(Collection(p))
  }

  def dmUpdate = fun { in: Rep[(Array[Array[Double]], (Array[Double], Boolean))] =>
    val Tuple(m, v, cond) = in
    val width = m(0).length
    val matrix: Matr[Double] = CompoundMatrix(Collection(m).map{r => DenseVector(Collection(r))}, width)
    val vector: Vec[Double] = DenseVector(Collection(v))
    val matrixUpd = matrix.rows.update(toRep(1), vector)
    val res = IF (cond) THEN { Pair(matrixUpd, toRep(1)) } ELSE { Pair(matrixUpd, toRep(2)) }
    res._2
  }
}
