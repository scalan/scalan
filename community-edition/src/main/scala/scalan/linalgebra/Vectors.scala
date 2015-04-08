package scalan.linalgebra

/**
 * Created by Victor Smirnov on 3/12/15.
 */

import scalan._
import scalan.common.Default
import scalan.common.OverloadHack.{Overloaded2, Overloaded1}

trait Vectors { self: ScalanCommunityDsl =>

  type Vector[T] = Rep[AbstractVector[T]]

  trait AbstractVector[T] extends Reifiable[AbstractVector[T]] {

    def length: Rep[Int]
    def items: Rep[Collection[T]]
    def nonZeroIndices: Rep[Collection[Int]]
    def nonZeroValues:  Rep[Collection[T]]
    def nonZeroItems: Rep[Collection[(Int, T)]]
    implicit def elem: Elem[T]
    def zeroValue = elem.defaultRepValue

    def apply(i: Rep[Int]): Rep[T]

    def +^(other: Vector[T])(implicit n: Numeric[T]): Vector[T]
    @OverloadId("elementwise_sum_collection")
    def +^(other: Coll[T])(implicit n: Numeric[T], o: Overloaded1): Vector[T] = +^ (DenseVector(other))
    @OverloadId("elementwise_sum_value")
    def +^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vector[T]

    def -^(other: Vector[T])(implicit n: Numeric[T]): Vector[T]
    @OverloadId("elementwise_diff_collection")
    def -^(other: Coll[T])(implicit n: Numeric[T], o: Overloaded1): Vector[T] = -^ (DenseVector(other))
    @OverloadId("elementwise_diff_value")
    def -^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vector[T]

    def *^(other: Vector[T])(implicit n: Numeric[T]): Vector[T]
    @OverloadId("elementwise_mult_collection")
    def *^(other: Coll[T])(implicit n: Numeric[T], o: Overloaded1): Vector[T] = *^ (DenseVector(other))
    @OverloadId("elementwise_mult_value")
    def *^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vector[T]

    def /^(other: Rep[T])(implicit f: Fractional[T]): Vector[T] = *^ (toRep(f.one) / other)

    def *(mat: Rep[AbstractMatrix[T]])(implicit n: Numeric[T], o: Overloaded1): Rep[AbstractMatrix[T]] = ???

    def euclideanNorm(implicit num: Numeric[T]): Rep[Double]

    def reduce(implicit m: RepMonoid[T]): Rep[T]
    def dot(other: Vector[T])(implicit n: Numeric[T]): Rep[T]

    def nonZeroesLength: Rep[Int] = nonZeroItems.length
  }

  abstract class DenseVector[T](val items: Rep[Collection[T]])
                               (implicit val elem: Elem[T])
    extends AbstractVector[T] {

    def length = items.length
    def nonZeroIndices: Rep[Collection[Int]] = nonZeroItems.map { case Pair(i, v) => i }
    def nonZeroValues:  Rep[Collection[T]] = items.filter(v => v !== zeroValue)
    def nonZeroItems:   Rep[Collection[(Int, T)]] = {
      (Collection.indexRange(length) zip items).filter { case Pair(i, v) => v !== zeroValue }
    }

    def apply(i: Rep[Int]): Rep[T] = items(i)

    def +^(other: Vector[T])(implicit n: Numeric[T]): Vector[T] = {
      def default = DenseVector((items zip other.items).map { case Pair(v1, v2) => v1 + v2 })

      matchVector[T, AbstractVector[T]](other) {
        dv => default
      } {
        sv =>
          val nonZeroValuesNew = (sv.nonZeroValues zip items(sv.nonZeroIndices)).map { case Pair(v1, v2) => v1 + v2 }
          DenseVector(items.updateMany(nonZeroIndices, nonZeroValuesNew))
      } {
        v => default
      }
    }
    @OverloadId("elementwise_sum_value")
    def +^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vector[T] = {
      DenseVector(items.map(v => v + other))
    }

    def -^(other: Vector[T])(implicit n: Numeric[T]): Vector[T] = {
      def default = DenseVector((items zip other.items).map { case Pair(v1, v2) => v1 - v2 })

      matchVector[T, AbstractVector[T]](other) {
        dv => default
      } {
        sv =>
          val nonZeroValuesNew = (items(sv.nonZeroIndices) zip sv.nonZeroValues).map { case Pair(v1, v2) => v1 - v2 }
          DenseVector(items.updateMany(nonZeroIndices, nonZeroValuesNew))
      } {
        v => default
      }
    }
    @OverloadId("elementwise_diff_collection")
    def -^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vector[T] = {
      DenseVector(items.map(v => v - other))
    }

    def *^(other: Vector[T])(implicit n: Numeric[T]): Vector[T] = {
      def default = DenseVector((items zip other.items).map { case Pair(v1, v2) => v1 * v2 })

      matchVector[T, AbstractVector[T]](other) {
        dv => default
      } {
        sv =>
          val nonZeroValuesNew = (sv.nonZeroValues zip items(sv.nonZeroIndices)).map { case Pair(v1, v2) => v1 - v2 }
          SparseVector(sv.nonZeroIndices, nonZeroValuesNew, sv.length)
      } {
        v => default
      }
    }
    @OverloadId("elementwise_mult_value")
    def *^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vector[T] = {
      DenseVector(items.map(v => v * other))
    }

    def reduce(implicit m: RepMonoid[T]): Rep[T] = items.reduce(m)
    def dot(other: Vector[T])(implicit n: Numeric[T]): Rep[T] = {
      def default = (other.items zip items).map { case Pair(v1, v2) => v1 * v2 }.reduce

      matchVector[T, T](other) {
        dv => default
      } {
        sv => (items(sv.nonZeroIndices) zip sv.nonZeroValues).map { case Pair(v1, v2) => v1 * v2 }.reduce
      } {
        v => default
      }
    }

    def euclideanNorm(implicit num: Numeric[T]): Rep[Double] = Math.sqrt(items.map(v => v * v).reduce.asRep[Double])
  }

  abstract class SparseVector[T](val nonZeroIndices: Rep[Collection[Int]],
                                 val nonZeroValues: Rep[Collection[T]],
                                 val length: Rep[Int])(implicit val elem: Elem[T])
    extends AbstractVector[T] {

    def items: Rep[Collection[T]] = Collection.replicate(length, zeroValue).updateMany(nonZeroIndices, nonZeroValues)
    def nonZeroItems: Rep[Collection[(Int, T)]] = nonZeroIndices zip nonZeroValues

    def apply(i: Rep[Int]): Rep[T] = ??? // TODO: need efficient way to get value by index

    def +^(other: Vector[T])(implicit n: Numeric[T]): Vector[T] = {
      def default = other +^ self

      matchVector[T, AbstractVector[T]](other) {
        dv => default
      } {
        sv => SparseVector(outerJoin(nonZeroIndices, nonZeroValues, sv.nonZeroIndices, sv.nonZeroValues), length)
      } {
        v => default
      }
    }

    @OverloadId("elementwise_sum_value")
    def +^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vector[T] = {
      DenseVector(items.map(v => v + other))
    }

    def -^(other: Vector[T])(implicit n: Numeric[T]): Vector[T] = {
      matchVector[T, AbstractVector[T]](other) { // TODO: I don't like constructing items in this method
        dv =>
          val nonZeroValuesNew = (nonZeroValues zip dv.items(nonZeroIndices)).map { case Pair(v1, v2) => v1 - v2 }
          DenseVector(dv.items.updateMany(nonZeroIndices, nonZeroValuesNew))
      } {
        sv => SparseVector(outerJoin(nonZeroIndices, nonZeroValues, sv.nonZeroIndices, sv.nonZeroValues), length)
      } {
        v => (v -^ self) *^ n.negate(n.one)
      }
    }
    @OverloadId("elementwise_diff_collection")
    def -^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vector[T] = {
      DenseVector(items.map(v => v - other))
    }

    def *^(other: Vector[T])(implicit n: Numeric[T]): Vector[T] = {
      def default = other *^ self

      matchVector[T, AbstractVector[T]](other) {
        dv => default
      } {
        sv => SparseVector(innerJoin(nonZeroIndices, nonZeroValues, sv.nonZeroIndices, sv.nonZeroValues), length)
      } {
        v => default
      }
    }
    @OverloadId("elementwise_mult_value")
    def *^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vector[T] = {
      SparseVector(nonZeroIndices, nonZeroValues.map(v => v * other), length)
    }

    def reduce(implicit m: RepMonoid[T]): Rep[T] = items.reduce(m)  //TODO: it's inefficient

    def dot(other: Rep[AbstractVector[T]])(implicit n: Numeric[T]): Rep[T] = {
      def default = other.dot(self)

      matchVector[T, T](other) {
        dv => default
      } {
        // TODO implement innerJoin and uncomment
        //sv => innerJoin(nonZeroIndices, nonZeroValues, sv.nonZeroIndices, sv.nonZeroValues).bs.reduce
        sv => dotSparse(nonZeroIndices, nonZeroValues, sv.nonZeroIndices, sv.nonZeroValues)
      } {
        v => default
      }
    }

    def euclideanNorm(implicit num: Numeric[T]): Rep[Double] = Math.sqrt(nonZeroValues.map(v => v * v).reduce.asRep[Double])
  }

  trait AbstractVectorCompanion extends TypeFamily1[AbstractVector] {
    def defaultOf[T: Elem] = DenseVector.defaultOf[T]
    def zero[T: Elem](len: Rep[Int]) = DenseVector.zero[T](len)
  }

  trait DenseVectorCompanion extends ConcreteClass1[AbstractVector] {
    def defaultOf[T: Elem] = DenseVector.defaultOf[T]
    def zero[T: Elem](len: Rep[Int]): Vector[T] = {
      val zeroV = element[T].defaultRepValue
      DenseVector(Collection.replicate(len, zeroV))
    }
  }

  trait SparseVectorCompanion extends ConcreteClass1[AbstractVector] {
    def defaultOf[T: Elem] = {
      Default.defaultVal(SparseVector(element[Collection[Int]].defaultRepValue, element[Collection[T]].defaultRepValue, IntElement.defaultRepValue))
    }
    def apply[T: Elem](items: Rep[Collection[T]])(implicit n: Numeric[T], o: Overloaded1): Rep[SparseVector[T]] = {
      val nonZeroItems: Rep[IPairCollection[Int, T]] =
        (Collection.indexRange(items.length) zip items).filter { case Pair(i, v) => v !== n.zero }
      SparseVector(nonZeroItems, items.length)
    }
    @OverloadId("SparseVectorCompanion_apply_nonZeroItems")
    def apply[T: Elem](nonZeroItems: Rep[IPairCollection[Int, T]], length: Rep[Int])
                      (implicit n: Numeric[T], o: Overloaded2): Rep[SparseVector[T]] = {
      SparseVector(nonZeroItems.as, nonZeroItems.bs, length)
    }
    def zero[T: Elem](len: Rep[Int]) = SparseVector(emptyColl[Int], emptyColl[T], len)
  }
}

trait VectorsDsl extends impl.VectorsAbs { self: ScalanCommunityDsl =>

  def matchVector[T, R](vector: Vector[T])
                       (dense: Rep[DenseVector[T]] => Rep[R])
                       (sparse: Rep[SparseVector[T]] => Rep[R])
                       (fallback: Vector[T] => Rep[R]): Rep[R]

  def dotSparse[T: Elem](xIndices: Coll[Int], xValues: Coll[T], yIndices: Coll[Int], yValues: Coll[T])
                        (implicit n: Numeric[T]): Rep[T]

  def dotMerge[T: Elem](xItems: Coll[T], yIndices: Coll[Int], yValues: Coll[T])
                       (implicit n: Numeric[T]): Rep[T] = {
    (xItems(yIndices) zip yValues).map { case Pair(x, y) => x * y }.reduce
  }

  def innerJoin[T: Elem](xIndices: Coll[Int], xValues: Coll[T], yIndices: Coll[Int], yValues: Coll[T])
                        (implicit n: Numeric[T]): PairColl[Int, T]

  def outerJoin[T: Elem](xIndices: Coll[Int], xValues: Coll[T], yIndices: Coll[Int], yValues: Coll[T])
                        (implicit n: Numeric[T]): PairColl[Int, T]
}

trait VectorsDslSeq extends impl.VectorsSeq { self: ScalanCommunityDslSeq =>

  def matchVector[T, R](vector: Vector[T])
                       (dense: Rep[DenseVector[T]] => Rep[R])
                       (sparse: Rep[SparseVector[T]] => Rep[R])
                       (fallback: Vector[T] => Rep[R]): Rep[R] = {
    vector match {
      case dv: DenseVector[_] => dense(dv)
      case sv: SparseVector[_] => sparse(sv)
        // never happens in this case
      case _ => fallback(vector)
    }
  }

  def dotSparse[T: Elem](xIndices: Coll[Int], xValues: Coll[T], yIndices: Coll[Int], yValues: Coll[T])
                        (implicit n: Numeric[T]): Rep[T] = {
    var result = n.zero
    val yMap = yIndices.arr.zip(yValues.arr).toMap
    xIndices.arr.zip(xValues.arr).foldLeft(n.zero) {
      case (acc, (i, x)) =>
      yMap.get(i) match {
        case Some(y) => acc + x * y
        case None => acc
      }
    }
  }

  //TODO: need to implement innerJoin and outerJoin, preferable in Collections DSL
  def innerJoin[T: Elem](xIndices: Coll[Int], xValues: Coll[T], yIndices: Coll[Int], yValues: Coll[T])
                        (implicit n: Numeric[T]): PairColl[Int, T] = ???/*{
    var result = n.zero
    val yMap = (yIndices.arr zip yValues.arr).toMap
    (xIndices.arr zip xValues.arr).foldLeft(n.zero) {
      case (acc, (i, x)) =>
        yMap.get(i) match {
          case Some(y) => acc + x * y
          case None => acc
        }
    }
  }*/

  def outerJoin[T: Elem](xIndices: Coll[Int], xValues: Coll[T], yIndices: Coll[Int], yValues: Coll[T])
                        (implicit n: Numeric[T]): PairColl[Int, T] = ???/*{
    val yMap = (yIndices.arr zip yValues.arr).toMap
    (xIndices.arr zip xValues.arr) {
      case (i, x) =>
        yMap.get(i) match {
          case Some(y) => x * y
          case None => (0, n.zero)
        }
    }
  }*/

  def pairArray_innerJoin[Int, T](xs: Coll[(Int, T)], ys: Coll[(Int, T)], f: Rep[((T, T)) => T])
                                 (implicit ordK: Ordering[Int], eR: Elem[T]): Coll[(Int, T)] = ???/*{
    //implicit val eK: Elem[Int] = xs.eItem.eFst

    val xIter = xs.arr.iterator
    val yIter = ys.arr.iterator

    val buffer = mutable.ArrayBuffer[(Int, T)]()

    @tailrec
    def go(keyX: Int, keyY: Int, valueX: T, valueY: T) {
      val cmp = ordK.compare(keyX, keyY)
      if (cmp == 0) {
        // keyX == keyY
        val value = f((valueX, valueY))
        buffer.append((keyX, value))
        if (xIter.hasNext && yIter.hasNext) {
          val (keyX1, valueX1) = xIter.next()
          val (keyY1, valueY1) = yIter.next()
          go(keyX1, keyY1, valueX1, valueY1)
        }
      } else if (cmp < 0) {
        // keyX < keyY
        if (xIter.hasNext) {
          val (keyX1, valueX1) = xIter.next()
          go(keyX1, keyY, valueX1, valueY)
        }
      } else {
        // keyY < keyX
        if (yIter.hasNext) {
          val (keyY1, valueY1) = yIter.next()
          go(keyX, keyY1, valueX, valueY1)
        }
      }
    }

    if (xIter.hasNext && yIter.hasNext) {
      val (keyX1, valueX1) = xIter.next()
      val (keyY1, valueY1) = yIter.next()
      go(keyX1, keyY1, valueX1, valueY1)
    }

    fromArray(buffer.toArray)(pairElement(eK, eR))
  }*/

  def pairArray_outerJoin[Int, T](xs: Coll[(Int, T)], ys: Coll[(Int, T)], f: Rep[((T, T)) => T], f1: Rep[T => T], f2: Rep[T => T])
                                 (implicit ordK: Ordering[Int], eR: Elem[T]): Coll[(Int, T)] = ???/*{
    implicit val eK: Elem[K] = xs.eItem.eFst

    val xIter = xs.toArray.iterator
    val yIter = ys.toArray.iterator

    val buffer = mutable.ArrayBuffer[(K, R)]()

    // called only when yIter is empty
    def finishX() {
      xIter.foreach { kv => buffer.append((kv._1, f1(kv._2))) }
    }

    // called only when xIter is empty
    def finishY() {
      yIter.foreach { kv => buffer.append((kv._1, f2(kv._2))) }
    }

    @tailrec
    def go(keyX: K, keyY: K, valueX: V1, valueY: V2) {
      val cmp = ordK.compare(keyX, keyY)
      if (cmp == 0) {
        // keyX == keyY
        val value = f((valueX, valueY))
        buffer.append((keyX, value))
        (xIter.hasNext, yIter.hasNext) match {
          case (true, true) =>
            val (keyX1, valueX1) = xIter.next()
            val (keyY1, valueY1) = yIter.next()
            go(keyX1, keyY1, valueX1, valueY1)
          case (true, false) =>
            finishX()
          case (false, true) =>
            finishY()
          case _ => {}
        }
      } else if (cmp < 0) {
        // keyX < keyY
        val value = f1(valueX)
        buffer.append((keyX, value))
        if (xIter.hasNext) {
          val (keyX1, valueX1) = xIter.next()
          go(keyX1, keyY, valueX1, valueY)
        } else {
          buffer.append((keyY, f2(valueY)))
          finishY()
        }
      } else {
        // keyY < keyX
        val value = f2(valueY)
        buffer.append((keyY, value))
        if (yIter.hasNext) {
          val (keyY1, valueY1) = yIter.next()
          go(keyX, keyY1, valueX, valueY1)
        } else {
          buffer.append((keyX, f1(valueX)))
          finishX()
        }
      }
    }

    (xIter.hasNext, yIter.hasNext) match {
      case (true, true) =>
        val (keyX1, valueX1) = xIter.next()
        val (keyY1, valueY1) = yIter.next()
        go(keyX1, keyY1, valueX1, valueY1)
      case (true, false) =>
        finishX()
      case (false, true) =>
        finishY()
      case _ => {}
    }

    fromArray(buffer.toArray)(pairElement(eK, eR))
  }*/
}

trait VectorsDslExp extends impl.VectorsExp { self: ScalanCommunityDslExp =>

  def matchVector[T, R](vector: Vector[T])
                       (dense: Rep[DenseVector[T]] => Rep[R])
                       (sparse: Rep[SparseVector[T]] => Rep[R])
                       (fallback: Vector[T] => Rep[R]): Rep[R] = {
    vector.elem.asInstanceOf[Elem[_]] match {
      case _: DenseVectorElem[_] => dense(vector.asRep[DenseVector[T]])
      case _: SparseVectorElem[_] => sparse(vector.asRep[SparseVector[T]])
      case _ => fallback(vector)
    }
  }

  def dotSparse[T: Elem](xIndices: Coll[Int], xValues: Coll[T], yIndices: Coll[Int], yValues: Coll[T])
                        (implicit n: Numeric[T]): Rep[T] = {
    DotSparse(xIndices.arr, xValues.arr, yIndices.arr, yValues.arr)
  }

  case class DotSparse[T](xIndices: Arr[Int], xValues: Arr[T], yIndices: Arr[Int], yValues: Arr[T])
                         (implicit val n: Numeric[T], selfType: Elem[T]) extends BaseDef[T] {
    override def mirror(f: Transformer) = DotSparse(f(xIndices), f(xValues), f(yIndices), f(yValues))
    def uniqueOpId = name(selfType)
  }

  def innerJoin[T: Elem](xIndices: Coll[Int], xValues: Coll[T], yIndices: Coll[Int], yValues: Coll[T])
                        (implicit n: Numeric[T]): PairColl[Int, T] = ???

  def outerJoin[T: Elem](xIndices: Coll[Int], xValues: Coll[T], yIndices: Coll[Int], yValues: Coll[T])
                        (implicit n: Numeric[T]): PairColl[Int, T] = ???
}
