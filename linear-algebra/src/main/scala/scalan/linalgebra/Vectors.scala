package scalan.linalgebra

/**
 * Created by Victor Smirnov on 3/12/15.
 */

import scalan._
import scalan.collections.{CollectionsDsl, CollectionsDslStd, CollectionsDslExp}
import scalan.common.OverloadHack.{Overloaded1, Overloaded2}
import scala.annotation.unchecked.uncheckedVariance

trait Vectors { self: LADsl =>

  type Vec[T] = Rep[Vector[T]]

  trait Vector[T] extends Def[Vector[T]] {

    def length: Rep[Int]
    def items: Rep[Collection[T]]
    def nonZeroIndices: Rep[Collection[Int]]
    def nonZeroValues:  Rep[Collection[T]]
    def nonZeroItems: PairColl[Int, T]
    implicit def eT: Elem[T]
    def zeroValue = eT.defaultRepValue

    def apply(i: Rep[Int]): Rep[T]
    @OverloadId("apply_by_collection")
    def apply(is: Coll[Int])(implicit o: Overloaded1): Vec[T]

    def mapBy[R: Elem](f: Rep[T => R @uncheckedVariance]): Vec[R]
    def filterBy(f: Rep[T @uncheckedVariance => Boolean]): Vec[T]

    def +^(other: Vec[T])(implicit n: Numeric[T]): Vec[T]
    @OverloadId("elementwise_sum_collection")
    def +^(other: Coll[T])(implicit n: Numeric[T], o: Overloaded1): Vec[T] = +^ (DenseVector(other))
    @OverloadId("elementwise_sum_value")
    def +^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vec[T]

    def -^(other: Vec[T])(implicit n: Numeric[T]): Vec[T]
    @OverloadId("elementwise_diff_collection")
    def -^(other: Coll[T])(implicit n: Numeric[T], o: Overloaded1): Vec[T] = -^ (DenseVector(other))
    @OverloadId("elementwise_diff_value")
    def -^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vec[T]

    def *^(other: Vec[T])(implicit n: Numeric[T]): Vec[T]
    @OverloadId("elementwise_mult_collection")
    def *^(other: Coll[T])(implicit n: Numeric[T], o: Overloaded1): Vec[T] = *^ (DenseVector(other))
    @OverloadId("elementwise_mult_value")
    def *^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vec[T]

    def /^(other: Rep[T])(implicit f: Fractional[T]): Vec[T] = *^ (toRep(f.one) / other)

    def pow_^(order: Rep[Double])(implicit n: Numeric[T], o: Overloaded2): Vec[T]

    def euclideanNorm(implicit num: Numeric[T]): Rep[Double]

    def reduce(implicit m: RepMonoid[T]): Rep[T]
    def dot(other: Vec[T])(implicit n: Numeric[T]): Rep[T]

    def nonZeroesLength: Rep[Int] = nonZeroItems.length

    def companion: Rep[VectorCompanion]
  }

  abstract class DenseVector[T](val items: Rep[Collection[T]])(implicit val eT: Elem[T])
    extends Vector[T] {

    def length = items.length
    def nonZeroIndices = nonZeroItems.as
    def nonZeroValues = items.filter(v => v !== zeroValue)  //TODO: investigate crash in ddmmm if nonZeroItems.bs
    def nonZeroItems = PairCollectionAOS((Collection.indexRange(length) zip items).filter { case Pair(i, v) => v !== zeroValue })

    def apply(i: Rep[Int]): Rep[T] = items(i)
    @OverloadId("apply_by_collection")
    def apply(is: Coll[Int])(implicit o: Overloaded1): Vec[T] = DenseVector(items(is))

    def mapBy[R: Elem](f: Rep[T => R @uncheckedVariance]): Vec[R] = DenseVector(items.mapBy(f))
    def filterBy(f: Rep[T @uncheckedVariance => Boolean]): Vec[T] = DenseVector(items.map(v => IF (f(v)) THEN v ELSE zeroValue))

    def +^(other: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      other match {
        case SparseVector(nonZeroItemsL, _) =>
          val (nonZeroIndicesL, nonZeroValuesL) = (nonZeroItemsL.as, nonZeroItemsL.bs)
          val nonZeroValuesNew = (nonZeroValuesL zip items(nonZeroIndicesL)).map { case Pair(v1, v2) => v1 + v2 }
          DenseVector(items.updateMany(nonZeroIndicesL, nonZeroValuesNew))
        case _ =>
          DenseVector((items zip other.items).map { case Pair(v1, v2) => v1 + v2 })
      }
    }

    @OverloadId("elementwise_sum_value")
    def +^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vec[T] = {
      DenseVector(items.map(v => v + other))
    }

    def -^(other: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      other match {
        case SparseVector(nonZeroItemsL, _) =>
          val (nonZeroIndicesL, nonZeroValuesL) = (nonZeroItemsL.as, nonZeroItemsL.bs)
          val nonZeroValuesNew = (nonZeroValuesL zip items(nonZeroIndicesL)).map { case Pair(v1, v2) => v1 - v2 }
          DenseVector(items.updateMany(nonZeroIndicesL, nonZeroValuesNew))
        case _ =>
          DenseVector((items zip other.items).map { case Pair(v1, v2) => v1 - v2 })
      }
    }
    @OverloadId("elementwise_diff_value")
    def -^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vec[T] = {
      DenseVector(items.map(v => v - other))
    }

    def *^(other: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      other match {
        case SparseVector(nonZeroItemsL, lengthL) =>
          val (nonZeroIndicesL, nonZeroValuesL) = (nonZeroItemsL.as, nonZeroItemsL.bs)
          val nonZeroValuesNew = (nonZeroValuesL zip items(nonZeroIndicesL)).map { case Pair(v1, v2) => v1 * v2 }
          SparseVector(nonZeroIndicesL zip nonZeroValuesNew, lengthL)
        case _ =>
          DenseVector((items zip other.items).map { case Pair(v1, v2) => v1 * v2 })
      }
    }
    @OverloadId("elementwise_mult_value")
    def *^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vec[T] = {
      DenseVector(items.map(v => v * other))
    }

    def pow_^(order: Rep[Double])(implicit n: Numeric[T], o: Overloaded2): Vec[T] = {
      DenseVector(items.map(v => Math.pow(v.toDouble, order).asRep[T]))
    }

    def reduce(implicit m: RepMonoid[T]): Rep[T] = items.reduce(m)
    def dot(other: Vec[T])(implicit n: Numeric[T]): Rep[T] = {
      other match {
        case SparseVector(nonZeroItemsL, _) =>
          (items(nonZeroItemsL.as) zip nonZeroItemsL.bs).map { case Pair(v1, v2) => v1 * v2 }.reduce
        case _ =>
          (other.items zip items).map { case Pair(v1, v2) => v1 * v2 }.reduce
      }
    }

    def euclideanNorm(implicit num: Numeric[T]): Rep[Double] = Math.sqrt(items.map(v => v * v).reduce.asRep[Double])

    def companion = DenseVector
  }

  abstract class ConstVector[T](val item: Rep[T], val length: Rep[Int])
                               (implicit val eT: Elem[T])
    extends Vector[T] {

    def items: Rep[Collection[T]] = Collection.replicate(length, item)
    def nonZeroIndices: Rep[Collection[Int]] = IF (item !== zeroValue) THEN Collection.indexRange(length) ELSE Collection.empty[Int]
    def nonZeroValues:  Rep[Collection[T]] =  IF (item !== zeroValue) THEN items ELSE Collection.empty[T]
    def nonZeroItems = nonZeroIndices zip nonZeroValues

    def apply(i: Rep[Int]): Rep[T] = item
    @OverloadId("apply_by_collection")
    def apply(is: Coll[Int])(implicit o: Overloaded1): Vec[T] = ConstVector(item, is.length)

    def mapBy[R: Elem](f: Rep[T => R @uncheckedVariance]): Vec[R] = ConstVector(f(item), length)
    def filterBy(f: Rep[T @uncheckedVariance => Boolean]): Vec[T] = IF (f(item)) THEN ConstVector(item, length) ELSE ConstVector(item, 0)

    def +^(other: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      other match {
        case ConstVector(otherItem, _) =>
          ConstVector(item + otherItem, length)
        case SparseVector(nonZeroItemsL, _) =>
          val nonZeroValuesNew = nonZeroItemsL.bs.map { v => v + item }
          DenseVector(items.updateMany(nonZeroItemsL.as, nonZeroValuesNew))
        case _ =>
          DenseVector(other.items.map { v => v + item })
      }
    }

    @OverloadId("elementwise_sum_value")
    def +^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vec[T] = {
      ConstVector(item + other, length)
    }

    def -^(other: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      other match {
        case ConstVector(otherItem, _) =>
          ConstVector(item - otherItem, length)
        case SparseVector(nonZeroItemsL, _) =>
          val nonZeroValuesNew = nonZeroItemsL.bs.map { v => v - item }
          DenseVector(items.updateMany(nonZeroItemsL.as, nonZeroValuesNew))
        case _ =>
          DenseVector(other.items.map { v => v - item })
      }
    }
    @OverloadId("elementwise_diff_value")
    def -^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vec[T] = {
      ConstVector(item - other, length)
    }

    def *^(other: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      other match {
        case ConstVector(otherItem, _) =>
          ConstVector(item * otherItem, length)
        case SparseVector(nonZeroItemsL, _) =>
          val nonZeroValuesNew = nonZeroItemsL.bs.map { v => v * item }
          DenseVector(items.updateMany(nonZeroItemsL.as, nonZeroValuesNew))
        case _ =>
          DenseVector(other.items.map { v => v * item })
      }
    }
    @OverloadId("elementwise_mult_value")
    def *^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vec[T] = {
      ConstVector(item * other, length)
    }

    def pow_^(order: Rep[Double])(implicit n: Numeric[T], o: Overloaded2): Vec[T] = {
      ConstVector(Math.pow(item.toDouble, order).asRep[T], length)
    }

    def reduce(implicit m: RepMonoid[T]): Rep[T] = items.reduce(m)
    def dot(other: Vec[T])(implicit n: Numeric[T]): Rep[T] = {
      (other *^ item).reduce
    }

    def euclideanNorm(implicit num: Numeric[T]): Rep[Double] = Math.sqrt(items.map(v => v * v).reduce.asRep[Double])

    def companion = ConstVector
  }

  abstract class SparseVector[T](val nonZeroItems: PairColl[Int, T], val length: Rep[Int])(implicit val eT: Elem[T])
    extends Vector[T] {

    def items: Rep[Collection[T]] = Collection.replicate(length, zeroValue).updateMany(nonZeroIndices, nonZeroValues)
    def nonZeroIndices = nonZeroItems.as
    def nonZeroValues = nonZeroItems.bs

    def apply(i: IntRep): Rep[T] = {
      val zero = toRep(0)
      IF (nonZeroIndices.length > zero) THEN {
        val k = binarySearch(i, nonZeroIndices)
        IF (k >= zero) THEN nonZeroValues(k) ELSE zeroValue
      } ELSE zeroValue
    }

    @OverloadId("apply_by_collection")
    def apply(is: Coll[Int])(implicit o: Overloaded1): Vec[T] = ??? // TODO: need efficient way to get value by index

    def mapBy[R: Elem](f: Rep[T => R @uncheckedVariance]): Vec[R] = {
      IF (f(zeroValue) === element[R].defaultRepValue) THEN {
        SparseVector(nonZeroIndices, nonZeroValues.mapBy(f), length)
      } ELSE {
        ???
      }
    }
    def filterBy(f: Rep[T @uncheckedVariance => Boolean]): Vec[T] = {
      val filteredItems = nonZeroItems.filter { v => f(v._2) }
      SparseVector(filteredItems, length)
    }

    def +^(other: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      other match {
        case SparseVector(nonZeroItemsL, _) =>
          val newItems = nonZeroItems outerSum nonZeroItemsL
          SparseVector(newItems, length)
        case _ =>
          other +^ self
      }
    }

    @OverloadId("elementwise_sum_value")
    def +^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vec[T] = {
      DenseVector(items.map(v => v + other))
    }

    def -^(other: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      // TODO: I don't like constructing items in this method
      other match {
        case SparseVector(nonZeroItemsL, _) =>
          val newItems = nonZeroItems outerSubtr nonZeroItemsL
          SparseVector(newItems, length)
        case DenseVector(items) =>
          val nonZeroValuesNew = (nonZeroValues zip items(nonZeroIndices)).map { case Pair(v1, v2) => v1 - v2 }
          DenseVector(items.updateMany(nonZeroIndices, nonZeroValuesNew))
        case _ =>
          (other -^ self) *^ n.negate(n.one)
      }
    }
    @OverloadId("elementwise_diff_value")
    def -^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vec[T] = {
      DenseVector(items.map(v => v - other))
    }

    def *^(other: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      other match {
        case SparseVector(nonZeroItemsL, _) =>
          val newItems = nonZeroItems innerMult nonZeroItemsL
          SparseVector(newItems, length)
        case _ =>
          other *^ self
      }
    }
    @OverloadId("elementwise_mult_value")
    def *^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vec[T] = {
      SparseVector(nonZeroIndices zip nonZeroValues.map(v => v * other), length)
    }

    def pow_^(order: Rep[Double])(implicit n: Numeric[T], o: Overloaded2): Vec[T] = {
      SparseVector(nonZeroIndices zip nonZeroValues.map(v => Math.pow(v.toDouble, order).asRep[T]), length)
    }

    def reduce(implicit m: RepMonoid[T]): Rep[T] = items.reduce(m)  //TODO: it's inefficient

    def dot(other: Rep[Vector[T]])(implicit n: Numeric[T]): Rep[T] = {
      other match {
        case SparseVector(nonZeroItemsL, _) =>
          // TODO implement innerJoin and uncomment
          //sv => innerJoin(nonZeroIndices, nonZeroValues, sv.nonZeroIndices, sv.nonZeroValues).bs.reduce
          dotSparse(nonZeroIndices, nonZeroValues, nonZeroItemsL.as, nonZeroItemsL.bs)
        case _ =>
          other.dot(self)
      }
    }

    def euclideanNorm(implicit num: Numeric[T]): Rep[Double] = Math.sqrt(nonZeroValues.map(v => v * v).reduce.asRep[Double])

    def companion = SparseVector
  }

  trait VectorCompanion extends TypeFamily1[Vector]

  trait DenseVectorCompanion extends ConcreteClass1[Vector] with VectorCompanion
  trait ConstVectorCompanion extends ConcreteClass1[Vector] with VectorCompanion
  trait SparseVectorCompanion extends ConcreteClass1[Vector] with VectorCompanion {
    def apply[T: Elem](is: Coll[Int], vs: Coll[T], length: Rep[Int]): Vec[T] =
      SparseVector(PairCollectionSOA(is, vs), length)
    @OverloadId("separate_collections")
    def apply[T: Elem](nonZeroItems: Coll[(Int, T)], length: Rep[Int])(implicit o: Overloaded1): Vec[T] =
      SparseVector(PairCollectionAOS(nonZeroItems), length)
  }
}

trait VectorsDsl extends impl.VectorsAbs { self: LADsl =>

//  type VecCompanion = Rep[VectorCompanion]

  def dotSparse[T: Elem](xIndices: Coll[Int], xValues: Coll[T], yIndices: Coll[Int], yValues: Coll[T])
                        (implicit n: Numeric[T]): Rep[T]

  def dotMerge[T: Elem](xItems: Coll[T], yIndices: Coll[Int], yValues: Coll[T])
                       (implicit n: Numeric[T]): Rep[T] = {
    (xItems(yIndices) zip yValues).map { case Pair(x, y) => x * y }.reduce
  }

  /*def innerJoin[T: Elem](xIndices: Coll[Int], xValues: Coll[T], yIndices: Coll[Int], yValues: Coll[T],
                         f: Rep[((T, T)) => T])(implicit n: Numeric[T]): PairColl[Int, T] =
    (xIndices zip xValues).innerJoin(yIndices zip yValues, f)

  def outerJoin[T: Elem](xIndices: Coll[Int], xValues: Coll[T], yIndices: Coll[Int], yValues: Coll[T],
                         f: Rep[((T, T)) => T])(implicit n: Numeric[T]): PairColl[Int, T] =
     (xIndices zip xValues).outerJoin(yIndices zip yValues, f)*/

  def binarySearch(index: IntRep, indices: Coll[Int]): IntRep

  implicit class VectorExtensions[T](vector: Vec[T]) {
    implicit def eItem: Elem[T] = vector.selfType1.asInstanceOf[VectorElem[T, _]].eT

    def map[R: Elem](f: Rep[T] => Rep[R]): Vec[R] = vector.mapBy(fun(f))

    def filter(f: Rep[T] => Rep[Boolean]): Vec[T] = vector.filterBy(fun(f))
  }
}

trait VectorsDslStd extends impl.VectorsStd { self: LADslStd =>

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

  def binarySearch(index: IntRep, indices: Coll[Int]): IntRep = {
    val zero = 0
    val one = 1
    val two = 2
    def check(start: Int, end: Int): Int = {
      if (end - start < two) {
        if (index === indices(start)) start else {
          if (index === indices(end)) end else -1
        }
      } else {
        val middle = (start + end) div two
        if (index === indices(middle)) middle else {
          if (index < indices(middle)) check(start, middle - one) else check(middle + one, end)
        }
      }
    }
    check(zero, Math.min(index, indices.length - one))
  }
}

trait VectorsDslExp extends impl.VectorsExp { self: LADslExp =>
  def dotSparse[T: Elem](xIndices: Coll[Int], xValues: Coll[T], yIndices: Coll[Int], yValues: Coll[T])
                        (implicit n: Numeric[T]): Rep[T] = {
    DotSparse(xIndices.arr, xValues.arr, yIndices.arr, yValues.arr)
  }

  case class DotSparse[T](xIndices: Arr[Int], xValues: Arr[T], yIndices: Arr[Int], yValues: Arr[T])
                         (implicit val n: Numeric[T], selfType: Elem[T]) extends BaseDef[T]

  def binarySearch(index: IntRep, indices: Coll[Int]): IntRep = array_binary_search(index, indices.arr)
}
