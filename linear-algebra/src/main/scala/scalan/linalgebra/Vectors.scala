package scalan.linalgebra

/**
 * Created by Victor Smirnov on 3/12/15.
 */

import scalan._
import scalan.collections.{CollectionsDsl, CollectionsDslStd, CollectionsDslExp}
import scalan.common.OverloadHack.{Overloaded2, Overloaded1}
import scala.annotation.unchecked.uncheckedVariance

trait Vectors { self: LADsl =>

  type Vec[T] = Rep[Vector[T]]

  trait Vector[T] extends Def[Vector[T]] {

    def length: IntRep
    def items: Coll[T]
    def nonZeroIndices: Coll[Int]
    def nonZeroValues:  Coll[T]
    def nonZeroItems: Coll[(Int, T)]
    implicit def eT: Elem[T]
    def zero: Rep[T] = eT.defaultRepValue

    def apply(i: IntRep): Rep[T]
    @OverloadId("apply_by_collection")
    def apply(is: Coll[Int])(implicit o: Overloaded1): Vec[T]

    def filterBy(f: Rep[T @uncheckedVariance => Boolean]): Vec[T]

    def +^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T]
    @OverloadId("elementwise_sum_collection")
    def +^(coll: Coll[T])(implicit n: Numeric[T], o: Overloaded1): Vec[T] = self +^ DenseVector(coll)
    @OverloadId("elementwise_sum_value")
    def +^(value: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vec[T] = self +^ ConstVector(value, length)

    def -^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T]
    @OverloadId("elementwise_diff_collection")
    def -^(coll: Coll[T])(implicit n: Numeric[T], o: Overloaded1): Vec[T] = self -^ DenseVector(coll)
    @OverloadId("elementwise_diff_value")
    def -^(value: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vec[T] = self -^ ConstVector(value, length)

    def *^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T]
    @OverloadId("elementwise_mult_collection")
    def *^(coll: Coll[T])(implicit n: Numeric[T], o: Overloaded1): Vec[T] = self *^ DenseVector(coll)
    @OverloadId("elementwise_mult_value")
    def *^(value: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vec[T] = self *^ ConstVector(value, length)

    def /^(vector: Vec[T])(implicit f: Fractional[T]): Vec[T]
    @OverloadId("elementwise_div_collection")
    def /^(coll: Coll[T])(implicit f: Fractional[T], o: Overloaded1): Vec[T] = self /^ DenseVector(coll)
    @OverloadId("elementwise_div_value")
    def /^(value: Rep[T])(implicit f: Fractional[T], o: Overloaded2): Vec[T] = self /^ ConstVector(value, length)

    def *(matrix: Matr[T])(implicit n: Numeric[T]): Vec[T]

    def pow_^(order: Rep[Double])(implicit n: Numeric[T], o: Overloaded2): Vec[T]

    def euclideanNorm(implicit n: Numeric[T]): DoubleRep

    def sum(implicit n: Numeric[T]): Rep[T]
    def reduce(implicit m: RepMonoid[T]): Rep[T]
    def dot(vector: Vec[T])(implicit n: Numeric[T]): Rep[T]

    def nonZeroesLength: IntRep = nonZeroItems.length
  }

  abstract class DenseVector[T](val items: Coll[T])(implicit val eT: Elem[T])
    extends Vector[T] {

    def length = items.length
    def nonZeroIndices = nonZeroItems.as
    def nonZeroValues = items.filter(v => v !== zero)
    def nonZeroItems = (Collection.indexRange(length) zip items).filter { case Pair(i, v) => v !== zero }

    def apply(i: IntRep): Rep[T] = items(i)
    @OverloadId("apply_by_collection")
    def apply(is: Coll[Int])(implicit o: Overloaded1): Vec[T] = DenseVector(items(is))

    def filterBy(f: Rep[T @uncheckedVariance => Boolean]): Vec[T] = {
      val filtered = (Collection.indexRange(length) zip items).filter(p => f(p._2))
      SparseVectorBoxed(filtered, length)
    }

    def +^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      def updates = (items(vector.nonZeroIndices) zip vector.nonZeroValues).map { case Pair(v1, v2) => v1 + v2 }
      def updatedFlatItems = items.updateMany(vector.nonZeroIndices, updates)
      def shiftedFlatItems(cv: Rep[T]) = (self +^ cv).items.updateMany(vector.nonZeroIndices, updates)
      vector match {
        case DenseVector(_) => DenseVector((items zip vector.items).map { case Pair(v1, v2) => v1 + v2 })
        case ConstVector(cv, _) => DenseVector(items.map(v => v + cv))
        case ZeroVector(_) => self
        case SparseVector(_, _, _) => DenseVector(updatedFlatItems)
        case SparseVectorBoxed(_, _) => DenseVector(updatedFlatItems)
        case ShiftVector(_, _, cv, _) => DenseVector(shiftedFlatItems(cv))
        case ShiftVectorBoxed(_, cv, _) => DenseVector(shiftedFlatItems(cv))
        case _ => vector +^ self
        //case _ => !!!("matcher for @vector argument in DenseVector.+^(vector: Vec[T]) is not specified.")
      }
    }
    def -^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      def updates = (items(vector.nonZeroIndices) zip vector.nonZeroValues).map { case Pair(v1, v2) => v1 - v2 }
      def updatedFlatItems = items.updateMany(vector.nonZeroIndices, updates)
      def shiftedFlatItems(cv: Rep[T]) = (self +^ cv).items.updateMany(vector.nonZeroIndices, updates)
      vector match {
        case DenseVector(_) => DenseVector((items zip vector.items).map { case Pair(v1, v2) => v1 - v2 })
        case ConstVector(cv, _) => DenseVector(items.map(v => v - cv))
        case ZeroVector(_) => self
        case SparseVector(_, _, _) => DenseVector(updatedFlatItems)
        case SparseVectorBoxed(_, _) => DenseVector(updatedFlatItems)
        case ShiftVector(_, _, cv, _) => DenseVector(shiftedFlatItems(cv))
        case ShiftVectorBoxed(_, cv, _) => DenseVector(shiftedFlatItems(cv))
        case _ => vector *^ (-toRep(n.one)) +^ self
        //case _ => !!!("matcher for @vector argument in DenseVector.-^(vector: Vec[T]) is not specified.")
      }
    }
    def *^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      lazy val (is, vs) = (vector.nonZeroIndices, vector.nonZeroValues)
      def newValues = (items(is) zip vs).map { case Pair(v1, v2) => v1 * v2 }
      def newItems = (vector.nonZeroItems zip items(is)).map { case Pair(Pair(i, v1), v2) => (i, v1 * v2) }
      def shiftedFlatItems(cv: Rep[T]) = (self *^ cv).items.updateMany(is, newValues)
      vector match {
        case DenseVector(_) => DenseVector((items zip vector.items).map { case Pair(v1, v2) => v1 * v2 })
        case ConstVector(cv, _) => DenseVector(items.map(v => v * cv))
        case ZeroVector(_) => vector
        case SparseVector(_, _, _) => SparseVector(is, newValues, length)
        case SparseVectorBoxed(_, _) => SparseVectorBoxed(newItems, length)
        case ShiftVector(_, _, cv, _) => DenseVector(shiftedFlatItems(cv))
        case ShiftVectorBoxed(_, cv, _) => DenseVector(shiftedFlatItems(cv))
        case _ => vector *^ self
        //case _ => !!!("matcher for @vector argument in DenseVector.*^(vector: Vec[T]) is not specified.")
      }
    }
    def /^(vector: Vec[T])(implicit f: Fractional[T]): Vec[T] = {
      lazy val (is, vs) = (vector.nonZeroIndices, vector.nonZeroValues)
      def updates = (items(is) zip vs).map { case Pair(v1, v2) => v1 / v2 }
      def updatedFlatItems = (items zip vector.items).map { case Pair(v1, v2) => v1 / v2 }
      def shiftedFlatItems(cv: Rep[T]) = (self /^ cv).items.updateMany(is, updates)
      vector match {
        case DenseVector(_) => DenseVector(updatedFlatItems)
        case ConstVector(value, _) => DenseVector(items.map(v => v / value))
        case ZeroVector(_) => !!!("attempt to divide DenseVector by ZeroVector.")
        case SparseVector(_, _, _) => DenseVector(updatedFlatItems)
        case SparseVectorBoxed(_, _) => DenseVector(updatedFlatItems)
        case ShiftVector(_, _, cv, _) => DenseVector(shiftedFlatItems(cv))
        case ShiftVectorBoxed(_, cv, _) => DenseVector(shiftedFlatItems(cv))
        case _ => !!!("matcher for @vector argument in DenseVector./^(vector: Vec[T]) is not specified.")
      }
    }
    def dot(vector: Vec[T])(implicit n: Numeric[T]): Rep[T] = {
      def const(cv: Rep[T]) = ConstVector(cv, length)
      def sparse(cv: Rep[T]) = SparseVector(vector.nonZeroIndices, vector.nonZeroValues.map(v => v - cv), length)
      def sparseBoxed(cv: Rep[T]) = SparseVectorBoxed(vector.nonZeroItems.map(v => (v._1, v._2 - cv)), length)
      vector match {
        case ConstVector(cv, _) => self.sum * cv
        case ZeroVector(_) => zero
        case ShiftVector(_, _, cv, _) => (self dot sparse(cv)) + (self dot const(cv))
        case ShiftVectorBoxed(_, cv, _) => (self dot sparseBoxed(cv)) + (self dot const(cv))
        case _ => (self *^ vector).sum
      }
    }
    def *(matrix: Matr[T])(implicit n: Numeric[T]): Vec[T] = {
      def mV = CompoundMatrix(items.map(v => ConstVector(v, matrix.numRows)), matrix.numColumns)
      def standart = (matrix *^^ mV).sumByColumns
      matrix match {
        case DenseFlatMatrix(_, _) => standart
        case CompoundMatrix(_, _) => standart
        case ConstMatrix(value, _, _) => ConstVector(sum * value, length)
        case DiagonalMatrix(diagonalValues) => self *^ DenseVector(diagonalValues)
        case ConstDiagonalMatrix(diagonalValue, _) => self *^ diagonalValue
        case _ => standart
        //case _ => !!!("matcher for @matrix argument in DenseFlatMatrix.*(matrix: Matr[T]) is not specified.")
      }
    }

    def pow_^(order: DoubleRep)(implicit n: Numeric[T], o: Overloaded2): Vec[T] = {
      // TODO: match element[T]
      DenseVector(items.map(v => Math.pow(v.toDouble, order).asRep[T]))
    }

    def sum(implicit n: Numeric[T]): Rep[T] = reduce
    def reduce(implicit m: RepMonoid[T]): Rep[T] = items.reduce(m)

    def euclideanNorm(implicit n: Numeric[T]): DoubleRep = Math.sqrt(items.map(v => v * v).reduce.toDouble)
  }

  /*trait ConstantVector[T] extends Vector[T] {

    implicit def eT: Elem[T]

    def const: Rep[T]
    def length: IntRep
  }*/

    abstract class ConstVector[T](val const: Rep[T], val length: IntRep)(implicit val eT: Elem[T])
      extends Vector[T] {

      def items: Coll[T] = Collection.replicate(length, const)
      def nonZeroIndices: Coll[Int] = IF (const !== zero) THEN Collection.indexRange(length) ELSE Collection.empty[Int]
      def nonZeroValues:  Coll[T] =  IF (const !== zero) THEN items ELSE Collection.empty[T]
      def nonZeroItems:   Coll[(Int, T)] = nonZeroIndices zip nonZeroValues

      def apply(i: IntRep): Rep[T] = const
      @OverloadId("apply_by_collection")
      def apply(is: Coll[Int])(implicit o: Overloaded1): Vec[T] = ConstVector(const, is.length)

      def filterBy(f: Rep[T @uncheckedVariance => Boolean]): Vec[T] = IF (f(const)) THEN ConstVector(const, length) ELSE ConstVector(const, 0)

      def +^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
        def newValues = vector.nonZeroValues.map(v => v + const)
        def newItems = vector.nonZeroItems.map { case Pair(i, v) => (i, v + const) }
        vector match {
          case ConstVector(cv, _) => ConstVector(const + cv, length)
          case SparseVector(is, _, _) => ShiftVector(is, newValues, const, length)
          case SparseVectorBoxed(_, _) => ShiftVectorBoxed(newItems, const, length)
          case ShiftVector(is, _, cv, _) => ShiftVector(is, newValues, const + cv, length)
          case ShiftVectorBoxed(_, cv, _) => ShiftVectorBoxed(newItems, const + cv, length)
          case _ => vector +^ self
        }
      }
      def -^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
        def newValues = vector.nonZeroValues.map(v => const - v)
        def newItems = vector.nonZeroItems.map { case Pair(i, v) => (i, const - v) }
        vector match {
          case DenseVector(vs) => DenseVector(vs.map(v => const - v))
          case ConstVector(cv, _) => ConstVector(const - cv, length)
          case ZeroVector(_) => self
          case SparseVector(is, _, _) => ShiftVector(is, newValues, const, length)
          case SparseVectorBoxed(_, _) => ShiftVectorBoxed(newItems, const, length)
          case ShiftVector(is, _, cv, _) => ShiftVector(is, newValues, const - cv, length)
          case ShiftVectorBoxed(_, cv, _) => ShiftVectorBoxed(newItems, const - cv, length)
          case _ => vector *^ (-toRep(n.one)) +^ self
          //case _ => !!!("matcher for @vector argument in ConstVector.-^(vector: Vec[T]) is not specified.")
        }
      }
      def *^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
        def newValues = vector.nonZeroValues.map(v => v * const)
        def newItems = vector.nonZeroItems.map { case Pair(i, v) => (i, v * const) }
        vector match {
          case ConstVector(cv, _) => ConstVector(const * cv, length)
          case ZeroVector(_) => vector
          case SparseVector(is, _, _) => SparseVector(is, newValues, length)
          case SparseVectorBoxed(_, _) => SparseVectorBoxed(newItems, length)
          case ShiftVector(is, _, cv, _) => ShiftVector(is, newValues, const * cv, length)
          case ShiftVectorBoxed(_, cv, _) => ShiftVectorBoxed(newItems, const * cv, length)
          case _ => vector *^ self
        }
      }
      def /^(vector: Vec[T])(implicit f: Fractional[T]): Vec[T] = {
        def singular: Vec[T] = ConstVector(toRep(f.one), length) // TODO: in case x -> 0: x/x -> 1
        def flatItems = vector.items.map(v => const / v)
        def newItems = vector.nonZeroItems.map { case Pair(i, v) => (i, const / v) }
        vector match {
          case DenseVector(_) => DenseVector(flatItems)
          case ConstVector(cv, _) => IF (const === cv) THEN singular ELSE ConstVector(const / cv, length)
          case ZeroVector(_) => IF (const === zero) THEN singular ELSE ConstVector(const / zero, length)
          case SparseVector(_, _, _) => DenseVector(flatItems)
          case SparseVectorBoxed(_, _) => DenseVector(flatItems)
          case ShiftVector(is, vs, cv, _) => ShiftVector(is, vs.map(v => const / v), const / cv, length)
          case ShiftVectorBoxed(_, cv, _) => ShiftVectorBoxed(newItems, const / cv, length)
          case _ => DenseVector(flatItems)
          //case _ => !!!("matcher for @vector argument in ConstVector./^(vector: Vec[T]) is not specified.")
        }
      }
      def dot(vector: Vec[T])(implicit n: Numeric[T]): Rep[T] = {
        vector match {
          case ConstVector(cv, _) => const * cv * length.to[T]
          case ZeroVector(_) => zero
          case _ => const * vector.sum
        }
      }
      def *(matrix: Matr[T])(implicit n: Numeric[T]): Vec[T] = ???

      def pow_^(order: DoubleRep)(implicit n: Numeric[T], o: Overloaded2): Vec[T] = {
        ConstVector(Math.pow(const.toDouble, order).asRep[T], length)
      }
      def sum(implicit n: Numeric[T]): Rep[T] = const * length.to[T]
      // TODO: need some monoid matching or optimization rule
      def reduce(implicit m: RepMonoid[T]): Rep[T] = items.reduce(m)

      def euclideanNorm(implicit n: Numeric[T]): DoubleRep = Math.sqrt(items.map(v => v * v).reduce.toDouble)
    }

    abstract class ZeroVector[T](val length: IntRep)(implicit val eT: Elem[T])
      extends Vector[T] {

      def items: Coll[T] = Collection.replicate(length, zero)
      def nonZeroIndices: Coll[Int] = Collection.empty[Int]
      def nonZeroValues:  Coll[T] =  Collection.empty[T]
      def nonZeroItems:   Coll[(Int, T)] = Collection.empty[(Int, T)]

      def apply(i: IntRep) = zero
      @OverloadId("apply_by_collection")
      def apply(is: Coll[Int])(implicit o: Overloaded1): Vec[T] = ZeroVector(is.length)

      def filterBy(f: Rep[T @uncheckedVariance => Boolean]): Vec[T] = self

      def +^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = vector
      def -^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
        def newValues = vector.nonZeroValues.map(v => -v)
        def newItems = vector.nonZeroItems.map { case Pair(i, v) => (i, -v) }
        vector match {
          case DenseVector(vs) => DenseVector(vs.map(v => -v))
          case ConstVector(cv, _) => ConstVector(-cv, length)
          case SparseVector(is, _, _) => SparseVector(is, newValues, length)
          case SparseVectorBoxed(_, _) => SparseVectorBoxed(newItems, length)
          case ShiftVector(is, _, cv, _) => ShiftVector(is, newValues, -cv, length)
          case ShiftVectorBoxed(_, cv, _) => ShiftVectorBoxed(newItems, -cv, length)
          case _ => vector *^ (-toRep(n.one))
          //case _ => !!!("matcher for @vector argument in ConstVector.-^(vector: Vec[T]) is not specified.")
        }
      }
      def *^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = self
      def /^(vector: Vec[T])(implicit f: Fractional[T]): Vec[T] = {
        def one = toRep(f.one)
        def singular: Vec[T] = ConstVector(one, length) // TODO: in case x -> 0: x/x -> 1
        def newItems = vector.nonZeroItems.map { case Pair(i, v) => (i, IF (v === zero) THEN one ELSE zero) }
        def filter0 = newItems.filter(p => p._2 !== zero)
        def filter1 = newItems.filter(p => p._2 !== one)
        def shift: Vec[T] = ShiftVectorBoxed(filter1, one, length)
        def sparse: Vec[T] = SparseVectorBoxed(filter0, length)
        def shiftOrZero = IF (filter1.length > toRep(0)) THEN shift ELSE singular
        def sparseOrZero = IF (filter0.length > toRep(0)) THEN sparse ELSE self
        vector match {
          case ZeroVector(_) => singular
          case ConstVector(cv, _) => IF (cv === zero) THEN singular ELSE self
          case SparseVector(_, _, _) => shiftOrZero
          case SparseVectorBoxed(_, _) => shiftOrZero
          case ShiftVector(_, _, cv, _) => IF (cv === zero) THEN shiftOrZero ELSE sparseOrZero
          case ShiftVectorBoxed(_, cv, _) => IF (cv === zero) THEN shiftOrZero ELSE sparseOrZero
          case _ => self // TODO: check it
        }
      }
      def dot(vector: Vec[T])(implicit n: Numeric[T]): Rep[T] = zero
      def *(matrix: Matr[T])(implicit n: Numeric[T]): Vec[T] = ZeroVector(matrix.numColumns)

      // TODO: we ignore @order <= 0.0
      def pow_^(order: DoubleRep)(implicit n: Numeric[T], o: Overloaded2): Vec[T] = self
      def sum(implicit n: Numeric[T]): Rep[T] = zero
      // TODO: need some monoid matching or optimization rule
      def reduce(implicit m: RepMonoid[T]): Rep[T] = items.reduce(m)

      def euclideanNorm(implicit n: Numeric[T]): DoubleRep = zero.toDouble
    }

  abstract class SparseVector[T](val nonZeroIndices: Coll[Int], val nonZeroValues: Coll[T],
                                 val length: IntRep)(implicit val eT: Elem[T])
    extends Vector[T] {

    def items: Coll[T] = Collection.replicate(length, zero).updateMany(nonZeroIndices, nonZeroValues)
    def nonZeroItems: Coll[(Int, T)] = nonZeroIndices zip nonZeroValues

    def apply(i: IntRep): Rep[T] = {
      val zeroInt = toRep(0)
      IF (nonZeroIndices.length > zeroInt) THEN {
        val k = binarySearch(i, nonZeroIndices)
        IF (k >= zeroInt) THEN nonZeroValues(k) ELSE zero
      } ELSE zero
    }

    @OverloadId("apply_by_collection")
    def apply(is: Coll[Int])(implicit o: Overloaded1): Vec[T] = ??? // TODO: need efficient binarySearchMany

    def filterBy(f: Rep[T @uncheckedVariance => Boolean]): Vec[T] = {
      // EDIT: NOT TODO: need to consider f(zeroValue) === FALSE
      // EDIT: we don't need to consider it, as vector.filter works by its definition on NONZERO items only
      val filteredItems = nonZeroItems.filter { v => f(v._2) }
      SparseVector(filteredItems.as, filteredItems.bs, length)
    }

    def +^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      val (x1, x2) = (nonZeroItems, vector.nonZeroItems)
      def newItems = x1 outerSum x2
      def shiftedItems(cv: Rep[T]) = (x1 outerSum x2.map(v => (v._1, v._2 - cv))).map(v => (v._1, v._2 + cv))
      vector match {
        case SparseVector(_, _, _) => SparseVectorBoxed(newItems, length)
        case SparseVectorBoxed(_, _) => SparseVectorBoxed(newItems, length)
        case ShiftVector(_, _, cv, _) => ShiftVectorBoxed(shiftedItems(cv), cv, length)
        case ShiftVectorBoxed(_, cv, _) => ShiftVectorBoxed(shiftedItems(cv), cv, length)
        case _ => vector +^ self
      }
    }
    def -^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      val (is, vs, x1, x2) = (nonZeroIndices, nonZeroValues, nonZeroItems, vector.nonZeroItems)
      def newItems = x1 outerSubtr x2
      def shiftedItems(cv: Rep[T]) = (x1 outerSubtr x2.map(v => (v._1, v._2 - cv))).map(v => (v._1, v._2 - cv))
      vector match {
        case DenseVector(vs2) => DenseVector(vs2.map(-_).updateMany(is, (vs zip vs2(is)).map(v => v._1 - v._2)))
        case ConstVector(cv, _) => ShiftVector(is, vs.map(v => v - cv), -cv, length)
        case ZeroVector(_) => self
        case SparseVector(_, _, _) => SparseVectorBoxed(newItems, length)
        case SparseVectorBoxed(_, _) => SparseVectorBoxed(newItems, length)
        case ShiftVector(_, _, cv, _) => ShiftVectorBoxed(shiftedItems(cv), -cv, length)
        case ShiftVectorBoxed(_, cv, _) => ShiftVectorBoxed(shiftedItems(cv), -cv, length)
        case _ => vector *^ (-toRep(n.one)) +^ self
        //case _ => !!!("matcher for @vector argument in SparseVector.-^(vector: Vec[T]) is not specified.")
      }
    }
    def *^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      lazy val (x1, x2) = (nonZeroItems, vector.nonZeroItems)
      def newItems = x1 innerMult x2
      def shifted(cv: Rep[T]) = (x1 innerMult x2.map(v => (v._1, v._2 - cv))) outerSum x1.map(v => (v._1, v._2 * cv))
      vector match {
        case SparseVector(_, _, _) => SparseVector(newItems.as, newItems.bs, length)
        case SparseVectorBoxed(_, _) => SparseVectorBoxed(newItems, length)
        case ShiftVector(_, _, cv, _) => SparseVector(shifted(cv).as, shifted(cv).bs, length)
        case ShiftVectorBoxed(_, cv, _) => SparseVectorBoxed(shifted(cv), length)
        case _ => vector *^ self
      }
    }
    def /^(vector: Vec[T])(implicit f: Fractional[T]): Vec[T] = {
      lazy val (is, vs) = (nonZeroIndices, nonZeroValues)
      def newValues = (vs zip vector.items(is)).map { case Pair(v1, v2) => v1 / v2 }
      // TODO: ShiftVectors can be more optimal in this case
      vector match {
        case DenseVector(_) => SparseVector(is, newValues, length)
        case ConstVector(cv, _) => SparseVector(is, vs.map(v => v / cv), length)
        case ZeroVector(_) => !!!("attempt to divide SparseVector by ZeroVector.")
        case SparseVector(_, _, _) => SparseVector(is, newValues, length)
        case SparseVectorBoxed(_, _) => SparseVector(is, newValues, length)
        case ShiftVector(_, _, _, _) => SparseVector(is, newValues, length)
        case ShiftVectorBoxed(_, _, _) => SparseVector(is, newValues, length)
        case _ => !!!("matcher for @vector argument in SparseVector./^(vector: Vec[T]) is not specified.")
      }
    }
    def dot(vector: Vec[T])(implicit n: Numeric[T]): Rep[T] = {
      def standart = (self *^ vector).sum
      def sparse(cv: Rep[T]) = SparseVector(vector.nonZeroIndices, vector.nonZeroValues.map(v => v - cv), vector.length)
      def sparseBoxed(cv: Rep[T]) = SparseVectorBoxed(vector.nonZeroItems.map(v => (v._1, v._2 - cv)), vector.length)
      vector match {
        case SparseVector(_, _, _) => standart
        case SparseVectorBoxed(_, _) => standart
        case ShiftVector(_, _, cv, _) => (self dot sparse(cv)) + self.sum * cv
        case ShiftVectorBoxed(_, cv, _) => (self dot sparseBoxed(cv)) + self.sum * cv
        case _ => vector dot self
      }
    }
    def *(matrix: Matr[T])(implicit n: Numeric[T]): Vec[T] = {
      def mV = CompoundMatrix(nonZeroValues.map(v => ConstVector(v, matrix.numRows)), matrix.numColumns)
      def standart: Vec[T] = (matrix(nonZeroIndices) *^^ mV).sumByColumns
      matrix match {
        case DenseFlatMatrix(_, _) => standart
        case CompoundMatrix(_, _) => standart
        case ConstMatrix(value, _, _) => ConstVector(sum * value, length)
        case DiagonalMatrix(diagonalValues) => self *^ DenseVector(diagonalValues)
        case ConstDiagonalMatrix(diagonalValue, _) => self *^ diagonalValue
        case _ => standart
        //case _ => !!!("matcher for @matrix argument in DenseFlatMatrix.*(matrix: Matr[T]) is not specified.")
      }
    }

    def pow_^(order: DoubleRep)(implicit n: Numeric[T], o: Overloaded2): Vec[T] = {
      SparseVector(nonZeroIndices, nonZeroValues.map(v => Math.pow(v.toDouble, order).asRep[T]), length)
    }

    def sum(implicit n: Numeric[T]): Rep[T] = nonZeroValues.reduce
    def reduce(implicit m: RepMonoid[T]): Rep[T] = items.reduce(m)  //TODO: it's inefficient

    def euclideanNorm(implicit n: Numeric[T]): DoubleRep = Math.sqrt(nonZeroValues.map(v => v * v).reduce.toDouble)
  }

  abstract class SparseVectorBoxed[T](val nonZeroItems: Coll[(Int, T)], val length: IntRep)(implicit val eT: Elem[T])
    extends Vector[T] {

    def items: Coll[T] = Collection.replicate(length, zero).updateMany(nonZeroIndices, nonZeroValues)
    def nonZeroIndices: Coll[Int] = nonZeroItems.as
    def nonZeroValues: Coll[T] = nonZeroItems.bs

    def apply(i: IntRep): Rep[T] = {
      val zeroInt = toRep(0)
      IF (nonZeroIndices.length > zeroInt) THEN {
        val k = binarySearch(i, nonZeroIndices)
        IF (k >= zeroInt) THEN nonZeroValues(k) ELSE zero
      } ELSE zero
    }

    @OverloadId("apply_by_collection")
    def apply(is: Coll[Int])(implicit o: Overloaded1): Vec[T] = ??? // TODO: need efficient way to get value by index

    def filterBy(f: Rep[T @uncheckedVariance => Boolean]): Vec[T] = {
      val filteredItems = nonZeroItems.filter { v => f(v._2) }
      SparseVectorBoxed(filteredItems, length)
    }

    def +^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      lazy val (x1, x2) = (nonZeroItems, vector.nonZeroItems)
      def shiftedItems(cv: Rep[T]) = (x1 outerSum x2.map(v => (v._1, v._2 - cv))).map(v => (v._1, v._2 + cv))
      vector match {
        case SparseVectorBoxed(_, _) => SparseVectorBoxed(x1 outerSum x2, length)
        case ShiftVector(_, _, cv, _) => ShiftVectorBoxed(shiftedItems(cv), cv, length)
        case ShiftVectorBoxed(_, cv, _) => ShiftVectorBoxed(shiftedItems(cv), cv, length)
        case _ => vector +^ self
      }
    }
    def -^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      lazy val (is, vs) = (nonZeroIndices, nonZeroValues)
      lazy val (x1, x2) = (nonZeroItems, vector.nonZeroItems)
      def newItems = x1 outerSubtr x2
      def shiftedItems(cv: Rep[T]) = x1 outerSubtr x2.map(v => (v._1, v._2 - cv))
      def shift(cv: Rep[T]) = SparseVectorBoxed(shiftedItems(cv), length) -^ ConstVector(cv, length)
      vector match {
        case DenseVector(vs2) => DenseVector(vs2.map(-_).updateMany(is, (vs zip vs2(is)).map(v => v._1 - v._2)))
        case ConstVector(cv, _) => ShiftVectorBoxed(x1.map(v => (v._1, v._2 - cv)), -cv, length)
        case ZeroVector(_) => self
        case SparseVector(_, _, _) => SparseVectorBoxed(newItems, length)
        case SparseVectorBoxed(_, _) => SparseVectorBoxed(newItems, length)
        case ShiftVector(_, _, cv, _) => shift(cv)
        case ShiftVectorBoxed(_, cv, _) => shift(cv)
        case _ => vector *^ (-toRep(n.one)) +^ self
        //case _ => !!!("matcher for @vector argument in SparseVectorBoxed.-^(vector: Vec[T]) is not specified.")
      }
    }
    def *^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      lazy val (x1, x2) = (nonZeroItems, vector.nonZeroItems)
      def newItems = x1 innerMult x2
      def shiftedItems(cv: Rep[T]) = (x1 innerMult x2.map(v => (v._1, v._2 - cv))) outerSum x1.map(v => (v._1, v._2 * cv))
      vector match {
        case SparseVector(_, _, _) => SparseVectorBoxed(newItems, length)
        case SparseVectorBoxed(_, _) => SparseVectorBoxed(newItems, length)
        case ShiftVector(_, _, cv, _) => SparseVectorBoxed(shiftedItems(cv), length)
        case ShiftVectorBoxed(_, cv, _) => SparseVectorBoxed(shiftedItems(cv), length)
        case _ => vector *^ self
      }
    }
    def /^(vector: Vec[T])(implicit f: Fractional[T]): Vec[T] = {
      lazy val (is, vs) = (nonZeroIndices, nonZeroValues)
      def newValues = (vs zip vector.items(is)).map { case Pair(v1, v2) => v1 / v2 }
      // TODO: ShiftVectors can be more optimal in this case
      vector match {
        case DenseVector(_) => SparseVector(is, newValues, length)
        case ConstVector(cv, _) => SparseVector(is, vs.map(v => v / cv), length)
        case ZeroVector(_) => !!!("attempt to divide SparseVectorBoxed by ZeroVector.")
        case SparseVector(_, _, _) => SparseVector(is, newValues, length)
        case SparseVectorBoxed(_, _) => SparseVector(is, newValues, length)
        case ShiftVector(_, _, cv, _) => SparseVector(is, newValues, length)
        case ShiftVectorBoxed(_, cv, _) => SparseVector(is, newValues, length)
          // TODO: check case _
        case _ => !!!("matcher for @vector argument in SparseVector./^(vector: Vec[T]) is not specified.")
      }
    }
    def dot(vector: Vec[T])(implicit n: Numeric[T]): Rep[T] = {
      lazy val len = vector.length
      def sparse(cv: Rep[T]) = SparseVector(vector.nonZeroIndices, vector.nonZeroValues.map(v => v - cv), len)
      def sparseBoxed(cv: Rep[T]) = SparseVectorBoxed(vector.nonZeroItems.map(v => (v._1, v._2 - cv)), len)
      vector match {
        case SparseVector(_, _, _) => (self *^ vector).sum
        case SparseVectorBoxed(_, _) => (self *^ vector).sum
        case ShiftVector(_, _, cv, _) => (self dot sparse(cv)) + sum * cv
        case ShiftVectorBoxed(_, cv, _) => (self dot sparseBoxed(cv)) + sum * cv
        case _ => vector dot self
      }
    }
    def *(matrix: Matr[T])(implicit n: Numeric[T]): Vec[T] = ???

    def pow_^(order: DoubleRep)(implicit n: Numeric[T], o: Overloaded2): Vec[T] = {
      SparseVectorBoxed(nonZeroIndices zip nonZeroValues.map(v => Math.pow(v.toDouble, order).asRep[T]), length)
    }

    def sum(implicit n: Numeric[T]): Rep[T] = nonZeroValues.reduce
    def reduce(implicit m: RepMonoid[T]): Rep[T] = items.reduce(m)  //TODO: it's inefficient

    def euclideanNorm(implicit n: Numeric[T]): DoubleRep = Math.sqrt(nonZeroValues.map(v => v * v).reduce.toDouble)
  }

  abstract class ShiftVector[T](val nonZeroIndices: Coll[Int], val nonZeroValues: Coll[T],
                                val constItem: Rep[T], val length: IntRep)(implicit val eT: Elem[T])
    extends Vector[T] {

    def items: Coll[T] = {
      Collection.replicate(length, constItem).updateMany(nonZeroIndices, nonZeroValues)
    }
    def nonZeroItems: Coll[(Int, T)] = nonZeroIndices zip nonZeroValues

    def apply(i: IntRep): Rep[T] = {
      val zero = toRep(0)
      IF (nonZeroIndices.length > zero) THEN {
        val k = binarySearch(i, nonZeroIndices)
        IF (k >= zero) THEN nonZeroValues(k) ELSE constItem
      } ELSE constItem
    }

    @OverloadId("apply_by_collection")
    def apply(is: Coll[Int])(implicit o: Overloaded1): Vec[T] = ??? // TODO: need efficient binarySearchMany

    def filterBy(f: Rep[T @uncheckedVariance => Boolean]): Vec[T] = {
      val filteredItems = nonZeroItems.filter { v => f(v._2) }
      SparseVector(filteredItems.as, filteredItems.bs, length)
    }

    def +^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      def x1 = nonZeroIndices zip nonZeroValues.map(v => v - constItem)
      def x2(cv: Rep[T]) = vector.nonZeroItems.map(v => (v._1, v._2 - cv))
      def shiftedItems(cv: Rep[T]) = (x1 outerSum x2(cv)).map(v => (v._1, v._2 + constItem + cv))
      vector match {
        case ShiftVector(_, _, cv, _) => ShiftVectorBoxed(shiftedItems(cv), constItem + cv, length)
        case ShiftVectorBoxed(_, cv, _) => ShiftVectorBoxed(shiftedItems(cv), constItem + cv, length)
        case _ => vector +^ self
      }
    }
    def -^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      def x1 = nonZeroItems.map(v => (v._1, v._2 - constItem))
      def x2(c2: Rep[T]) = vector.nonZeroItems.map(v => (v._1, v._2 - c2))
      def updates = (nonZeroValues zip vector.items(nonZeroIndices)).map { case Pair(v1, v2) => v1 - v2 }
      def newItems = (x1 outerSubtr vector.nonZeroItems).map(v => (v._1, v._2 + constItem))
      def shiftedItems(c2: Rep[T]) = (x1 outerSubtr x2(c2)).map(v => (v._1, v._2 + constItem - c2))
      vector match {
        case DenseVector(_) => DenseVector(vector.items.map(v => constItem - v).updateMany(nonZeroIndices, updates))
        case ConstVector(c2, _) => ShiftVector(nonZeroIndices, nonZeroValues.map(v => v - c2), constItem - c2, length)
        case ZeroVector(_) => self
        case SparseVector(_, _, _) => ShiftVectorBoxed(newItems, constItem, length)
        case SparseVectorBoxed(_, _) => ShiftVectorBoxed(newItems, constItem, length)
        case ShiftVector(_, _, c2, _) => ShiftVectorBoxed(shiftedItems(c2), constItem - c2, length)
        case ShiftVectorBoxed(_, c2, _) => ShiftVectorBoxed(shiftedItems(c2), constItem - c2, length)
        case _ => vector *^ (-toRep(n.one)) +^ self
        //case _ => !!!("matcher for @vector argument in ShiftVector.-^(vector: Vec[T]) is not specified.")
      }
    }
    def *^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      def x1 = nonZeroItems.map(v => (v._1, v._2 - constItem))
      def x2(cv: Rep[T]) = vector.nonZeroItems.map(v => (v._1, v._2 - cv))
      def x1_c2(cv: Rep[T]) = x1.map(v => (v._1, v._2 * cv))
      def x2_c1(cv: Rep[T]) = x2(cv).map(v => (v._1, v._2 * constItem))
      def newItems(cv: Rep[T]) = (x1 innerMult x2(cv)) outerSum x1_c2(cv) outerSum x2_c1(cv)
      def shiftedItems(c2: Rep[T]) = newItems(c2).map(v => (v._1, v._2 + constItem * c2))
      vector match {
        case ShiftVector(_, _, cv, _) => ShiftVectorBoxed(shiftedItems(cv), constItem * cv, length)
        case ShiftVectorBoxed(_, cv, _) => ShiftVectorBoxed(shiftedItems(cv), constItem * cv, length)
        case _ => vector *^ self
      }
    }
    def /^(vector: Vec[T])(implicit f: Fractional[T]): Vec[T] = {
      lazy val (is, vs, c1) = (nonZeroIndices, nonZeroValues, constItem)
      def updates = (vs zip vector.items(is)).map { case Pair(v1, v2) => v1 / v2 }
      def shiftedItems = vector.items.map(v => c1 / v).updateMany(is, updates)
      vector match {
        case DenseVector(_) => DenseVector(shiftedItems)
        case ConstVector(c2, _) => ShiftVector(is, vs.map(v => v / c2), c1 / c2, length)
        case ZeroVector(_) => !!!("attempt to divide ShiftVector by ZeroVector.")
        case SparseVector(_, _, _) => DenseVector(shiftedItems)
        case SparseVectorBoxed(_, _) => DenseVector(shiftedItems)
        case ShiftVector(_, _, _, _) => DenseVector(shiftedItems)
        case ShiftVectorBoxed(_, _, _) => DenseVector(shiftedItems)
          // TODO: check case _
        case _ => !!!("matcher for @vector argument in ShiftVector./^(vector: Vec[T]) is not specified.")
      }
    }
    def dot(vector: Vec[T])(implicit n: Numeric[T]): Rep[T] = {
      def sparseBoxed(cv: Rep[T]) = SparseVectorBoxed(vector.nonZeroItems.map(v => (v._1, v._2 - cv)), vector.length)
      def shift(cv: Rep[T]) = self.sum * cv
      vector match {
        case ShiftVector(is, vs, cv, l) => (self dot SparseVector(is, vs.map(v => v - cv), l)) + shift(cv)
        case ShiftVectorBoxed(_, cv, _) => (self dot sparseBoxed(cv)) + shift(cv)
        case _ => vector dot self
      }
    }
    def *(matrix: Matr[T])(implicit n: Numeric[T]): Vec[T] = ???

    def pow_^(order: DoubleRep)(implicit n: Numeric[T], o: Overloaded2): Vec[T] = {
      SparseVector(nonZeroIndices, nonZeroValues.map(v => Math.pow(v.toDouble, order).asRep[T]), length)
    }

    def sum(implicit n: Numeric[T]): Rep[T] = nonZeroValues.reduce + (length - nonZeroValues.length).to[T] * constItem
    def reduce(implicit m: RepMonoid[T]): Rep[T] = items.reduce(m)  //TODO: it's inefficient

    def euclideanNorm(implicit n: Numeric[T]): DoubleRep = Math.sqrt(nonZeroValues.map(v => v * v).reduce.toDouble)
  }

  abstract class ShiftVectorBoxed[T](val nonZeroItems: Coll[(Int, T)], val offset: Rep[T],
                                     val length: IntRep)(implicit val eT: Elem[T])
    extends Vector[T] {

    def items = Collection.replicate(length, offset).updateMany(nonZeroIndices, nonZeroValues)
    def nonZeroIndices = nonZeroItems.as
    def nonZeroValues = nonZeroItems.bs

    def apply(i: IntRep) = {
      val zeroInt = toRep(0)
      IF (nonZeroIndices.length > zeroInt) THEN {
        val k = binarySearch(i, nonZeroIndices)
        IF (k >= zeroInt) THEN nonZeroValues(k) ELSE zero
      } ELSE zero
    }

    @OverloadId("apply_by_collection")
    def apply(is: Coll[Int])(implicit o: Overloaded1): Vec[T] = ??? // TODO: need efficient way to get value by index

    def filterBy(f: Rep[T @uncheckedVariance => Boolean]): Vec[T] = {
      val filteredItems = nonZeroItems.filter { v => f(v._2) }
      SparseVectorBoxed(filteredItems, length)
    }

    def +^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      def x1 = nonZeroItems.map(v => (v._1, v._2 - offset))
      def x2(cv: Rep[T]) = vector.nonZeroItems.map(v => (v._1, v._2 - cv))
      def shiftedItems(cv: Rep[T]) = (x1 outerSum x2(cv)).map(v => (v._1, v._2 + offset + cv))
      vector match {
        case ShiftVectorBoxed(_, cv, _) => ShiftVectorBoxed(shiftedItems(cv), offset + cv, length)
        case _ => vector +^ self
      }
    }
    def -^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      def x1 = nonZeroItems.map(v => (v._1, v._2 - offset))
      def x2(cv: Rep[T]) = vector.nonZeroItems.map(v => (v._1, v._2 - cv))
      def updates = (nonZeroValues zip vector.items(nonZeroIndices)).map { case Pair(v1, v2) => v1 - v2 }
      def newItems = (x1 outerSubtr vector.nonZeroItems).map(v => (v._1, v._2 + offset))
      def shiftedItems(cv: Rep[T]) = (x1 outerSubtr x2(cv)).map(v => (v._1, v._2 + offset - cv))
      vector match {
        case DenseVector(vs) => DenseVector(vs.map(v => offset - v).updateMany(nonZeroIndices, updates))
        case ConstVector(cv, _) => ShiftVectorBoxed(nonZeroItems.map(v => (v._1, v._2 - cv)), offset - cv, length)
        case ZeroVector(_) => self
        case SparseVector(_, _, _) => ShiftVectorBoxed(newItems, offset, length)
        case SparseVectorBoxed(_, _) => ShiftVectorBoxed(newItems, offset, length)
        case ShiftVector(_, _, cv, _) => ShiftVectorBoxed(shiftedItems(cv), offset - cv, length)
        case ShiftVectorBoxed(_, cv, _) => ShiftVectorBoxed(shiftedItems(cv), offset - cv, length)
        case _ => vector *^ (-toRep(n.one)) +^ self
        //case _ => !!!("matcher for @vector argument in ShiftVectorBoxed.-^(vector: Vec[T]) is not specified.")
      }
    }
    def *^(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      lazy val c1 = offset
      def x1 = nonZeroItems.map(v => (v._1, v._2 - c1))
      def x2(cv: Rep[T]) = vector.nonZeroItems.map(v => (v._1, v._2 - cv))
      def x1_c2(cv: Rep[T]) = x1.map(v => (v._1, v._2 * cv))
      def x2_c1(cv: Rep[T]) = x2(cv).map(v => (v._1, v._2 * c1))
      def outerItems(cv: Rep[T]) = (x1 innerMult x2(cv)) outerSum x1_c2(cv) outerSum x2_c1(cv)
      def shiftedItems(cv: Rep[T]) = outerItems(cv).map(v => (v._1, v._2 + c1 * cv))
      vector match {
        case ShiftVectorBoxed(_, cv, _) => ShiftVectorBoxed(shiftedItems(cv), c1 * cv, length)
        case _ => vector *^ self
      }
    }
    def /^(vector: Vec[T])(implicit f: Fractional[T]): Vec[T] = {
      def updates = (nonZeroValues zip vector.items(nonZeroIndices)).map { case Pair(v1, v2) => v1 / v2 }
      def shiftedItems = vector.items.map(v => offset / v).updateMany(nonZeroIndices, updates)
      def newItems(cv: Rep[T]) = nonZeroItems.map(v => (v._1, v._2 / cv))
      vector match {
        case DenseVector(_) => DenseVector(shiftedItems)
        case ConstVector(cv, _) => ShiftVectorBoxed(newItems(cv), offset / cv, length)
        case ZeroVector(_) => !!!("attempt to divide ShiftVectorBoxed by ZeroVector.")
        case SparseVector(_, _, _) => DenseVector(shiftedItems)
        case SparseVectorBoxed(_, _) => DenseVector(shiftedItems)
        case ShiftVector(_, _, _, _) => DenseVector(shiftedItems)
        case ShiftVectorBoxed(_, _, _) => DenseVector(shiftedItems)
        case _ => !!!("matcher for @vector argument in ShiftVectorBoxed./^(vector: Vec[T]) is not specified.")
      }
    }
    def dot(vector: Vec[T])(implicit n: Numeric[T]): Rep[T] = {
      def sparseBoxed(cv: Rep[T]) = SparseVectorBoxed(vector.nonZeroItems.map(v => (v._1, v._2 - cv)), vector.length)
      def shift(cv: Rep[T]) = self.sum * cv
      vector match {
        case ShiftVectorBoxed(_, cv, _) => (self dot sparseBoxed(cv)) + shift(cv)
        case _ => vector dot self
      }
    }
    def *(matrix: Matr[T])(implicit n: Numeric[T]): Vec[T] = ???

    def pow_^(order: DoubleRep)(implicit n: Numeric[T], o: Overloaded2): Vec[T] = {
      SparseVectorBoxed(nonZeroIndices zip nonZeroValues.map(v => Math.pow(v.toDouble, order).asRep[T]), length)
    }

    def sum(implicit n: Numeric[T]): Rep[T] = nonZeroValues.reduce + (length - nonZeroValues.length).to[T] * offset
    def reduce(implicit m: RepMonoid[T]): Rep[T] = items.reduce(m)  //TODO: it's inefficient

    def euclideanNorm(implicit n: Numeric[T]): DoubleRep = Math.sqrt(nonZeroValues.map(v => v * v).reduce.toDouble)
  }

  trait AbstractVectorCompanion extends TypeFamily1[Vector] {
    def zero[T: Elem](len: IntRep): Vec[T] = ZeroVector[T](len)
    def const[T: Elem](const: Rep[T], len: IntRep): Vec[T] = ConstVector(const, len)
    def sparse[T: Elem](nonZeroIndices: Coll[Int], nonZeroValues: Coll[T], length: IntRep): Vec[T] =
      SparseVector(nonZeroIndices, nonZeroValues, length)
    @OverloadId("sparse_boxed_apply")
    def sparse[T: Elem](nonZeroItems: Coll[(Int, T)], length: IntRep): Vec[T] =
      SparseVectorBoxed(nonZeroItems, length)
  }
  trait DenseVectorCompanion extends ConcreteClass1[DenseVector]
  trait ConstVectorCompanion extends ConcreteClass1[ConstVector]
  trait SparseVectorCompanion extends ConcreteClass1[SparseVector]
  trait SparseVectorBoxedCompanion extends ConcreteClass1[SparseVectorBoxed]
}

trait VectorsDsl extends CollectionsDsl with impl.VectorsAbs { self: LADsl =>

  type VectorCompanion = Rep[AbstractVectorCompanion]

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

    //def map[R: Elem](f: Rep[T] => Rep[R]): Vec[R] = vector.mapBy(fun(f))

    def filter(f: Rep[T] => Rep[Boolean]): Vec[T] = vector.filterBy(fun(f))
  }
}

trait VectorsDslStd extends CollectionsDslStd with impl.VectorsStd { self: LADslStd =>

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

trait VectorsDslExp extends CollectionsDslExp with impl.VectorsExp { self: LADslExp =>

  def dotSparse[T: Elem](xIndices: Coll[Int], xValues: Coll[T], yIndices: Coll[Int], yValues: Coll[T])
                        (implicit n: Numeric[T]): Rep[T] = {
    DotSparse(xIndices.arr, xValues.arr, yIndices.arr, yValues.arr)
  }

  case class DotSparse[T](xIndices: Arr[Int], xValues: Arr[T], yIndices: Arr[Int], yValues: Arr[T])
                         (implicit val n: Numeric[T], selfType: Elem[T]) extends BaseDef[T]

  def binarySearch(index: IntRep, indices: Coll[Int]): IntRep = array_binary_search(index, indices.arr)
}
