package scalan.collections

import scala.collection.Seq
import scalan._
import scalan.common.Default

trait Seqs extends Base with BaseTypes { self: ScalanCommunityDsl =>
  type RSeq[A] = Rep[Seq[A]]

  /** Iterable collection that have a defined order of elements. */
  trait SSeq[A] extends BaseTypeEx[Seq[A], SSeq[A]] { self =>
    implicit def eA: Elem[A]
    def wrappedValueOfBaseType: Rep[Seq[A]]

    /** The size of this sequence. */
    @External def size: Rep[Int]

    /** Selects an element by its index in the $coll. */
    @External def apply(idx: Rep[Int]): Rep[A]

    @External def slice(unc_from: Rep[Int], unc_until: Rep[Int]): Rep[Seq[A]]

    /** Tests whether this sequence is empty. */
    @External def isEmpty: Rep[Boolean]

    /** Builds a new sequence by applying a function to all elements of this sequence. */
    @External(methodName = "map") def map[B: Elem](f: Rep[A => B]): Rep[Seq[B]]
    //@External def map[B: Elem](f: Rep[A] => Rep[B]): Rep[Seq[B]] = mapBy(fun(f))

    /** Reduces the elements of this sequence using the specified associative binary operator. */
    @External def reduce(op: Rep[((A, A)) => A]): Rep[A]

    /** Selects all elements of this sequence which satisfy a predicate. */
    @External def filter(p: Rep[A => Boolean]): Rep[Seq[A]]

    /** A copy of the sequence with an element prepended. */
    @External def +:(elem: Rep[A]): Rep[Seq[A]]

    /** Returns a new sequence which contains all elements of this sequence
      * except some of occurrences of elements that also appear in that. */
    @External def diff(that: Rep[Seq[A]]): Rep[Seq[A]]

    @External def toArray: Rep[Array[A]]
  }

  trait SSeqCompanion extends ExCompanion1[Seq] {
    /** Creates a sequence based on an array. */
    @External def apply[A: Elem](@ArgList arr: Rep[Array[A]]): Rep[Seq[A]]

    @External def empty[A:Elem]: Rep[Seq[A]]
    @External def fromList[A:Elem](list: Rep[List[A]]): Rep[Seq[A]]
  }

  def DefaultOfSeq[A: Elem]: Default[Seq[A]] = Default.defaultVal(Seq.empty[A])
}

trait SeqsDsl extends impl.SeqsAbs { self: ScalanCommunityDsl => }
trait SeqsDslSeq extends impl.SeqsSeq { self: ScalanCommunityDslSeq =>
  trait SeqSSeq[A] extends SSeqImpl[A] {
    override def map[B:Elem](f: Rep[A => B]): Rep[Seq[B]] = wrappedValueOfBaseType.map(f)
    override def toArray = wrappedValueOfBaseType.toArray[A](eA.classTag)
  }
  implicit class SeqOps(s: Seq.type) {
    def fromList[A](list: List[A]): Seq[A] = Seq(list: _*)
  }
}
trait SeqsDslExp extends impl.SeqsExp { self: ScalanCommunityDslExp => }
