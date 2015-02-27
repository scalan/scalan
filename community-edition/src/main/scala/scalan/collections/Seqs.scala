package scalan.collections

import scala.collection.Seq
import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe._

trait Seqs extends Base with BaseTypes { self: ScalanCommunityDsl =>
  type RSeq[A] = Rep[SSeq[A]]

  /** Iterable collection that have a defined order of elements. */
  @ContainerType
  trait SSeq[A] extends BaseTypeEx[Seq[A], SSeq[A]] { self =>
    implicit def eA: Elem[A]
    def wrappedValueOfBaseType: Rep[Seq[A]]

    /** The size of this sequence. */
    @External @Semantics(name = ContainerLength)
    def size: Rep[Int]

    /** Selects an element by its index in the $coll. */
    @External @Semantics(name = ContainerApply)
    def apply(idx: Rep[Int]): Rep[A]

    @External def slice(unc_from: Rep[Int], unc_until: Rep[Int]): Rep[SSeq[A]]

    /** Tests whether this sequence is empty. */
    @External def isEmpty: Rep[Boolean]

    /** Builds a new sequence by applying a function to all elements of this sequence. */
    @External(methodName = "map") def map[B: Elem](f: Rep[A => B]): Rep[SSeq[B]]
    //@External def map[B: Elem](f: Rep[A] => Rep[B]): Rep[Seq[B]] = mapBy(fun(f))

    /** Reduces the elements of this sequence using the specified associative binary operator. */
    @External def reduce(op: Rep[((A, A)) => A]): Rep[A]

    /** Selects all elements of this sequence which satisfy a predicate. */
    @External def filter(p: Rep[A => Boolean]): Rep[SSeq[A]]

    /** A copy of the sequence with an element prepended. */
    @External def +:(elem: Rep[A]): Rep[SSeq[A]]

    /** Returns a new sequence which contains all elements of this sequence
      * except some of occurrences of elements that also appear in that. */
    @External def diff(that: Rep[SSeq[A]]): Rep[SSeq[A]]

    /** Convert the sequence to an array. */
    @External def toArray: Rep[Array[A]]
  }

  trait SSeqCompanion extends ExCompanion1[SSeq] {
    /** Creates a sequence based on an array. */
    @External def apply[A: Elem](@ArgList arr: Rep[Array[A]]): Rep[SSeq[A]]

    /** Creates a sequence without elements. */
    @External def empty[A:Elem]: Rep[SSeq[A]]

    /** Creates a sequence with one element. */
    @External def fromElem[A: Elem](elem: Rep[A]): Rep[SSeq[A]]

    /** Creates a sequence which based on a list. */
    @External def fromList[A:Elem](list: Rep[List[A]]): Rep[SSeq[A]]
  }

  def DefaultOfSeq[A: Elem]: Default[Seq[A]] = Default.defaultVal(Seq.empty[A])
}

trait SeqsDsl extends impl.SeqsAbs { self: ScalanCommunityDsl =>
}

trait SeqsDslSeq extends impl.SeqsSeq { self: ScalanCommunityDslSeq =>
  trait SeqSSeq[A] extends SSeqImpl[A] {
    override def map[B:Elem](f: Rep[A => B]): Rep[SSeq[B]] = SSeqImpl(wrappedValueOfBaseType.map(f))
    override def toArray = wrappedValueOfBaseType.toArray[A](eA.classTag)
  }
  implicit class SeqOps(s: Seq.type) {
    def fromElem[A](elem: A): Seq[A] = Seq(elem)
    def fromList[A](list: List[A]): Seq[A] = Seq(list: _*)
  }
}

trait SeqsDslExp extends impl.SeqsExp { self: ScalanCommunityDslExp =>

  type FilterArgs[A] = (Rep[SSeq[A]], Rep[A => Boolean])

  override def rewriteDef[T](d: Def[T]) = d match {
    case SSeqMethods.apply(Def(d2), i) => d2 match {
      case SSeqMethods.map(t: SSeqMapArgs[a,b] @unchecked) =>
        val xs = t._1; val f = t._2
        implicit val eT = xs.elem.eItem
        f(xs(i))
      case _ =>
        super.rewriteDef(d)
    }
    case SSeqCompanionMethods.apply(HasViews(source, iso: ArrayIso[a,b])) => {
      implicit val ea = iso.eA
      ViewSSeq(SSeq(source.asRep[Array[a]]))(SSeqIso(iso.innerIso))
    }
    case SSeqMethods.filter(Def(view: ViewSSeq[a, b]), f) => {
      val ff = f.asRep[b => Boolean]
      val iso = view.innerIso
      implicit val eA = iso.eFrom
      implicit val eB = iso.eTo
      val filtered = view.source.filter(fun{ x => ff(iso.to(x))})
      ViewSSeq(filtered)(SSeqIso(iso))
    }

    case _ => super.rewriteDef(d)
  }

}
