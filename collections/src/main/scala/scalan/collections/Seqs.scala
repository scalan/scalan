package scalan.collections

import scala.collection.Seq
import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe._

trait Seqs { self: SeqsDsl =>
  type RSeq[A] = Rep[SSeq[A]]

  /** Iterable collection that have a defined order of elements. */
  @ContainerType @FunctorType
  trait SSeq[A] extends TypeWrapper[Seq[A], SSeq[A]] { self =>
    implicit def eA: Elem[A]

    /** The size of this sequence. */
    @External @Semantics(name = ContainerLength)
    def size: Rep[Int]

    /** Selects an element by its index in the coll. */
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

    /** Convert the sequence to list. */
    @External def toList: Rep[List[A]]
  }

  trait SSeqCompanion extends ExCompanion1[SSeq] {
    /** Creates a sequence based on an array. */
    @External def apply[A: Elem](@ArgList arr: Rep[Array[A]]): Rep[SSeq[A]]

    /** Creates a sequence without elements. */
    @External def empty[A:Elem]: Rep[SSeq[A]]

    /** Creates a sequence that contains one element. */
    @External def single[A: Elem](elem: Rep[A]): Rep[SSeq[A]]

    /** Creates a sequence which based on a list. */
    @External def fromList[A:Elem](list: Rep[List[A]]): Rep[SSeq[A]]
  }

  def DefaultOfSeq[A: Elem]: Default[Seq[A]] = Default.defaultVal(Seq.empty[A])
}

trait SeqsDslStd extends impl.SeqsStd {
  trait StdSSeq[A] extends SSeqImpl[A] {
    override def map[B:Elem](f: Rep[A => B]): Rep[SSeq[B]] = SSeqImpl(wrappedValue.map(f))
    override def toArray = wrappedValue.toArray[A](eA.classTag)
  }
  implicit class SeqOps(s: Seq.type) {
    def single[A](elem: A): Seq[A] = Seq(elem)
    def fromList[A](list: List[A]): Seq[A] = Seq(list: _*)
  }
}

trait SeqsDslExp extends impl.SeqsExp {
  import SSeqMethods._
  override def rewriteDef[T](d: Def[T]) = d match {
    // Rule: xs.map(f)(i) ==> f(xs(i))
    case apply(Def(map(xs: RSeq[a], f: Rep[Function1[_, b]] @unchecked)), i) =>
      implicit val eT = xs.elem.eItem
      f.asRep[a => b](xs(i))

//    case SSeqCompanionMethods.apply(HasViews(source, iso: ArrayIso[a,b])) => {
//      implicit val ea = iso.eA
//      ViewSSeq(SSeq(source.asRep[Array[a]]))(SSeqIso(iso.innerIso))
//    }
    case SSeqMethods.filter(Def(view: ViewSSeq[a, b]), f) => {
      val ff = f.asRep[b => Boolean]
      val iso = view.innerIso
      implicit val eA = iso.eFrom
      implicit val eB = iso.eTo
      val filtered = view.source.filter(fun{ x => ff(iso.to(x))})
      ViewSSeq(filtered, iso)
    }

    case SSeqMethods.+:(HasViews(xs, Def(iso: SSeqIso[a, b])), value@HasViews(_, _)) =>
      implicit val eA = iso.innerIso.eFrom
      val xs1 = xs.asRep[SSeq[a]]
      val value1 = iso.innerIso.from(value.asRep[b])
      ViewSSeq(xs1.+:(value1), iso.innerIso)

    case SSeqMethods.diff(HasViews(xs, Def(iso: SSeqIso[a, b])), ys@HasViews(_, Def(_: SSeqIso[_, _]))) =>
      implicit val eA = iso.innerIso.eFrom
      val xs1 = xs.asRep[SSeq[a]]
      val ys1 = iso.from(ys.asRep[SSeq[b]])
      ViewSSeq(xs1.diff(ys1), iso.innerIso)

    case _ => super.rewriteDef(d)
  }

}
