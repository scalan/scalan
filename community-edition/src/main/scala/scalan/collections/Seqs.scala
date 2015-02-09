package scalan.collections

import scala.collection.immutable.Seq
import scalan._
import scalan.common.Default

trait Seqs extends Base with BaseTypes { self: SeqsDsl =>
  type RSeq[A] = Rep[Seq[A]]

  /** Iterable collection that have a defined order of elements. */
  trait SSeq[A] extends BaseTypeEx[Seq[A], SSeq[A]] { self =>
    implicit def eA: Elem[A]

    @External def isEmpty: Rep[Boolean]
  }

  trait SSeqCompanion extends ExCompanion1[Seq] {
    /** Creates a sequence based on an array. */
    @External def apply[A: Elem](arr: Rep[Array[A]]): Rep[Seq[A]]

    @External def empty[A:Elem]: Rep[Seq[A]]
  }

  implicit def DefaultOfSeq[A: Elem]: Default[Seq[A]] = Default.defaultVal(Seq.empty[A])
}

trait SeqsDsl extends impl.SeqsAbs
trait SeqsDslSeq extends impl.SeqsSeq
trait SeqsDslExp extends impl.SeqsExp
