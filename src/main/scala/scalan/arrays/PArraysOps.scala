package scalan.arrays

import scalan.{ScalanStaged, ScalanSeqImplementation, ScalanDsl}

trait PArraysOps { scalan: PArraysDsl =>
  import implicitManifests._

  trait PArrayOps[T] extends PArray[T] {
    def map[R:Elem](f: Rep[T] => Rep[R]) = {
      val fun_f = fun(f)
      PArray(arr.map(fun_f))
    }
  }
  trait PArrayCompanion {
    def apply[T:Elem](arr: Rep[Array[T]]): PA[T] = fromArray(arr)
    def fromArray[T](arr: Rep[Array[T]])(implicit eT: Elem[T]): PA[T] = eT match {
      case baseE: BaseElem[a] =>
        BaseArray[a](arr.asRep[Array[a]])
      case pairE: PairElem[a,b] =>
        implicit val ea = pairE.ea
        implicit val eb = pairE.eb
        val ps = arr.asRep[Array[(a,b)]]
        val as = fromArray(ps.map(fun { _._1 }))
        val bs = fromArray(ps.map(fun { _._2 }))
        PairArray[a,b](as, bs)
      case _ => ???
    }
  }

  //-------------------------------  BaseType ----------------------------------
  trait BaseArrayOps[T] extends PArrayOps[T] {
    def length = arr.length
  }
  trait BaseArrayCompanion {
  }

  //-------------------------------  Tuple2Type ----------------------------------
  trait PairArrayOps[A,B] extends PArrayOps[(A,B)] {
    implicit def eA: Elem[A]
    implicit def eB: Elem[B]
    def eT = element[(A,B)]
    def as: PA[A]
    def bs: PA[B]
    def mapPairs[R:Elem](f: (Rep[A],Rep[B]) => Rep[R]): PA[R] = {
      map({ (p: Rep[(A,B)]) => { val Pair(a,b) = p; f(a,b)}})
    }
    def arr = as.arr zip bs.arr
    def length = as.length
  }
  trait PairArrayCompanion {
  }
}

trait PArraysDsl extends ScalanDsl with PArraysAbs with PArraysOps { }

trait PArraysDslSeq extends PArraysDsl with PArraysSeq with ScalanSeqImplementation

trait PArraysDslExp extends PArraysDsl with PArraysExp with ScalanStaged