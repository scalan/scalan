package scalan.arrays

import scalan.common.Default
import scalan._

trait PArraysOps { scalan: PArraysDsl =>
  import TagImplicits._

  trait PArrayOps[T] extends PArray[T] {
    def map[R:Elem](f: Rep[T] => Rep[R]) = {
      val fun_f = fun(f)
      mapBy(fun_f)
    }
    def mapBy[R:Elem](f: Rep[T=>R]) = {
      PArray(arr.map(f))
    }
    def zip[U:Elem](ys: PA[U]) = PairArray(self, ys)
  }
  trait PArrayCompanion extends TypeFamily1[PArray] {
    def defaultOf[A](implicit ea: Elem[A]): Default[Rep[PArray[A]]] = ea match {
      case baseE: BaseElem[a] => BaseArray.defaultOf[a](baseE)
      case pairE: PairElem[a,b] => PairArray.defaultOf[a,b](pairE.ea, pairE.eb)
      case _ => ???
    }

    def apply[T:Elem](arr: Rep[Array[T]]): PA[T] = fromArray(arr)
    def fromArray[T:Elem](arr: Rep[Array[T]]): PA[T] = element[T] match {
      case baseE: BaseElem[a] =>
        BaseArray[a](arr.asRep[Array[a]])
      case pairE: PairElem[a,b] =>
        implicit val ea = pairE.ea
        implicit val eb = pairE.eb
        val ps = arr.asRep[Array[(a,b)]]
        val as = fromArray(ps.map(fun { _._1 }))
        val bs = fromArray(ps.map(fun { _._2 }))
        as zip bs //PairArray[a,b](as, bs)
      case _ => ???
    }

    def replicate[T:Elem](len: Rep[Int], v: Rep[T]): PA[T] = element[T] match {
      case baseE: BaseElem[a] =>
        BaseArray[a](array_Replicate(len, v.asRep[a]))
      case _ => ???

    }

    def singleton[T:Elem](v: Rep[T]): PA[T] = element[T] match {
      case paE: PArrayElem[tFrom,tTo] => ???
      case _ => replicate(toRep(1), v)
    }
  }

  //-------------------------------  BaseType ----------------------------------
  trait BaseArrayOps[T] extends PArrayOps[T] {
    def length = arr.length
  }
  trait BaseArrayCompanion extends ConcreteClass1[BaseArray] {
    def defaultOf[A](implicit ea: Elem[A]) = Default.defaultVal(BaseArray(Default.defaultOf[Rep[Array[A]]]))
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
  trait PairArrayCompanion extends ConcreteClass2[PairArray] {
    def defaultOf[A,B](implicit ea: Elem[A], eb: Elem[B]) = {
      val as = PArray.defaultOf[A].value
      val bs = PArray.defaultOf[B].value
      Default.defaultVal(PairArray(as, bs))
    }
  }
}

trait PArraysDsl extends ScalanDsl with PArraysAbs with PArraysOps { }

trait PArraysDslSeq extends PArraysDsl with PArraysSeq with ScalanSeqImplementation

trait PArraysDslExp extends PArraysDsl with PArraysExp with ScalanStaged