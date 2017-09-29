package scalan.common

import scalan.Scalan

trait MetaTests { self: MetaTestsModule =>

  type RMetaTest[T] = Rep[MetaTest[T]]
  trait MetaTest[T] extends Def[MetaTest[T]] { self =>
    def test: RMetaTest[T]
    def give: Rep[T]
    def size: Rep[Int]
    implicit def elem: Elem[T]
  }
  trait MetaTestCompanion

  abstract class MT0(val size: Rep[Int]) extends MetaTest[Unit] {

    def test: RMetaTest[Unit] = ???
    def give: Rep[Unit] = ???
    def elem = UnitElement
  }
  trait MT0Companion

  abstract class MT1[T](val data: Rep[T], val size: Rep[Int]) extends MetaTest[T] {
    def test: RMetaTest[T] = ???
    def give: Rep[T] = ???
  }

  abstract class MT2[T, R](val indices: Rep[T], val values: Rep[R], val size: Rep[Int])
    extends MetaTest[(T, R)] {
    implicit def eT: Elem[T]; implicit def eR: Elem[R]
    def test: RMetaTest[(T, R)] = ???
    def give: Rep[(T, R)] = ???
    lazy val elem = element[(T, R)]
  }
}

trait MetaTestsModule extends impl.MetaTestsDefs
