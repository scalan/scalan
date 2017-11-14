package scalan.common

import scalan.Scalan

trait MetaTests { self: MetaTestsModule =>

  type RMetaTest[T] = Rep[MetaTest[T]]
  trait MetaTest[T] extends Def[MetaTest[T]] { self =>
    def test: RMetaTest[T]
    def give: Rep[T]
    def size: Rep[Int]
    implicit def eT: Elem[T]
  }
  trait MetaTestCompanion

  abstract class MT0(val size: Rep[Int]) extends MetaTest[Unit] {

    def test: RMetaTest[Unit] = ???
    def give: Rep[Unit] = ???
    def eT = UnitElement
  }
  trait MT0Companion

  abstract class MT1[T](val data: Rep[T], val size: Rep[Int]) extends MetaTest[T] {
    def test: RMetaTest[T] = ???
    def give: Rep[T] = ???
  }

  abstract class MT2[A, B](val indices: Rep[A], val values: Rep[B], val size: Rep[Int])
    extends MetaTest[(A, B)] {
    implicit def eA: Elem[A]; implicit def eB: Elem[B]
    def test: RMetaTest[(A, B)] = ???
    def give: Rep[(A, B)] = ???
    lazy val eT = element[(A, B)]
  }
}

trait MetaTestsModule extends impl.MetaTestsDefs
