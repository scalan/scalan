package scalan.common

/**
 * Created by Victor Smirnov on 4/14/15.
 */

trait MetaTests { self: MetaTestsDsl =>

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

  abstract class MT1[T](val data: Rep[T], val size: Rep[Int])(implicit val elem: Elem[T]) extends MetaTest[T] {

    def test: RMetaTest[T] = ???
    def give: Rep[T] = ???
  }

  abstract class MT2[T, R](val indices: Rep[T], val values: Rep[R], val size: Rep[Int])
                          (implicit val eT: Elem[T], implicit val eR: Elem[R]) extends MetaTest[(T, R)] {

    def test: RMetaTest[(T, R)] = ???
    def give: Rep[(T, R)] = ???
    lazy val elem = element[(T, R)]
  }
}

trait MetaTestsDsl extends impl.MetaTestsAbs
