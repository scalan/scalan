package scalan.meta

trait StaticMetaTests { self: StaticMetaTestsDsl =>

  type RMetaTest = Rep[MetaTest]
  trait MetaTest extends Def[MetaTest]

  abstract class MTWithStaticData(val f1: Rep[Int], val f2: Int, val f3: Rep[Double]) extends MetaTest

  abstract class MTWithNonTypeDescImplicits[A]()(implicit val elem: Elem[A], o: Ordering[A], kp: KeyPath) extends MetaTest
}
