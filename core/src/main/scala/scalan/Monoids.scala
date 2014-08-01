package scalan

import scalan._
import scalan.primitives._
import scalan.primitives.NumericOps

trait Monoids { self: Scalan =>
  case class RepMonoid[A](opName: String, append: Rep[((A, A)) => A], zero: Rep[A], isCommutative: Boolean)(implicit val eA: Elem[A]) {
    override def toString = s"Monoid[${eA.name}]($opName, $zero)"
  }

  object RepMonoid {
    def apply[A](opName: String, zero: A, isCommutative: Boolean)(append: (Rep[A], Rep[A]) => Rep[A])(implicit eA: Elem[A], d: DummyImplicit): RepMonoid[A] =
      new RepMonoid(opName, fun { p: Rep[(A, A)] => append(p._1, p._2) }, toRep(zero), isCommutative)
  }

  implicit lazy val ByteRepPlusMonoid: RepMonoid[Byte] =
    RepMonoid("+", 0.asInstanceOf[Byte], isCommutative = true) { (a, b) => a + b }
  implicit lazy val IntRepPlusMonoid: RepMonoid[Int] =
    RepMonoid("+", 0, isCommutative = true) { (a, b) => a + b }
  implicit lazy val FloatRepPlusMonoid: RepMonoid[Float] =
    RepMonoid("+", 0.0f, isCommutative = true) { (a, b) => a + b }
  implicit lazy val DoubleRepPlusMonoid: RepMonoid[Double] =
    RepMonoid("+", 0.0, isCommutative = true) { (a, b) => a + b }
  implicit lazy val BooleanRepOrMonoid: RepMonoid[Boolean] =
    RepMonoid("||", false, isCommutative = true) { (a, b) => a || b }

  lazy val IntRepMultMonoid =
    RepMonoid[Int]("*", 1, isCommutative = true) { (a, b) => a * b }
  lazy val FloatRepMultMonoid =
    RepMonoid[Float]("*", 1.0f, isCommutative = true) { (a, b) => a * b }
  lazy val DoubleRepMultMonoid =
    RepMonoid[Double]("*", 1.0, isCommutative = true) { (a, b) => a * b }
  lazy val BooleanRepAndMonoid =
    RepMonoid[Boolean]("&&", true, isCommutative = true) { (a, b) => a && b }

  lazy val predefinedMonoids = Set[RepMonoid[_]](
    ByteRepPlusMonoid,
    IntRepPlusMonoid,
    IntRepMultMonoid,
    FloatRepMultMonoid,
    FloatRepPlusMonoid,
    DoubleRepPlusMonoid,
    DoubleRepMultMonoid,
    BooleanRepAndMonoid,
    BooleanRepOrMonoid)

  def isPredefined(m: RepMonoid[_]): Boolean = predefinedMonoids.contains(m)
}
