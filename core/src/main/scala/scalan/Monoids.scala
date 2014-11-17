package scalan

trait Monoids { self: Scalan =>
  case class RepMonoid[A](opName: String, append: Rep[((A, A)) => A], zero: Rep[A], isCommutative: Boolean)(implicit val eA: Elem[A]) {
    override def toString = s"Monoid[${eA.name}]($opName, $zero)"
  }

  object RepMonoid {
    def apply[A](opName: String, zero: A, isCommutative: Boolean)(append: (Rep[A], Rep[A]) => Rep[A])(implicit eA: Elem[A], d: DummyImplicit): RepMonoid[A] =
      new RepMonoid(opName, fun { p: Rep[(A, A)] => append(p._1, p._2) }, toRep(zero), isCommutative)
  }

  implicit def numericPlusMonoid[A](implicit n: Numeric[A], e: Elem[A]): RepMonoid[A] =
    RepMonoid("+", n.zero, isCommutative = true) { _ + _ }
  def numericMultMonoid[A](implicit n: Numeric[A], e: Elem[A]): RepMonoid[A] =
    RepMonoid("*", n.one, isCommutative = true) { _ * _ }

  implicit lazy val BooleanRepOrMonoid: RepMonoid[Boolean] =
    RepMonoid("||", false, isCommutative = true) { (a, b) => a || b }
  lazy val BooleanRepAndMonoid =
    RepMonoid[Boolean]("&&", true, isCommutative = true) { (a, b) => a && b }
}
