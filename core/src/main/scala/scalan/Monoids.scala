package scalan

trait Monoids { self: Scalan =>
  case class RepMonoid[A](opName: String, zero: Rep[A], append: Rep[((A, A)) => A], isCommutative: Boolean)(implicit val eA: Elem[A]) {
    override def toString = repMonoid_toString(this)
  }

  object RepMonoid {
    def apply[A](opName: String, zero: A, isCommutative: Boolean)(append: (Rep[A], Rep[A]) => Rep[A])(implicit eA: Elem[A], d: DummyImplicit): RepMonoid[A] =
      new RepMonoid(opName, toRep(zero), fun { p: Rep[(A, A)] => append(p._1, p._2) }, isCommutative)
  }

  def repMonoid_toString[A](m: RepMonoid[A]) = s"Monoid[${m.eA.name}](${m.opName}, ${m.zero}, ${m.append})"

  implicit def numericPlusMonoid[A](implicit n: Numeric[A], e: Elem[A]): RepMonoid[A] =
    RepMonoid("+", n.zero, isCommutative = true) { _ + _ }
  def numericMultMonoid[A](implicit n: Numeric[A], e: Elem[A]): RepMonoid[A] =
    RepMonoid("*", n.one, isCommutative = true) { _ * _ }

  implicit lazy val BooleanRepOrMonoid: RepMonoid[Boolean] =
    RepMonoid("||", false, isCommutative = true) { (a, b) => a || b }
  lazy val BooleanRepAndMonoid =
    RepMonoid[Boolean]("&&", true, isCommutative = true) { (a, b) => a && b }
}

trait MonoidsSeq extends Monoids { self: ScalanSeq =>
  override def repMonoid_toString[A](m: RepMonoid[A]) = s"Monoid[${m.eA.name}](${m.opName}, ${m.zero})"
}