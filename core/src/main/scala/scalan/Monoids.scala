package scalan

trait Monoids { self: Scalan =>
  case class RepMonoid[A](opName: String, zero: Rep[A], append: Rep[((A, A)) => A], isCommutative: Boolean)(implicit val eA: Elem[A]) {
    override def toString = repMonoid_toString(this)
    // avoids having to write monoid.append((x, y))
    def append(x: Rep[A], y: Rep[A]): Rep[A] = append.apply(Pair(x, y))
  }

  object RepMonoid {
    def apply[A](opName: String, zero: A, isCommutative: Boolean)(append: (Rep[A], Rep[A]) => Rep[A])(implicit eA: Elem[A]): RepMonoid[A] =
      new RepMonoid(opName, toRep(zero), fun { p: Rep[(A, A)] => append(p._1, p._2) }, isCommutative)
  }

  def repMonoid_toString[A](m: RepMonoid[A]) = s"Monoid[${m.eA.name}](${m.opName}, ${m.zero}, ${m.append})"

  // avoid monoids duplication (since duplicate functions aren't eliminated for now
  private val numericPlusMonoids = scala.collection.mutable.Map.empty[Elem[_], RepMonoid[_]]
  private val numericMultMonoids = scala.collection.mutable.Map.empty[Elem[_], RepMonoid[_]]

  implicit def numericPlusMonoid[A](implicit n: Numeric[A], e: Elem[A]): RepMonoid[A] =
    numericPlusMonoids.getOrElseUpdate(e, RepMonoid("+", n.zero, isCommutative = true) { _ + _ }).
      asInstanceOf[RepMonoid[A]]

  def numericMultMonoid[A](implicit n: Numeric[A], e: Elem[A]): RepMonoid[A] =
    numericMultMonoids.getOrElseUpdate(e, RepMonoid("*", n.one, isCommutative = true) { _ * _ }).
      asInstanceOf[RepMonoid[A]]

  implicit lazy val BooleanRepOrMonoid: RepMonoid[Boolean] =
    RepMonoid("||", false, isCommutative = true) { (a, b) => a || b }
  lazy val BooleanRepAndMonoid =
    RepMonoid[Boolean]("&&", true, isCommutative = true) { (a, b) => a && b }
}

trait MonoidsStd extends Monoids { self: ScalanStd =>
  override def repMonoid_toString[A](m: RepMonoid[A]) = s"Monoid[${m.eA.name}](${m.opName}, ${m.zero})"
}