package scalan.meta

trait Examples {
  val reactiveModule =
    """package scalan.rx
     |import scalan._
     |trait Reactive extends Scalan {
     |  type Obs[A] = Rep[Observable[A]]
     |  trait Observable[A] {
     |    implicit def eA: Elem[A]
     |  }
     |  class ObservableImpl1[A](implicit val eA: Elem[A]) extends Observable[A] {
     |  }
     |  class ObservableImpl2[A](implicit val eA: Elem[A]) extends Observable[A] {
     |  }
     |}
    """.stripMargin

}
