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

  val colsModuleText =
    """package scalan.collection
     |import scalan._
     |trait Cols extends Scalan {
     |  type Col[A] = Rep[Collection[A]]
     |  trait Collection[A] {
     |    implicit def eA: Elem[A]
     |    def length: Rep[Int];
     |    def apply(i: Rep[Int]): Rep[A]
     |  }
     |  abstract class ColOverArray[A](val arr: Rep[WArray[A]]) extends Col[A] {
     |    val list: Rep[WList[A]] = arr.toList
     |    def length: Rep[Int] = ColOverArray.this.arr.length;
     |    def apply(i: Rep[Int]): Rep[A] = ColOverArray.this.arr.apply(i)
     |  };
     |}
    """.stripMargin

}
