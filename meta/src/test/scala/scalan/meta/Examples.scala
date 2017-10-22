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

  val warraysModuleText =
    """
     |package scala {
     |  import scalan._
     |  import impl._
     |  import scala.wrappers.WrappersModule
     |
     |  trait WArrays extends Base { self: WrappersModule =>
     |    type RepWArray[T] = Rep[WArray[T]];
     |    @External("Array") @ContainerType @FunctorType trait WArray[T] extends Def[WArray[T]] { self =>
     |      implicit def eT: Elem[T];
     |      @External def apply(i: Rep[Int]): Rep[T];
     |      @External def zip[B](ys: Rep[WArray[B]]): Rep[WArray[scala.Tuple2[T, B]]];
     |      @External def map[B](f: Rep[scala.Function1[T, B]]): Rep[WArray[B]];
     |      @External def length: Rep[Int]
     |    };
     |    trait WArrayCompanion {
     |      @External def fill[@Reified T](n: Rep[Int], elem: Rep[Thunk[T]]): Rep[WArray[T]]
     |    }
     |  }
     |}
    """.stripMargin

}
