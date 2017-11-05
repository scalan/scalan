package scala {
  import scalan._

  import impl._

  import scala.wrappers.WrappersModule

  trait WArrays extends Base { self: WrappersModule =>
    @External("Array") @ContainerType @FunctorType trait WArray[T] extends Def[WArray[T]] { self =>
      implicit def eT: Elem[T];
      @External def zip[B](ys: Rep[WArray[B]]): Rep[WArray[scala.Tuple2[T, B]]];
      @External def map[B](f: Rep[scala.Function1[T, B]]): Rep[WArray[B]];
      @External def length: Rep[Int]
    };
    trait WArrayCompanion {
      @External def fill[@Reified T](n: Rep[Int], elem: Rep[Thunk[T]]): Rep[WArray[T]]
    }
  }
}