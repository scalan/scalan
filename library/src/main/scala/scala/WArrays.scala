package scala {
  import scalan._

  import impl._

  import scala.wrappers.WrappersModule

  trait WArrays extends Base { self: WrappersModule =>
    type RepWArray[T] = Rep[WArray[T]];
    @ContainerType @FunctorType trait WArray[T] extends Def[WArray[T]] { self =>
      implicit def eT: Elem[T];
      @External def apply(i: Rep[Int]): Rep[T];
      @External def zip[B](ys: Rep[WArray[B]]): Rep[WArray[scala.Tuple2[T, B]]];
      @External def map[B](f: Rep[scala.Function1[T, B]]): Rep[WArray[B]];
      @External def length: Rep[Int]
    };
    trait WArrayCompanion extends ExCompanion1[WArray] {
      @External def fill[@Reified T](n: Rep[Int], elem: Rep[Thunk[T]]): Rep[WArray[T]]
    }
  }
}