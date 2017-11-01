package scala {
  import scalan._

  import impl._

  import scala.wrappers.WrappersModule

  trait WArrays extends Base { self: WrappersModule =>
    @External("Array") @ContainerType @FunctorType trait WArray[T] extends Def[WArray[T]] { self =>
      implicit def eT: Elem[T];
      @External def apply(i: Rep[Int]): Rep[T];
      @External def length: Rep[Int]
    };
    trait WArrayCompanion
  }
}