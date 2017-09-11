package scala {
  import scalan._

  import impl._

  import scala.Array

  import scala.wrappers.WrappersModule

  trait WArrays extends Base with TypeWrappers { self: WrappersModule =>
    trait WArray[T] extends TypeWrapper[Array[T], WArray[T]] { self =>
      implicit def eeT: Elem[T];
      def wrappedValue: Rep[Array[T]];
      @External def apply(i: Rep[Int]): Rep[T];
      @External def length: Rep[Int]
    };
    trait WArrayCompanion extends ExCompanion1[WArray];
    def DefaultOfArray[T](implicit eT: Elem[T]): Rep[Array[T]] = toRep(null.asInstanceOf[Array[T]])
  }
}