package scalan.collection {
  import scalan._

  trait Cols extends Base { self: ColsModule =>
    trait Col[A] extends Def[Col[A]] {
      implicit def eA: Elem[A];
      def arr: Rep[WArray[A]];
      def length: Rep[Int];
      def apply(i: Rep[Int]): Rep[A]
    };
    trait ColBuilder {
      def fromArray[T](arr: Rep[WArray[T]]): Rep[Col[T]];
      def ddmvm(v: Rep[WArray[Double]]): Rep[Int] = {
        val xs: Rep[WArray[Int]] = WArray.fill[Int](v.length, Thunk(toRep(0.asInstanceOf[Int])));
        val c: Rep[WArray[scala.Tuple2[Int, Double]]] = xs.zip(v).map(fun(((d: Rep[scala.Tuple2[Int, Double]]) => d)));
        c.length
      }
    };
    trait ColCompanion
  }
}