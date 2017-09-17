package scalanizer.collections {
  import scalan._

  trait Cols extends Base { self: ColsModule =>
    type RepCol[A] = Rep[Col[A]];
    trait Col[A] extends Def[Col[A]] {
      implicit def eA: Elem[A];
      def arr: Rep[WArray[A]];
      def length: Rep[Int];
      def apply(i: Rep[Int]): Rep[A]
    };
    abstract class ColOverArray[A](val arr: Rep[WArray[A]])(implicit val eA: Elem[A]) extends Col[A] {
      def length: Rep[Int] = ColOverArray.this.arr.length;
      def apply(i: Rep[Int]): Rep[A] = ColOverArray.this.arr.apply(i)
    };
    trait ColCompanion {
      def fromArray[T](arr: Rep[WArray[T]])(implicit emT: Elem[T]): Rep[Col[T]] = ColOverArray(arr);
      def ddmvm(v: Rep[WArray[Double]]): Rep[Int] = {
        val xs: Rep[WArray[Int]] = WArray.fill[Int](v.length)(Thunk(toRep(0)));
        val c: Rep[WArray[scala.Tuple2[Int, Double]]] = xs.zip(v).map(fun(((d: Rep[scala.Tuple2[Int, Double]]) => d)))
        c.length
      }
    };
    trait ColOverArrayCompanion
  }

  trait ColsModule extends impl.ColsDefs with scala.wrappers.WrappersModule
}