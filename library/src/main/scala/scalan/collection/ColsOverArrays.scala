package scalan.collection {
  import scalan._

  trait ColsOverArrays extends Base { self: Library =>
    abstract class ColOverArray[A](val arr: Rep[WArray[A]]) extends Col[A] {
      def length: Rep[Int] = ColOverArray.this.arr.length;
      def apply(i: Rep[Int]): Rep[A] = ColOverArray.this.arr.apply(i)
    };
    abstract class ColOverArrayBuilder extends ColBuilder {
      def fromArray[T](arr: Rep[WArray[T]]): Rep[Col[T]] = ColOverArray(arr)
    };
    trait ColOverArrayCompanion;
    trait ColOverArrayBuilderCompanion
  }
}