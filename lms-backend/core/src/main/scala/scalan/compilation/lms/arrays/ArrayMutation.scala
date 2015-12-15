// copied from LMS to avoid depending on tests
// Apply any changes made there! To check:
// git log v0.9.0..HEAD test-src/epfl/test8-effects/Arrays.scala
// Don't forget to update the command after applying changes
package scalan.compilation.lms.arrays

trait ArrayMutation extends ArrayLoops {

  //def infix_update[T:Manifest](a: Rep[Array[T]], i: Rep[Int], x: Rep[T]): Rep[Unit]
  //def infix_insert[T:Manifest](a: Rep[Array[T]], i: Rep[Int], x: Rep[T]): Rep[Array[T]]

  def infix_mutable[T:Manifest](a: Rep[Array[T]]): Rep[Array[T]]
  def infix_clone[T:Manifest](a: Rep[Array[T]]): Rep[Array[T]]

}


trait ArrayMutationExp extends ArrayMutation with ArrayLoopsExp {

  //case class ArrayUpdate[T](a: Rep[Array[T]], i: Rep[Int], x: Rep[T]) extends Def[Unit]
  case class ArrayMutable[T](a: Rep[Array[T]]) extends Def[Array[T]]
  case class ArrayClone[T](a: Rep[Array[T]]) extends Def[Array[T]]

  //def infix_update[T:Manifest](a: Rep[Array[T]], i: Rep[Int], x: Rep[T]) = reflectWrite(a)(ArrayUpdate(a,i,x))

  def infix_mutable[T:Manifest](a: Rep[Array[T]]) = reflectMutable(ArrayMutable(a))
  def infix_clone[T:Manifest](a: Rep[Array[T]]) = ArrayClone(a)

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case SimpleLoop(s,i, ArrayElem(y)) => Nil
    case SimpleLoop(s,i, FlattenElem(y)) => Nil
    case SimpleLoop(s,i, ReduceElem(y)) => syms(y) // could also return zero value
    case SimpleLoop(s,i, ReduceIntElem(y)) => syms(y) // could also return zero value
    case SimpleLoop(s,i, ArrayIfElem(c,y)) => Nil
    case SimpleLoop(s,i, FlattenIfElem(c,y)) => Nil
    case SimpleLoop(s,i, ReduceIfElem(c,y)) => syms(y) // could also return zero value
    case SimpleLoop(s,i, ReduceIfIntElem(c,y)) => syms(y) // could also return zero value
    case ArrayIndex(a,i) => Nil
    case ArrayMutable(a) => Nil
    case ArrayClone(a) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case SimpleLoop(s,i, ArrayElem(y)) => syms(y)
    case SimpleLoop(s,i, FlattenElem(y)) => syms(y)
    case SimpleLoop(s,i, ReduceElem(y)) => Nil
    case SimpleLoop(s,i, ReduceIntElem(y)) => Nil
    case SimpleLoop(s,i, ArrayIfElem(c,y)) => syms(y)
    case SimpleLoop(s,i, FlattenIfElem(c,y)) => syms(y)
    case SimpleLoop(s,i, ReduceIfElem(c,y)) => Nil
    case SimpleLoop(s,i, ReduceIfIntElem(c,y)) => Nil
    case ArrayIndex(a,i) => Nil
    case ArrayMutable(a) => Nil
    case ArrayClone(a) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case SimpleLoop(s,i, ArrayElem(y)) => Nil
    case SimpleLoop(s,i, FlattenElem(y)) => Nil
    case SimpleLoop(s,i, ReduceElem(y)) => Nil
    case SimpleLoop(s,i, ReduceIntElem(y)) => Nil
    case SimpleLoop(s,i, ArrayIfElem(c,y)) => Nil
    case SimpleLoop(s,i, FlattenIfElem(c,y)) => Nil
    case SimpleLoop(s,i, ReduceIfElem(c,y)) => Nil
    case SimpleLoop(s,i, ReduceIfIntElem(c,y)) => Nil
    case ArrayIndex(a,i) => syms(a)
    case ArrayMutable(a) => Nil
    case ArrayClone(a) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case SimpleLoop(s,i, ArrayElem(y)) => Nil
    case SimpleLoop(s,i, FlattenElem(y)) => Nil
    case SimpleLoop(s,i, ReduceElem(y)) => Nil
    case SimpleLoop(s,i, ReduceIntElem(y)) => Nil
    case SimpleLoop(s,i, ArrayIfElem(c,y)) => Nil
    case SimpleLoop(s,i, FlattenIfElem(c,y)) => Nil
    case SimpleLoop(s,i, ReduceIfElem(c,y)) => Nil
    case SimpleLoop(s,i, ReduceIfIntElem(c,y)) => Nil
    case ArrayIndex(a,i) => Nil
    case ArrayMutable(a) => syms(a)
    case ArrayClone(a) => syms(a)
    case _ => super.copySyms(e)
  }
}

trait ScalaGenArrayMutation extends ScalaGenArrayLoops {
  val IR: ArrayMutationExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayMutable(a) =>
      emitValDef(sym, src"$a.clone // mutable")
    case ArrayClone(a) =>
      emitValDef(sym, src"$a.clone")
    case ArrayUpdate(a,i,x) =>
      emitValDef(sym, src"$a.update($i, $x)")
    case _ => super.emitNode(sym, rhs)
  }
}
