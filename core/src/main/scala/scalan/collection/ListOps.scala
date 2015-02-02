package scalan.collection

import scala.reflect.ClassTag
import scalan.common.Default
import scalan.staged.BaseExp
import scalan.{Scalan, ScalanExp, ScalanSeq}
import scala.reflect.runtime.universe._

trait ListOps { self: Scalan =>
  type Lst[T] = Rep[List[T]]
  implicit class RepListOps[T: Elem](xs: Lst[T]) {
    def head = list_head(xs)
    def tail = list_tail(xs)

    def length = list_length(xs)
    def mapBy[R: Elem](f: Rep[T => R]) = list_map(xs, f)
    def map[R: Elem](f: Rep[T] => Rep[R]) = list_map(xs, fun(f))
    def reduce(implicit m: RepMonoid[T]) = list_reduce(xs)

    def foldLeft[S: Elem](init: Rep[S])(f: Rep[((S, T)) => S]): Rep[S] = list_foldLeft[T, S](xs, init, f)
    def foldRight[S: Elem](init: Rep[S])(f: Rep[((T,S)) => S]): Rep[S] = list_foldRight[T, S](xs, init, f)

    def scan(implicit m: RepMonoid[T]) = list_scan(xs)
    def zip[U](ys: Lst[U]): Lst[(T, U)] = list_zip(xs, ys)
    def filterBy(f: Rep[T => Boolean]) = list_filter(xs, f)
    def filter(f: Rep[T] => Rep[Boolean]) = list_filter(xs, fun(f))
    def ::(x: Rep[T]) = list_cons(x, xs)
    def :::(ys: Lst[T]) = list_concat(ys, xs)
    def reverse = list_reverse(xs)
    //def grouped(size: Rep[Int]) = list_grouped(xs, size)
    //def stride(start: Rep[Int], length: Rep[Int], stride: Rep[Int]) =
    //  list_stride(xs, start, length, stride)
    //def update(index: Rep[Int], value: Rep[T]) = list_update(xs, index, value)
    //def updateMany(indexes: Lst[Int], values: Lst[T]) = list_updateMany(xs, indexes, values)
  }

  class ListCompanion {
    def rangeFrom0(n: Rep[Int]): Lst[Int] = list_rangeFrom0(n)
    def tabulate[T: Elem](n: Rep[Int])(f: Rep[Int] => Rep[T]): Lst[T] =
      rangeFrom0(n).map(f)
    def replicate[T: Elem](len: Rep[Int], v: Rep[T]) = list_replicate(len, v)
    def empty[T: Elem] = replicate(0, element[T].defaultRepValue)
  }
  val List: ListCompanion

  case class ListElem[T](val eItem: Elem[T]) extends Element[List[T]] {
    override def isEntityType = eItem.isEntityType

    lazy val tag = {
      implicit val rt = eItem.tag
      weakTypeTag[List[T]]
    }

    protected def getDefaultRep = List.empty(eItem)
  }


  implicit def listElement[T](implicit eItem: Elem[T]): Elem[List[T]] = new ListElem[T](eItem)
  implicit def extendListElement[T](elem: Elem[List[T]]): ListElem[T] = elem.asInstanceOf[ListElem[T]]

  //-----------------------------------------------------
  // Primitives
  def list_head[T](xs: Lst[T]): Rep[T]
  def list_tail[T: Elem](xs: Lst[T]): Lst[T]

  def list_length[T](xs: Lst[T]): Rep[Int]

  // provide: xs.length == res.length
  def list_map[T, R: Elem](xs: Lst[T], f: Rep[T => R]): Lst[R]
  
  def list_reduce[T](xs: Lst[T])(implicit m: RepMonoid[T]): Rep[T]
  def list_foldLeft[T,S:Elem](xs: Lst[T], init:Rep[S], f:Rep[((S,T))=>S]): Rep[S]
  def list_foldRight[T,S:Elem](xs: Lst[T], init:Rep[S], f:Rep[((T,S))=>S]): Rep[S]

  // provide: res._1.length == xs.length && res._2 = list_reduce(xs)
  def list_scan[T](xs: Lst[T])(implicit m: RepMonoid[T], elem : Elem[T]): Rep[(List[T], T)]

  // require: xs.length == ys.length
  // provide: res.length == xs.length
  def list_zip[T, U](xs: Lst[T], ys: Lst[U]): Lst[(T, U)]
  
  // provide: res.length == len
  def list_replicate[T: Elem](len: Rep[Int], v: Rep[T]): Lst[T]
  
  // provide: res.length = n
  def list_rangeFrom0(n: Rep[Int]): Lst[Int]
  
  def list_filter[T](xs: Lst[T], f: Rep[T => Boolean]): Lst[T]

  def list_cons[T](x: Rep[T], xs: Lst[T]): Lst[T]

  def list_concat[T: Elem](xs: Lst[T], ys: Lst[T]): Lst[T]

  def list_reverse[T](xs: Lst[T]): Lst[T]
//  def list_grouped[T](xs: Lst[T], size: Rep[Int]): Lst[List[T]]
//
//  // require: start in xs.indices && start + length * stride in xs.indices
//  // provide: res.length == length
//  def list_stride[T](xs: Lst[T], start: Rep[Int], length: Rep[Int], stride: Rep[Int]): Lst[T]
//
//  // require: index in xs.indices
//  // provide: res.length == xs.length
//  def list_update[T](xs: Lst[T], index: Rep[Int], value: Rep[T]): Lst[T] = ???
//
//  // require: forall i -> indexes(i) in xs.indices && indexes.length == values.length
//  // provide: res.length == xs.length
//  def list_updateMany[T](xs: Lst[T], indexes: Lst[Int], values: Lst[T]): Lst[T] = ???
}

trait ListOpsSeq extends ListOps { self: ScalanSeq =>
  import TagImplicits.elemToClassTag

  class ListCompanion1 extends ListCompanion {
    @inline
    def apply[T: ClassTag](xs: T*) = scala.List(xs: _*)
  }
  val List: ListCompanion1 = new ListCompanion1

  def list_head[T](xs: Lst[T]): Rep[T] = xs.head
  def list_tail[T: Elem](xs: Lst[T]): Lst[T] = xs.tail

  def list_length[T](a: Lst[T]): Rep[Int] = a.length
  def list_map[T, R: Elem](xs: List[T], f: T => R) = xs.map(f)
  def list_reduce[T](xs: Lst[T])(implicit m: RepMonoid[T]) = xs.fold(m.zero)((x, y) => m.append((x, y)))
  def list_foldLeft[T, S: Elem](xs: Lst[T], init: Rep[S], f: Rep[((S, T)) => S]): Rep[S] = {
    var state = init
    for (x <- xs) {
      state = f((state, x))
    }
    state
  }
  def list_foldRight[T, S: Elem](xs: Lst[T], init: Rep[S], f: Rep[((T, S)) => S]): Rep[S] = {
    xs.foldRight(init)((t,s) => f((t,s)))
  }
  def list_zip[T, U](xs: List[T], ys: List[U]): List[(T, U)] = xs zip ys
  def list_scan[T](xs: List[T])(implicit m: RepMonoid[T], elem : Elem[T]): Rep[(List[T], T)] = {
    val scan = xs.scan(m.zero)((x, y) => m.append((x, y)))
    val sum = scan.last
    (scan.dropRight(1).toList, sum)
  }
  def list_replicate[T: Elem](len: Rep[Int], v: Rep[T]): Lst[T] = scala.List.fill(len)(v)
  def list_rangeFrom0(n: Rep[Int]): Lst[Int] = 0.until(n).toList
  def list_filter[T](xs: List[T], f: T => Boolean): List[T] =xs.filter(f)
  def list_cons[T](x: Rep[T], xs: Lst[T]): Lst[T] = x :: xs
  def list_concat[T: Elem](xs: List[T], ys: List[T]) = xs ::: ys
  def list_reverse[T](xs: Lst[T]): Lst[T] = xs.reverse

//  def list_grouped[T](xs: Lst[T], size: Rep[Int]): Lst[List[T]] = {
//    implicit val ct = listToClassTag(xs)
//    xs.iterator.grouped(size).map(_.toList).toList
//  }
//  def list_stride[T](xs: Lst[T], start: Rep[Int], length: Rep[Int], stride: Rep[Int]): Lst[T] = {
//    implicit val ct = listToClassTag(xs)
//    scala.List.tabulate(length) { i =>
//      xs(start + i * stride)
//    }
//  }
//  def listToClassTag[T](xs: Rep[List[T]]): ClassTag[T] = ClassTag(xs.getClass.getComponentType)
}

trait ListOpsExp extends ListOps with BaseExp { self: ScalanExp =>
  def withElemOfList[T, R](xs: Lst[T])(block: Elem[T] => R): R =
    withElemOf(xs) { eTLst =>
      block(eTLst.eItem)
    }

  trait ListDef[T] extends Def[List[T]] {
    implicit def eT: Elem[T]
    lazy val selfType = element[List[T]]
    lazy val uniqueOpId = name(eT)
  }
  trait ListMethod[T] {
    def name[A](e: Elem[A]): String
    def xs: Exp[List[T]]
    lazy val uniqueOpId = withElemOfList(xs) { name(_) }
  }
  case class ListHead[T](xs: Exp[List[T]]) extends Def[T] {
    def uniqueOpId = name(xs.elem.eItem)
    def selfType = xs.elem.eItem
    override def mirror(t: Transformer) = ListHead(t(xs))
  }
  case class ListTail[T](xs: Exp[List[T]])(implicit val eT: Elem[T]) extends ListDef[T] {
    override def mirror(t: Transformer) = ListTail(t(xs))
  }
  case class ListLength[T](xs: Exp[List[T]]) extends Def[Int] with ListMethod[T] {
    def selfType = element[Int]
    override def mirror(t: Transformer) = ListLength(t(xs))
  }
  case class ListMap[T, R](xs: Exp[List[T]], f: Exp[T => R]) extends ListDef[R] {
    implicit lazy val eT = withResultElem(f) { e => e }
    override def mirror(t: Transformer) = ListMap(t(xs), t(f))
  }
  case class ListReduce[T](xs: Exp[List[T]], implicit val m: RepMonoid[T]) extends Def[T] with ListMethod[T] {
    def selfType = xs.elem.eItem
    override def mirror(t: Transformer) = ListReduce[T](t(xs), m)
  }
  case class ListFoldLeft[T,S:Elem](xs: Exp[List[T]], init:Exp[S], f:Exp[((S,T))=>S]) extends BaseDef[S] with ListMethod[T] {
    override def mirror(t: Transformer) = ListFoldLeft(t(xs), t(init), t(f))
  }
  case class ListFoldRight[T,S:Elem](xs: Exp[List[T]], init:Exp[S], f:Exp[((T,S))=>S]) extends BaseDef[S] with ListMethod[T] {
    override def mirror(t: Transformer) = ListFoldRight(t(xs), t(init), t(f))
  }
  case class ListScan[T](xs: Exp[List[T]], implicit val m: RepMonoid[T])(implicit val selfType: Elem[(List[T], T)]) extends Def[(List[T], T)] with ListMethod[T] {
    override def mirror(t: Transformer) = ListScan[T](t(xs), m)
  }
  case class ListZip[T: Elem, U: Elem](xs: Exp[List[T]], ys: Exp[List[U]]) extends ListDef[(T, U)] {
    lazy val eT = element[(T, U)]
    override def mirror(t: Transformer) = ListZip(t(xs), t(ys))
  }
  case class ListReplicate[T](len: Exp[Int], v: Exp[T])(implicit val eT: Elem[T]) extends ListDef[T] {
    override def mirror(t: Transformer) = ListReplicate(t(len), t(v))
  }
  case class ListRangeFrom0(n: Exp[Int]) extends ListDef[Int] {
    def eT = element[Int]
    override def mirror(t: Transformer) = ListRangeFrom0(t(n))
  }
  case class ListFilter[T](xs: Exp[List[T]], f: Exp[T => Boolean])(implicit val eT: Elem[T]) extends ListDef[T] {
    override def mirror(t: Transformer) = ListFilter(t(xs), t(f))
  }
  case class ListCons[T](x: Exp[T], xs: Exp[List[T]])(implicit val eT: Elem[T]) extends ListDef[T] {
    override def mirror(t: Transformer) = ListCons(t(x), t(xs))
  }
  case class ListConcat[T](xs: Exp[List[T]], ys: Exp[List[T]])(implicit val eT: Elem[T]) extends ListDef[T] {
    override def mirror(t: Transformer) = ListConcat(t(xs), t(ys))
  }
  case class ListReverse[T](xs: Exp[List[T]])(implicit val eT: Elem[T]) extends ListDef[T] {
    override def mirror(t: Transformer) = ListReverse(t(xs))
  }

  val List: ListCompanion = new ListCompanion

  def list_head[T](xs: Lst[T]): Rep[T] = ListHead(xs)
  def list_tail[T: Elem](xs: Lst[T]): Lst[T] = ListTail(xs)

  def list_length[T](a: Exp[List[T]]): Rep[Int] = ListLength(a)
  def list_map[T, R: Elem](xs: Exp[List[T]], f: Exp[T => R]) = ListMap(xs, f)

  def list_reduce[T](xs: Lst[T])(implicit m: RepMonoid[T]) =
    withElemOfList(xs) { implicit eT => ListReduce(xs, m) }
  def list_foldLeft[T,S:Elem](xs: Lst[T], init:Rep[S], f:Rep[((S,T))=>S]): Rep[S] =
    withElemOfList(xs) { implicit eT => ListFoldLeft(xs, init, f) }
  def list_foldRight[T,S:Elem](xs: Lst[T], init:Rep[S], f:Rep[((T,S))=>S]): Rep[S] =
    withElemOfList(xs) { implicit eT => ListFoldRight(xs, init, f) }

  def list_scan[T](xs: Lst[T])(implicit m: RepMonoid[T], elem : Elem[T]): Rep[(List[T], T)] =
    ListScan(xs, m)

  def list_zip[T, U](xs: Lst[T], ys: Lst[U]): Lst[(T, U)] = {
    implicit val eT = xs.elem.eItem
    implicit val eU = ys.elem.eItem
    ListZip(xs, ys)
  }

  def list_replicate[T: Elem](len: Rep[Int], v: Rep[T]): Lst[T] =
    ListReplicate(len, v)

  def list_rangeFrom0(n: Rep[Int]): Lst[Int] =
    ListRangeFrom0(n)

  def list_filter[T](xs: Lst[T], f: Rep[T => Boolean]): Lst[T] =
    withElemOfList(xs) { implicit eT => ListFilter(xs, f) }

  def list_cons[T](x: Rep[T], xs: Lst[T]): Lst[T] =
    withElemOf(x) { implicit eT => ListCons(x, xs) }

  def list_concat[T: Elem](xs: Lst[T], ys: Lst[T]): Lst[T] =
    ListConcat(xs, ys)

  def list_reverse[T](xs: Lst[T]): Lst[T] =
    withElemOfList(xs) { implicit eT => ListReverse(xs) }

  override def rewriteDef[T](d: Def[T]) = d match {
    case ListLength(Def(d2: Def[List[a]]@unchecked)) =>
      d2.asDef[List[a]] match {
        case Const(scalaList) => toRep(scalaList.length)
        case ListMap(xs, _) =>
          implicit val eT = xs.elem.eItem
          xs.length
        case ListZip(xs, _) =>
          implicit val eT = xs.elem.eItem
          xs.length
        case ListReplicate(length, _) => length
        case ListRangeFrom0(n) => n
        case _ =>
          super.rewriteDef(d)
      }
    case ListReplicate(Def(Const(len)), Def(c : Const[a] @unchecked)) => {
      implicit val eA = c.selfType
      Const(scala.List.fill(len)(c.x))
    }
    case ListMap(xs, Def(l: Lambda[_, _])) if l.isIdentity => xs
    case ListMap(Def(d2), f: Rep[Function1[a, b]]@unchecked) =>
      d2.asDef[List[a]] match {
        case ListMap(xs: Rep[List[c]]@unchecked, g) =>
          val xs1 = xs.asRep[List[c]]
          val g1 = g.asRep[c => a]
          implicit val eB = f.elem.eRange
          implicit val eC = xs.elem.eItem
          xs1.map { x => f(g1(x))}
        case ListReplicate(length, x) =>
          implicit val eB = f.elem.eRange
          List.replicate(length, f(x))
        case _ =>
          super.rewriteDef(d)
      }
    case ListFilter(Def(d2: Def[List[a]]@unchecked), f) =>
      d2.asDef[List[a]] match {
        case ListFilter(xs, g) =>
          implicit val eT = xs.elem.eItem
          xs.filter { x => f(x) && g(x)}
        case _ =>
          super.rewriteDef(d)
      }
    case _ => super.rewriteDef(d)
  }
}
