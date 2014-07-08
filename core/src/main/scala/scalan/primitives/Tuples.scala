/**
 * Author: Alexander Slesarenko
 * Date: 7/25/12
 */
package scalan.primitives

import scalan.common.OverloadHack._
import scalan.staged.BaseExp
import scalan.{ScalanStaged, Scalan, ScalanSeq}

trait Tuples { self: Scalan =>
  implicit object Pair {
    def apply[A,B](a: Rep[A], b: Rep[B]) = zipPair[A,B]((a,b))
    implicit def unapply[A,B](p: Rep[(A,B)]) = Some(unzipPair[A,B](p))
  }
  def unzipPair[A, B](p: Rep[(A, B)]): (Rep[A], Rep[B])
  implicit def zipPair[A, B](p: (Rep[A], Rep[B])): Rep[(A, B)]

  implicit class PairOps[A, B](p: Rep[(A, B)]) {
    def _1: Rep[A] = { val (a, _) = unzipPair[A,B](p); a }
    def _2: Rep[B] = { val (_, b) = unzipPair[A,B](p); b }
  }
  //implicit def pimpPair[A, B](p: Rep[(A, B)]): PairOps[A, B] = new PairOps[A,B](p)

  implicit class PairOps3[A,B,C](t: Rep[(A,(B,C))]) {
    def _1: Rep[A] = { val (a, _) = unzipPair(t); a }
    def _2: Rep[B] = { val (b, _) = unzipPair(unzipPair(t)._2); b }
    def _3: Rep[C] = { val (_, c) = unzipPair(unzipPair(t)._2); c }
  }
  //implicit def pimpPair3[A, B, C](p: Rep[(A, (B,C))]): PairOps3[A, B, C] = new PairOps3(p)

  implicit class PairOps4[A,B,C,D](t: Rep[(A,(B,(C,D)))]) {
    def _1: Rep[A] = { val Pair(x, _) = t; x }
    def _2: Rep[B] = { val Pair(_, Pair(x, _)) = t; x }
    def _3: Rep[C] = { val Pair(_, Pair(_, Pair(x, _))) = t; x }
    def _4: Rep[D] = { val Pair(_, Pair(_, Pair(_, x))) = t; x }
  }
  //implicit def pimpPair4[A,B,C,D](p: Rep[(A,(B,(C,D)))]) = new PairOps4(p)

  implicit class PairOps5[A,B,C,D,E](t: Rep[(A,(B,(C,(D,E))))]) {
    def _1: Rep[A] = { val Pair(x, _) = t; x }
    def _2: Rep[B] = { val Pair(_, Pair(x, _)) = t; x }
    def _3: Rep[C] = { val Pair(_, Pair(_, Pair(x, _))) = t; x }
    def _4: Rep[D] = { val Pair(_, Pair(_, Pair(_, Pair(x, _)))) = t; x }
    def _5: Rep[E] = { val Pair(_, Pair(_, Pair(_, Pair(_, x)))) = t; x }
  }
  //implicit def pimpPair5[A,B,C,D,E](p: Rep[(A,(B,(C,(D,E))))]) = new PairOps5(p)

  implicit class PairOps6[A,B,C,D,E,F](t: Rep[(A,(B,(C,(D,(E,F)))))]) {
    def _1: Rep[A] = { val Pair(x, _) = t; x }
    def _2: Rep[B] = { val Pair(_, Pair(x, _)) = t; x }
    def _3: Rep[C] = { val Pair(_, Pair(_, Pair(x, _))) = t; x }
    def _4: Rep[D] = { val Pair(_, Pair(_, Pair(_, Pair(x, _)))) = t; x }
    def _5: Rep[E] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _))))) = t; x }
    def _6: Rep[F] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, x))))) = t; x }
  }
  //implicit def pimpPair6[A,B,C,D,E,F](p: Rep[(A,(B,(C,(D,(E,F)))))]) = new PairOps6(p)

  implicit class PairOps7[A,B,C,D,E,F,G](t: Rep[(A,(B,(C,(D,(E,(F,G))))))]) {
    def _1: Rep[A] = { val Pair(x, _) = t; x }
    def _2: Rep[B] = { val Pair(_, Pair(x, _)) = t; x }
    def _3: Rep[C] = { val Pair(_, Pair(_, Pair(x, _))) = t; x }
    def _4: Rep[D] = { val Pair(_, Pair(_, Pair(_, Pair(x, _)))) = t; x }
    def _5: Rep[E] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _))))) = t; x }
    def _6: Rep[F] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _)))))) = t; x }
    def _7: Rep[G] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, x)))))) = t; x }
  }
  //implicit def pimpPair7[A,B,C,D,E,F,G](p: Rep[(A,(B,(C,(D,(E,(F,G))))))]) = new PairOps7(p)

  implicit def zipTuple3[A, B, C](p: (Rep[A], Rep[B], Rep[C])): Rep[(A,(B,C))] = Pair(p._1, Pair(p._2, p._3))
  implicit def zipTuple4[A, B, C, D](p: (Rep[A], Rep[B], Rep[C], Rep[D])): Rep[(A, (B, (C, D)))] = Pair(p._1, Pair(p._2, Pair(p._3, p._4)))
  implicit def zipTuple5[A, B, C, D, E](p: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E])): Rep[(A, (B, (C, (D,E))))] = Pair(p._1, Pair(p._2, Pair(p._3, Pair(p._4, p._5))))
  implicit def zipTuple6[A, B, C, D, E, F](p: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F])): Rep[(A, (B, (C, (D,(E,F)))))] = Pair(p._1, Pair(p._2, Pair(p._3, Pair(p._4, Pair(p._5, p._6)))))
  implicit def zipTuple7[A, B, C, D, E, F,G](p: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F], Rep[G])): Rep[(A, (B, (C, (D,(E,(F,G))))))] = Pair(p._1, Pair(p._2, Pair(p._3, Pair(p._4, Pair(p._5, Pair(p._6, p._7))))))

  object Tuple {
    def unapply[A,B,C](p: Rep[(A,(B,C))]) = Some((p._1, p._2, p._3))
    def unapply[A,B,C,D](p: Rep[(A, (B, (C, D)))])(implicit o: Overloaded1) = Some((p._1, p._2, p._3, p._4))
    def unapply[A,B,C,D,E](p: Rep[(A, (B, (C, (D,E))))])(implicit o: Overloaded2) = Some((p._1, p._2, p._3, p._4, p._5))
    def unapply[A,B,C,D,E,F](p: Rep[(A, (B, (C, (D, (E, F)))))])(implicit o: Overloaded3) = Some((p._1, p._2, p._3, p._4, p._5, p._6))
    def unapply[A,B,C,D,E,F,G](p: Rep[(A, (B, (C, (D, (E, (F,G))))))])(implicit o: Overloaded4) = Some((p._1, p._2, p._3, p._4, p._5, p._6, p._7))
  }
}

trait TuplesSeq extends Tuples  { self: ScalanSeq =>
  def unzipPair[A, B](p: Rep[(A, B)]): (Rep[A], Rep[B]) = p
  def zipPair[A, B](p: (Rep[A], Rep[B])): Rep[(A, B)] = p
}

trait TuplesExp extends Tuples with BaseExp {  self: ScalanStaged =>

  def unzipPair[A, B](p: Rep[(A, B)]): (Rep[A], Rep[B]) = p match {
    case Def(Tup(a, b)) => (a, b)
    case _ => p.elem match {
      case pe: PairElem[_, _] =>
        implicit val eA = pe.ea
        implicit val eB = pe.eb
        (First(p), Second(p))
      case _ =>
        !!!("expected Tup[A,B] or Sym with type (A,B) but was " + p.toString, p)
    }
  }

  implicit def zipPair[A, B](p: (Exp[A], Exp[B])): Rep[(A, B)] = {
    implicit val ea = p._1.elem.asInstanceOf[Elem[A]]
    implicit val eb = p._2.elem.asInstanceOf[Elem[B]]
    Tup(p._1, p._2)
  }


  case class Tup[A:Elem, B:Elem](a: Exp[A], b: Exp[B]) extends Def[(A, B)] {
    override def mirror(t: Transformer) = Tup(t(a), t(b))
    override def self = this
    lazy val selfType = element[(A,B)]
    override def format = s"($a, $b)"
  }

  case class First[A, B](pair: Exp[(A, B)])(implicit val selfType: Elem[A]) extends Def[A] {
    override def mirror(t: Transformer) = First(t(pair))
    override def self = this
    override def format = s"$pair._1"
  }

  case class Second[A, B](pair: Exp[(A, B)])(implicit val selfType: Elem[B]) extends Def[B] {
    override def mirror(t: Transformer) = Second(t(pair))
    override def self = this
    override def format = s"$pair._2"
  }


  override def rewrite[T](d: Exp[T])(implicit eT: LElem[T]) = d match {
    case Def(First(Def(Tup(a, b)))) => a
    case Def(Second(Def(Tup(a, b)))) => b

    case _ => super.rewrite(d)
  }
}
