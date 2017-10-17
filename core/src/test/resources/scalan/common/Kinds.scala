package scalan.common

import scalan.{Base, Scalan}

trait Kinds extends Base { self: KindsModule =>
  type RKind[F[_],A] = Rep[Kind[F,A]]

  sealed trait Kind[F[_], A] extends Def[Kind[F,A]] {
    implicit def eA: Elem[A]
    implicit def cF: Cont[F]

    def flatMap[B](f: Rep[A] => Rep[Kind[F,B]]): Rep[Kind[F,B]] = Bind(self, fun(f))

    def mapBy[B](f: Rep[A => B]): Rep[Kind[F,B]] =
      flatMap(a => Return(f(a)))
  }
  trait KindCompanion

  abstract class Return[F[_],A]
        (val a: Rep[A])
        (implicit val cF: Cont[F]) extends Kind[F,A]
  {
    override def flatMap[B](f: Rep[A] => Rep[Kind[F,B]]): Rep[Kind[F,B]] = f(a)
  }
  trait ReturnCompanion

  abstract class Bind[F[_],S,B]
        (val a: Rep[Kind[F, S]], val f: Rep[S => Kind[F,B]]) extends Kind[F,B] {
    val eA = f.elem.eRange.typeArgs("A")._1.asElem[B]
    override def flatMap[R](f1: Rep[B] => Rep[Kind[F,R]]): Rep[Kind[F,R]] = {
      a.flatMap((s: Rep[S]) => f(s).flatMap(f1))
    }
  }
  trait BindCompanion
}
