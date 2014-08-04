package scalan.monads

import scalan.common.Default
import scalan._

/**
* User: Alexander Slesarenko
* Date: 8/2/14
*/
trait Monads { self: MonadsDsl =>

  type MonadRep[A] = Rep[Monad[A]]
  sealed trait Monad[A] extends UserType[Monad[A]] {
    implicit def elem: Elem[A]
    def flatMap[B:Elem](f: Rep[A] => Rep[Monad[B]]): Rep[Monad[B]]
    def map[B:Elem](f: Rep[A] => Rep[B]): Rep[Monad[B]] = flatMap(x => companion.point(f(x)))
    def filter(p: Rep[A] => Rep[Boolean]): Rep[Monad[A]] = self
    def run: Rep[A]
    def companion: Rep[MonadCompanion]
  }
  trait MonadCompanion extends TypeFamily1[Monad] {
    override def defaultOf[A](implicit ea: Elem[A]) = IdMonad.defaultOf[A]
    def point[A:Elem](value: Rep[A]): Rep[Monad[A]] = !!!
  }

  abstract class IdMonad[A](val value: Rep[A])(implicit val elem: Elem[A]) extends Monad[A] {
    def companion = IdMonad
    override def flatMap[B:Elem](f: Rep[A] => Rep[Monad[B]]) = f(value)
    override def run = value
  }
  trait IdMonadCompanion extends ConcreteClass1[IdMonad] with MonadCompanion {
    override def defaultOf[A](implicit ea: Elem[A]) = Default.defaultVal(IdMonad(ea.defaultRepValue))
    override def point[A:Elem](value: Rep[A]): Rep[Monad[A]] = IdMonad(value)
  }

  abstract class StateMonad[S,A](val step: Rep[S => (S,A)])(implicit val elem: Elem[A], val eS: Elem[S]) extends Monad[A] {
    def companion = StateMonad
    override def flatMap[B:Elem](f: Rep[A] => Rep[Monad[B]]) =
      StateMonad(fun { (s: Rep[S]) =>
        val Pair(s1, a) = step(s)
        val b = f(a)
        b.asInstanceOf[Rep[StateMonad[S,B]]].runFrom(s1)
      })

    override def run = ???

    def runFrom(s: Rep[S]): Rep[(S,A)] = step(s)
  }
  trait StateMonadCompanion extends ConcreteClass2[StateMonad] with MonadCompanion {
    override def defaultOf[S, A](implicit eS: Elem[S], eA: Elem[A]) = ???
    override def point[A: Elem](value: Rep[A]) = {
      implicit val eAnyRef = AnyRefElement
      StateMonad(fun({ (x: Rep[AnyRef]) => Pair(x, value)}))
    }

    def get[S:Elem]: Rep[StateMonad[S,S]] = StateMonad(fun { s => Pair(s, s)})
    def put[S:Elem](s: Rep[S]): Rep[StateMonad[S,Unit]] = StateMonad(fun { _ => Pair(s, ())})
    def modify[S:Elem](f: Rep[S] => Rep[S]) = StateMonad[S,Unit](fun { s => Pair(f(s), ())})
  }

  abstract class IOMonad[A](val step: Rep[Int => (Int,A)])(implicit val elem: Elem[A]) extends Monad[A] {
    def companion = IOMonad
    override def flatMap[B:Elem](f: Rep[A] => Rep[Monad[B]]) =
      IOMonad(fun { (s: Rep[Int]) =>
        val Pair(s1, a) = step(s)
        val b = f(a)
        b.asInstanceOf[Rep[IOMonad[B]]].step(s1)
      })
    override def run = ???
    def runIO = step(0)
  }
  trait IOMonadCompanion extends ConcreteClass1[IOMonad] with MonadCompanion {
    override def defaultOf[A](implicit ea: Elem[A]) = ??? // Default.defaultVal(IdMonad(ea.defaultRepValue))
    override def point[A: Elem](value: Rep[A]) = {
      IOMonad(fun({ (x: Rep[Int]) => Pair(x, value)}))
    }
    def println(s: Rep[String]) = IOMonad(fun({ (x: Rep[Int]) => doPrintln(x, s)}))
  }

}


trait MonadsDsl extends ScalanDsl with impl.MonadsAbs with Monads {
  def doPrintln(i: Rep[Int], s: Rep[String]): Rep[(Int,Unit)]
}

trait MonadsDslSeq extends MonadsDsl with impl.MonadsSeq with ScalanSeqImplementation {
  override def doPrintln(i: Rep[Int], s: Rep[String]) = (i + 1, println(s))
}

trait MonadsDslExp extends MonadsDsl with impl.MonadsExp with ScalanStaged {

  case class Println(i: Rep[Int], s: Rep[String]) extends BaseDef[(Int, Unit)]  {
    override def uniqueOpId = name(selfType)
    override def mirror(t: Transformer) = Println(t(i), t(s))
  }

  override def doPrintln(i: Rep[Int], s: Rep[String]) = Println(i, s)
}