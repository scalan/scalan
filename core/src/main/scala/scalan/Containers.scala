package scalan

import scala.annotation.implicitNotFound
import scala.reflect.runtime.universe._

trait Containers { self: Scalan =>

  type SomeF[X] = F[X] forSome { type F[_] }
  type SomeCont = Cont[SomeF]

  // TODO Could have Elem and Cont extend TypeDesc directly, see which makes ProxyExp simpler
  sealed trait TypeDesc {
    def name: String
  }
  case class AnElem(elem: Elem[_]) extends TypeDesc {
    def name = elem.name
  }
  case class ACont(cont: SomeCont) extends TypeDesc {
    def name = cont.name
  }

  @implicitNotFound(msg = "No Cont available for ${F}.")
  trait Cont[F[_]] {
    def tag[T](implicit tT: WeakTypeTag[T]): WeakTypeTag[F[T]]
    def lift[T](implicit eT: Elem[T]): Elem[F[T]]
    def unlift[T](implicit eFT: Elem[F[T]]): Elem[T]
    def getElem[T](fa: Rep[F[T]]): Elem[F[T]]
    def getItemElem[T](fa: Rep[F[T]]): Elem[T] = unlift(getElem(fa))
    def unapply[T](e: Elem[_]): Option[Elem[F[T]]]

    protected def getName = {
      // note: will use WeakTypeTag[x], so x type parameter ends up in the result
      // instead of the actual type parameter it's called with (see below)
      def tpeA[x] = tag[x].tpe

      val tpe = tpeA[Nothing]

      val str = cleanUpTypeName(tpe)

      if (str.endsWith("[x]"))
        str.stripSuffix("[x]")
      else
        "[x]" + str
    }
    lazy val name = getName

    override def toString = s"${getClass.getSimpleName}{$name}"
    def isFunctor = this.isInstanceOf[Functor[F]]
  }

  def container[F[_]: Cont] = implicitly[Cont[F]]

  implicit def containerElem[F[_]:Cont, A:Elem]: Elem[F[A]] = container[F].lift(element[A])

  trait Functor[F[_]] extends Cont[F] {
    def map[A:Elem,B:Elem](a: Rep[F[A]])(f: Rep[A] => Rep[B]): Rep[F[B]]
  }
}

trait ContainersStd extends Containers { self: ScalanStd =>

}
trait ContainersExp extends Containers { self: ScalanExp =>

}
