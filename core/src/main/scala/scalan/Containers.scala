package scalan

import scala.annotation.implicitNotFound
import scala.reflect.runtime.universe._

trait Containers { self: Scalan =>

  type Cont[F[_]] = Container[F]

  @implicitNotFound(msg = "No Container available for ${F}.")
  trait Container[F[_]] {
    def tag[T](implicit tT: WeakTypeTag[T]): WeakTypeTag[F[T]]
    def lift[T](implicit eT: Elem[T]): Elem[F[T]]

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

  trait Functor[F[_]] extends Container[F] {
    def map[A:Elem,B:Elem](a: Rep[F[A]])(f: Rep[A] => Rep[B]): Rep[F[B]]
  }
}

trait ContainersSeq extends Containers with Scalan { self: ScalanSeq =>

}
trait ContainersExp extends Containers with Scalan { self: ScalanExp =>

}
