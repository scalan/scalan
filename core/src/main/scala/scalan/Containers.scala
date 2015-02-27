package scalan

import scala.annotation.implicitNotFound
import scala.reflect.runtime.universe._

trait Containers { self: Scalan =>

  type Cont[F[_]] = Container[F]

  @implicitNotFound(msg = "No Container available for ${F}.")
  trait Container[F[_]] {
    def tag[T](implicit tT: WeakTypeTag[T]): WeakTypeTag[F[T]]
    def lift[T](implicit eT: Elem[T]): Elem[F[T]]
  }

  def container[F[_]: Cont] = implicitly[Cont[F]]

  implicit def containerElem[F[_]:Cont, A:Elem]: Elem[F[A]] = container[F].lift(element[A])

}

trait ContainersSeq extends Containers with Scalan { self: ScalanSeq =>

}
trait ContainersExp extends Containers with Scalan { self: ScalanExp =>

}
