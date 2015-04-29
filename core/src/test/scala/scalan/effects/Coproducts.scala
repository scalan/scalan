package scalan.monads

import scalan._
import scala.reflect.runtime.universe._

trait Coproducts  { self: MonadsDsl =>

  type RCoproduct[F[_],G[_],A] = Rep[Coproduct[F,G,A]]
  sealed trait Coproduct[F[_], G[_], A] extends Reifiable[Coproduct[F,G,A]] {
    implicit def cF: Cont[F]
    implicit def cG: Cont[G]
    implicit def eA: Elem[A]
    def run: Rep[F[A] | G[A]]
  }
  trait CoproductCompanion

  abstract class CoproductImpl[F[_],G[_],A]
      (val run: Rep[Either[F[A],G[A]]])
      (implicit val cF: Cont[F], val cG: Cont[G], val eA: Elem[A])
    extends Coproduct[F,G,A]

  trait CoproductImplCompanion
}

trait CoproductsDsl extends ScalanDsl with impl.CoproductsAbs with Coproducts { self: MonadsDsl =>


  // TODO implement better way to create new Cont for existing UDTs (without code duplication)
  implicit def coproductCont[F[_]:Cont, H[_]:Cont]: Cont[({type f[x] = Coproduct[F,H,x]})#f]
    = new Container[({type f[x] = Coproduct[F,H,x]})#f] {
        def tag[T](implicit tT: WeakTypeTag[T]): WeakTypeTag[Coproduct[F,H,T]]  = {
          //implicit val tagT = tT
          implicit val tagF = container[F].tag(tT)
          implicit val tagG = container[H].tag(tT)
          weakTypeTag[CoproductImpl[F, H, T]].asInstanceOf[WeakTypeTag[Coproduct[F,H,T]]]
          //TODO how to make it simple like this "element[Coproduct[F,H,T]].tag"
        }

        def lift[T](implicit eT: Elem[T]) = element[Coproduct[F,H,T]]
      }

  trait ~>[F[_],G[_]] { self =>
    implicit def cIn: Cont[F]
    implicit def cOut: Cont[G]
    def apply[A:Elem](f: Rep[F[A]]): Rep[G[A]]

    def or[H[_]:Cont](f: H ~> G): ({ type f[x] = Coproduct[F, H, x]})#f ~> G =
      new (({type f[x] = Coproduct[F,H,x]})#f ~> G) {
        def cIn = {
          implicit val cF = self.cIn
          container[({type f[x] = Coproduct[F,H,x]})#f]
        }
        implicit def cOut = self.cOut

        def apply[A:Elem](c: Rep[Coproduct[F,H,A]]): Rep[G[A]] =
          c.run.fold(fa => self(fa),
                     ha => f(ha))
      }

    override def toString = s"${cIn.name} ~> ${cOut.name}"
  }

  sealed trait Inject[F[_],G[_]] {
    def inj[A:Elem](sub: Rep[F[A]]): Rep[G[A]]
    def prj[A:Elem](sup: Rep[G[A]]): Rep[Unit | F[A]]
  }

  object Inject {
    implicit def injRefl[F[_]] = new Inject[F,F] {
      def inj[A:Elem](sub: Rep[F[A]]) = sub
      def prj[A:Elem](sup: Rep[F[A]]) = toRight(sup)
    }

    implicit def injLeft[F[_]:Cont,G[_]:Cont] = new Inject[F,({type λ[α] = Coproduct[F,G,α]})#λ] {
      def inj[A:Elem](sub: Rep[F[A]]) = CoproductImpl(sub.asLeft[G[A]])
      def prj[A:Elem](sup: Rep[Coproduct[F,G,A]]) =
        sup.run.foldBy(fun { fa => toRight(fa) },
                       constFun(SOption.none[F[A]]))
    }

    implicit def injRight[F[_]:Cont,G[_]:Cont,H[_]:Cont](implicit I: Inject[F,G]) =
      new Inject[F,({type f[x] = Coproduct[H,G,x]})#f] {
        def inj[A:Elem](sub: Rep[F[A]]) = CoproductImpl(I.inj(sub).asRight[H[A]])
        def prj[A:Elem](sup: Rep[Coproduct[H,G,A]]) =
          sup.run.foldBy(constFun(SOption.none[F[A]]),
                         fun { ga => I.prj(ga) })
      }
  }

  def lift[F[_],G[_],A](f: Rep[F[A]])(implicit I: Inject[F,G], cF: Cont[F], cG: Cont[G], eA: Elem[A]): RFree[G,A] =
    Bind(Suspend(I.inj(f)), fun { Return(_: Rep[A]) })

  def matchCoproduct[F[_], G[_], T, R](ft: RCoproduct[F,G,T])
                           (ret: Rep[CoproductImpl[F,G,T]] => Rep[R]): Rep[R]
}

trait CoproductsDslSeq extends CoproductsDsl with impl.CoproductsSeq with ScalanCtxSeq { self: MonadsDslSeq =>
  def matchCoproduct[F[_], G[_], T, R](ft: RCoproduct[F,G,T])
                                      (ret: Rep[CoproductImpl[F,G,T]] => Rep[R]): Rep[R] =
    ft match {
      case c: CoproductImpl[F,G,T] @unchecked => ret(c.asInstanceOf[CoproductImpl[F,G,T]])
    }
}

trait CoproductsDslExp extends CoproductsDsl with impl.CoproductsExp with ScalanExp { self: MonadsDslExp =>

  def matchCoproduct[F[_], G[_], T, R](ft: RCoproduct[F,G,T])
                                      (ret: Rep[CoproductImpl[F,G,T]] => Rep[R]): Rep[R] =
    ft.elem.asInstanceOf[Elem[_]] match {
      case _: CoproductImplElem[_,_,_] => ret(ft.asRep[CoproductImpl[F,G,T]])
    }

}