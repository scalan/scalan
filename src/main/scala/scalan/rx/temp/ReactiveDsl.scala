package scalan.rx.temp

/**
 * User: Alexander Slesarenko   
 * Date: 12/1/13
 */
trait ReactiveDsl extends Reactive {

  // single proxy for each type family
  implicit def proxyObservable[T:Elem](p: Obs[T]): Observable[T] = {
    implicit val mA = element[T].manifest;
    proxyOps[Observable[T], Observable[T]](p)
  }

  // root of elem family
  trait ObservableElem[A,B] extends ViewElem[A, B]

  trait ObservableImplElem[T] extends ObservableElem[ObservableImplData[T], ObservableImpl[T]]

  // 2) state representation type
  type ObservableImplData[A] = (A, (Int, Boolean))

  // 3) companion object with Iso, constructor and deconstructor
  object ObservableImpl {
    abstract class Iso[T](implicit val eT: Elem[T]) extends IsoBase[ObservableImplData[T], ObservableImpl[T]] {
      override def fromStaged = { case ObservableImpl(v,i,c) => Pair(v, Pair(i,c)) }
      override def toStaged = (p: Rep[(T, (Int, Boolean))]) => {
        val Pair(v, Pair(i, c)) = p
        ObservableImpl(v, i, c)
      }
      def manifest = { implicit val mT = eT.manifest; Predef.manifest[ObservableImpl[T]] }
      def zero = Common.zero(ObservableImpl(eT.zero.zero, -1, true))
    }

    def apply[T:Elem](value: Rep[T], index: Rep[Int], completed: Rep[Boolean]): Rep[ObservableImpl[T]]  = mkObservableImpl(value, index, completed)
    def unapply[T:Elem](p: Rep[ObservableImpl[T]]) = unmkObservableImpl(p)
  }

  // 4) implicit resolution of Iso
  implicit def isoObservableImpl[T:Elem]: Iso[ObservableImplData[T], ObservableImpl[T]]

  // 5) smart constructor and deconstructor
  def mkObservableImpl[T:Elem](value: Rep[T], index: Rep[Int], completed: Rep[Boolean]): Rep[ObservableImpl[T]]
  def unmkObservableImpl[T:Elem](p: Rep[ObservableImpl[T]]): Option[(Rep[T], Rep[Int], Rep[Boolean])]

}
