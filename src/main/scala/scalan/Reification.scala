/**
 * Author: Alexander Slesarenko
 * Date: 7/24/12
 */
package scalan

trait Reification { self: Scalan =>

  trait ReifiableObject[+A] {     // in place of Def[A]
    def thisSymbol: Rep[A] = !!!("should not be called")
    def name = getClass.getSimpleName
    def mirror(f: Transformer): Rep[_] = !!!("don't know how to mirror " + this)
    def decompose: Option[Rep[_]] = None
    def isScalarOp: Boolean = true
    //def onAddedInLambda(lam: Rep[_]): Unit = { }
  }

  abstract class Transformer { outer =>
    type Self = this.type
    def apply[A](x: Rep[A]): Rep[A]
    def isDefinedAt(x: Rep[_]): Boolean
    def domain: Set[Rep[_]]
    protected def add(self: Self, kv: (Rep[_], Rep[_])): Self
    def +(kv: (Rep[_], Rep[_])) = add(this, kv)
    def ++(kvs: Map[Rep[_], Rep[_]]) = kvs.foldLeft(this: Self)((ctx, kv) => add(ctx,kv))
    def merge(other: Self): Self = other.domain.foldLeft[Self](outer)((t,s) => t + (s, other(s)))
    def apply[A](xs: List[Rep[A]]): List[Rep[A]] = xs map (e => apply(e))
    def apply[A](xs: Seq[Rep[A]]): Seq[Rep[A]] = xs map (e => apply(e))
    def apply[X,A](f: X=>Rep[A]): X=>Rep[A] = (z:X) => apply(f(z))
    def apply[X,Y,A](f: (X,Y)=>Rep[A]): (X,Y)=>Rep[A] = (z1:X,z2:Y) => apply(f(z1,z2))
  }
  trait CanBeReified[C[_]] {
    def resolve[A](sym: Rep[C[A]]): C[A]
  }

//  implicit val parrayCanBeReified: CanBeReified[PArray]
//  implicit val pipeCanBeReified: CanBeReified[Pipe]
//  implicit val chunksCanBeReified: CanBeReified[Chunks]
//
//  def resolvePArray[A](s: PA[A]): PArray[A]
//  def resolvePipe[A](s: P[A]): Pipe[A]
//  def resolveChunks[A](s: Ch[A]): Chunks[A]

  //TODO
  implicit def reifyObject[A:Elem](obj: ReifiableObject[A]): Rep[A]
  implicit def toRep[A:Elem](x: A): Rep[A] = ??? //element[A].toRep(x)

}
