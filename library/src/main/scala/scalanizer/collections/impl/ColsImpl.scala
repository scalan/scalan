package scalanizer.collections

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait ColsDefs extends scalan.Scalan with Cols {
  self: ColsModule =>

  // entityProxy: single proxy for each type family
  implicit def proxyCol[A](p: Rep[Col[A]]): Col[A] = {
    proxyOps[Col[A]](p)(scala.reflect.classTag[Col[A]])
  }

  // familyElem
  class ColElem[A, To <: Col[A]](implicit _eA: Elem[A])
    extends EntityElem[To] {
    def eA = _eA
    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[Col[A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[Col[A]] => convertCol(x) }
      tryConvert(element[Col[A]], this, x, conv)
    }

    def convertCol(x: Rep[Col[A]]): Rep[To] = {
      x.elem match {
        case _: ColElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have ColElem[_, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def colElement[A](implicit eA: Elem[A]): Elem[Col[A]] =
    cachedElem[ColElem[A, Col[A]]](eA)

  implicit case object ColCompanionElem extends CompanionElem[ColCompanionCtor] {
    lazy val tag = weakTypeTag[ColCompanionCtor]
    protected def getDefaultRep = Col
  }

  abstract class ColCompanionCtor extends CompanionDef[ColCompanionCtor] with ColCompanion {
    def selfType = ColCompanionElem
    override def toString = "Col"
  }
  implicit def proxyColCompanionCtor(p: Rep[ColCompanionCtor]): ColCompanionCtor =
    proxyOps[ColCompanionCtor](p)

  case class ColOverArrayCtor[A]
      (override val arr: Rep[WArray[A]])
    extends ColOverArray[A](arr) with Def[ColOverArray[A]] {
    implicit val eA = arr.eT
    lazy val selfType = element[ColOverArray[A]]
  }
  // elem for concrete class
  class ColOverArrayElem[A](val iso: Iso[ColOverArrayData[A], ColOverArray[A]])(implicit override val eA: Elem[A])
    extends ColElem[A, ColOverArray[A]]
    with ConcreteElem[ColOverArrayData[A], ColOverArray[A]] {
    override lazy val parent: Option[Elem[_]] = Some(colElement(element[A]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))

    override def convertCol(x: Rep[Col[A]]) = ColOverArray(x.arr)
    override def getDefaultRep = ColOverArray(element[WArray[A]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[ColOverArray[A]]
    }
  }

  // state representation type
  type ColOverArrayData[A] = WArray[A]

  // 3) Iso for concrete class
  class ColOverArrayIso[A](implicit eA: Elem[A])
    extends EntityIso[ColOverArrayData[A], ColOverArray[A]] with Def[ColOverArrayIso[A]] {
    override def from(p: Rep[ColOverArray[A]]) =
      p.arr
    override def to(p: Rep[WArray[A]]) = {
      val arr = p
      ColOverArray(arr)
    }
    lazy val eFrom = element[WArray[A]]
    lazy val eTo = new ColOverArrayElem[A](self)
    lazy val selfType = new ColOverArrayIsoElem[A](eA)
    def productArity = 1
    def productElement(n: Int) = eA
  }
  case class ColOverArrayIsoElem[A](eA: Elem[A]) extends Elem[ColOverArrayIso[A]] {
    def getDefaultRep = reifyObject(new ColOverArrayIso[A]()(eA))
    lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[ColOverArrayIso[A]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class ColOverArrayCompanionCtor extends CompanionDef[ColOverArrayCompanionCtor] with ColOverArrayCompanion {
    def selfType = ColOverArrayCompanionElem
    override def toString = "ColOverArrayCompanion"

    @scalan.OverloadId("fromFields")
    def apply[A](arr: Rep[WArray[A]]): Rep[ColOverArray[A]] =
      mkColOverArray(arr)

    def unapply[A](p: Rep[Col[A]]) = unmkColOverArray(p)
  }
  lazy val ColOverArrayRep: Rep[ColOverArrayCompanionCtor] = new ColOverArrayCompanionCtor
  lazy val ColOverArray: ColOverArrayCompanionCtor = proxyColOverArrayCompanion(ColOverArrayRep)
  implicit def proxyColOverArrayCompanion(p: Rep[ColOverArrayCompanionCtor]): ColOverArrayCompanionCtor = {
    proxyOps[ColOverArrayCompanionCtor](p)
  }

  implicit case object ColOverArrayCompanionElem extends CompanionElem[ColOverArrayCompanionCtor] {
    lazy val tag = weakTypeTag[ColOverArrayCompanionCtor]
    protected def getDefaultRep = ColOverArrayRep
  }

  implicit def proxyColOverArray[A](p: Rep[ColOverArray[A]]): ColOverArray[A] =
    proxyOps[ColOverArray[A]](p)

  implicit class ExtendedColOverArray[A](p: Rep[ColOverArray[A]]) {
    def toData: Rep[ColOverArrayData[A]] = {
      implicit val eA = p.arr.eT
      isoColOverArray(eA).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoColOverArray[A](implicit eA: Elem[A]): Iso[ColOverArrayData[A], ColOverArray[A]] =
    reifyObject(new ColOverArrayIso[A]()(eA))

  registerModule(ColsModule)

  lazy val Col: Rep[ColCompanionCtor] = new ColCompanionCtor {
  }

  object ColOverArrayMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[ColOverArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[ColOverArray[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ColOverArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[ColOverArray[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "apply" =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[ColOverArray[A]], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ColOverArray[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ColOverArrayCompanionMethods {
  }

  def mkColOverArray[A]
    (arr: Rep[WArray[A]]): Rep[ColOverArray[A]] = {
    new ColOverArrayCtor[A](arr)
  }
  def unmkColOverArray[A](p: Rep[Col[A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ColOverArrayElem[A] @unchecked =>
      Some((p.asRep[ColOverArray[A]].arr))
    case _ =>
      None
  }

  object ColMethods {
    object arr {
      def unapply(d: Def[_]): Option[Rep[Col[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ColElem[_, _]] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[Col[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Col[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[Col[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ColElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[Col[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Col[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[Col[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[ColElem[_, _]] && method.getName == "apply" =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[Col[A]], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Col[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ColCompanionMethods {
    object fromArray {
      def unapply(d: Def[_]): Option[Rep[WArray[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem == ColCompanionElem && method.getName == "fromArray" =>
          Some(arr).asInstanceOf[Option[Rep[WArray[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[WArray[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object ddmvm {
      def unapply(d: Def[_]): Option[Rep[WArray[Double]]] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem == ColCompanionElem && method.getName == "ddmvm" =>
          Some(v).asInstanceOf[Option[Rep[WArray[Double]]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[WArray[Double]]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object ColsModule extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWTWwbRRgdr53acdI/Q4pAoIbIBNFWdumliIiDcZ0KMHHUjVpkKqrxemym3d0ZZsfBRlWPlWi5UPWExKESJxQhQS+IAxeEVHHoHXEDcUIg1ANVD0X9ZnZnvU7tOBzYw2hmdr6/9943u1t/oplAoMXAwS72Sx6RuGTreSWQRftt1u655BTp/MpuF25cf/ClhQ400V4anKVC9rBLPyLtJlpgl2oelQ1Bu6HBhsBU1tHemi+pHBQ9vSnRkXoYpqzClMeFKUYWK3VU2BhwYg985lMv9lCe7iFpBm6ePCcw50RsS+Xl6Y5GDcHVLPYdEkgmAomeD+3LDnNd4kjK/DL1vJ7ELZeU6zSQcP6Aw3xHEEnsqouDgAQfoCsoU0c5olzSeD2r14MGH/p9PC8NKaSl/IbnzxCu6xx4Eu2L0mlwlQqcyVKPMyFNiCy4e5+1zTLjY9hAhfpFvInLEKJbtqWgfhcs97NRGpXJnjqa49i5hLtkDSzVVhbqCIjbUXDrI32OUpxzENMJnUtpCE0phqakoCnaRFClHaxergvWH6DwSaUR6isXx6a4MB5IzW8Xr5133r1vz3mWMu7rGmfBx+EJmtZkAJJ3ztwI7p2+ddJC+SbK06DSCqTAjkwSHeE1h32fSZ1uDCEWXeBraRJfOkoFzgCkmRZrDwzZDvM49sFTBOw8MOVSh0p1WO3tj/gZizJQKTkxRzMAelzvpB5WthXO3cEPl7+//NuzPx+0UFqJsM9Fwm0a3O5QjpZCFbsulGNJExyi5kOmbOaRg0v36Hu3rksLpeoo1R/VV6N1EZhc6Qs0H1qEUv2Xnnz4y76OtCLiJxZh4p94p9MqbXzysYWsUZxmoQC7BkWZ5CRKV5kbwaPGpyVKVdQkr4b5vhqf2bbO75BDTOmLf/zV/vE4Oq8L1UIweOxKe+Ci8Mpn371A1r+yUK6pe3XVxV2tQkXJKRI4TZRjm0SE+9lN7KrZWCVm26SDe65p1CQmIamLE0nlRAG2AiqC3jPlz4X8rDGfFFfXi//YP93cUvSo908BqFgIA+eecxUh8GAKxgZZNS7sAI654r6+enXh7y8uPKEbM9ei0sO8ePw/tKXpov+z7eIaw7oOD9dqWAb1LYD6GkChhqiaDL+csEtgt5RKRSzoQxJZpGJAzdRc4k3FWaL5ZFDtJtbkc5Ng14XlHn76wL557RsLZd9EMx0QWzAW4JkW6/ltAyJ84STpy9fNXmYURAANC+zFH75NDNc2kCjRIaPJnqRu+Wy0HyoRnsUhvsvx7FWj0ENRJcq29IYfepXFo99ufUjvvrSq75IkLAncSiixKESSjABNg/ZGId7FBRKOpx+7RraJIhGptAvtKBrHSEZX9VaSVjW+tgNma/Gssj3n6ujJPIjMlAvaizCGT6xIfIaDuImXJqjJjtoGsLxy//O1I3dv/66v6rxqQLhS/PifZCiU+OKMQM6rPMJfkASukJ/qyUc+ZpAKsgoAAA=="
}
}

trait ColsModule extends scalanizer.collections.impl.ColsDefs with scala.wrappers.WrappersModule
