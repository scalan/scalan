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
    lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant))
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
    implicit val eA = arr.elem.typeArgs("T")._1.asElem[A]
    lazy val selfType = element[ColOverArray[A]]
  }
  // elem for concrete class
  class ColOverArrayElem[A](val iso: Iso[ColOverArrayData[A], ColOverArray[A]])(implicit override val eA: Elem[A])
    extends ColElem[A, ColOverArray[A]]
    with ConcreteElem[ColOverArrayData[A], ColOverArray[A]] {
    override lazy val parent: Option[Elem[_]] = Some(colElement(element[A]))
    override lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant))

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
    lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant))
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
      implicit val eA = p.arr.elem.typeArgs("T")._1.asElem[A]
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
  val dump = "H4sIAAAAAAAAALVWX4gbRRj/ssk1d7nrv+hVrJaeZzyxSlJ9qXAPmrvmxJLeHd3SQizKZDOJ0+7urLOTM5HSxz7Yt+KT4ENBEORQpC8iIqIUxIe+i29KQRBF+mBRsPjN7M5mc03uzgf3YZiZne/f7/f7ZnfzN5gIBcyFDnGJX/aoJGVbz6uhLNmneavr0pO0/RO/Wbx+7a+PLTjQgL0sPMeE7BKXvUNbDZjll2oek2uCdSKDs4IwWYe9NV8y2S95elPCsXoUpqLCVEaFKcUWi3Uonu0H1O773Gde4qGys4e0Gbp5+LwgQUDFllSe39nRsCG6miK+Q0PJRSjhici+4nDXpY5k3K8wz+tK0nRppc5CiecPONx3BJXUXnZJGNLwLbgCuTpMUuWSJespve6vBQO/D+alIcW0lN/o/Bka6Dr7noR9cTprgUoFz+SZF3AhTYg8unuTt8wy5xPcgGL9ItkgFQzRqdhSML+Dlvv5MI3KZE8dpgPiXCIduoqWaiuPdYTUbSu49ZFeAJkgCFBML+hcygNoygk0ZQVNyaaCKe0Q9XJd8F4foieTBegpF8/t4MJ4oDW/VXr3gvPaPXvas5RxT9c4hT6OjtG0JgOR/O7M9fDuKzdOWFBoQIGF1WYoBXFkmugYr2ni+1zqdBMIieggX/Pj+NJRqngGIc01eatvyHa4FxAfPcXAziBTLnOYVIfV3v6Yn5EoI5UyoOZoDkFP6h3Xw8q2GgRu/9vLX13++fEfDlqQVSLsBSLlNotutylHS2GZuC6WY0kTHKMWIqZs7tGD83fZ6zeuSQsydcj0hvW11ryITC72BMxEFpFU77MT//y4ry2tmPixRZj4X+a//uaXOy/lLLCGcZrCAuwaFmWSk5Bd5m4MjxoflZCpqklBDTM9NR7esi5sk0NC6dO//t66dRwu6EK1EAweu9Ieuii++P4XT9H1TyyYbOheXXFJR6tQUXKShk4DJvkGFdF+foO4ajZSifkWbZOuaxo1jUlE6txYUgOqAFtEFWHvmfKnI35WuU9LK+ulP+3v39tU9Kj3jyCoRAgD557zVSFIfweMDbJqnN0GHHPFfXb16uwfH77xkG7MySaTHglKx/9DW5ou+j/bLqkxquvoYK2GBVTfLKpvDSnUEC2nwy+k7FLYzWcyMPC7IMGiVQNqruZSLzqrhiNpdCXMpENp40SJR8aBrcv59O+lj548/Nh9C/KnYKKNEgtHwjrR5F2/ZaDD75qkPblk9nLD0CFURBAv+dxtELyskToJh4wSu5K5lXPxfqQ/fOZS1etZIiYs5FBciDItv+pHTmXp2c8332a3n1nRF0galRSwZUgtirEOY2CzKLhh/e7i1ojGlQfuji1KSEUq70IwisUROtFVnUqzqsbFMZCp4XQye3lrzkvDJxHhnCkXBRdjjN9Vkfr2hknnzo8Rkx33CmJ55d4Hq8du37yj7+eC6jq8R/zkR2Sgk+S2jEEuqDyi/44UrpifasR/AbWZxY+nCgAA"
}
}

trait ColsModule extends scalanizer.collections.impl.ColsDefs with scala.wrappers.WrappersModule
