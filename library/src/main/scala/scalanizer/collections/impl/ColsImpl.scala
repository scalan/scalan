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
      implicit val eTo: Elem[To] = this
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
      (override val arr: Rep[WArray[A]])(implicit eA: Elem[A])
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

  implicit class ExtendedColOverArray[A](p: Rep[ColOverArray[A]])(implicit eA: Elem[A]) {
    def toData: Rep[ColOverArrayData[A]] = isoColOverArray(eA).from(p)
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
    implicit val eA = arr.elem.typeArgs("T")._1.asElem[A]
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
      def unapply(d: Def[_]): Option[(Rep[WArray[T]], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(arr, emT, _*), _) if receiver.elem == ColCompanionElem && method.getName == "fromArray" =>
          Some((arr, emT)).asInstanceOf[Option[(Rep[WArray[T]], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[WArray[T]], Elem[T]) forSome {type T}] = exp match {
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
  val dump = "H4sIAAAAAAAAALVWT2hcRRj/9u2mm2zS1kZTsVoa4zOlVXarlwo56CZNxLJNQl9pIRZl9u3sOvX9GefNxt0SCl560Jt4EnooCF6CIh4UERGLIB56Fy+CFARRpEgLBYvfzHvz9m26m8SDexjezH7zm+/7/X7fvLf5B4xEAqYjl3gkKPtUkrKjn6uRtJ0zYaPt0VO0uXHt79Iv71z+woL9a7CXReeZkG3iscu0UYO9i4Fksmv7OlrC8VoMV1FwlUFwdrJjrgaT57qcOt0gDJifIlR2RshuQ5hHLgjCORVbUnluZ6D+jQg1RgKXRjIUkYQn4/0VN/Q86koWBhXm+21J6h6t1FgkMf4hNwxcQSV1FjwSRTR6C65AoQajVEGydD6m590V3sN9MK9zgjCJaSncOP4s5brOri9hX5LOClepYEyR+TwU0hxRRLg3woaZjsc0aEy1tKcGhYBgDEzWLpF1UsFTWxVHCha0EGycE/dN0qLLGKLCi1hDRL2molpv73DgnKNfntdplHuslFNWyooV26GCKXsQ9eeqCDtdiH+5PEBHQTy7A4RBoItBw373ovvqXWfct9Tmji5vDDGODLGt1gFJ/OHs+9Htl6+ftKC0BiUWVeuRFMSVWY0NVSQIQqnTNUsFIloo1cwwqfQpVYxB6gr1sNE1Oruhz0mASAnnEyiSx1wmVbBa25/oMJBkVFFyakILyHla77A2VXurnHvdGxvfbPz6xE8HLMgr/3W4yMDmEXabcrRLFojnYTmWNIfjqaVYKSf06YGZ2+y16+9JC3I1yHX6fbRSv4RKznUETMQ7YpfeZyf/+XlfU1qJ8EOLMOd/Xfz2u99uvViwwOrnaQwLcBaxKJOchPxC6CX0qPExCbmqeiipYaKjxkNb5qVtckglPfr7n43vT8BFXag2guFjV95DiMkXPvzqabr6iQWja7pNlzzS0i5UkpyikbsGo+E6FfF6cZ146mmgE4sN2iRtr6+HE05iUaeHisqpImwOXYS9Z8ofj/VZDgNqL63ad5wfP9hU8qj/H0VSiRCGzj0XqkKQ7g4cG2bVOLUNOeZ2++zq1am/Pnr9Yd2Yo3UmfcLtE/+hLU0X/Z9tl9YY13WkN1fDLLpvCt23ghJqihayx89m9mW4m8nloIc7K8GiVUNqYdGjfhyrhsNZdiVMZI/Sm1MnHh5Gti7n03vzHz916PH7FhRPw0gTLRYNpHWkHraDhqEOX2mSduS8WSv0U4dUEUH89E23TvCyRukkHDRObEvmVc4n67H/8DedqV4/pWbCQg4mhait5VeCGFTaz3y5+Ta7eWxJXyBZVjLEliEzmUx8mBCbR8P1+3cXt0Y8Lj1wd2xxQuak8i4Mo1Qc4BNd1emsqmqcG0KZGs6kTy9tzXm+P9I4raBqloZjfK+KzLs3Sjt3ZoiZnKRXkMsrd68tH7/5+S19P5dU1+E9EqTfID2fpLdlQnJJpRB/eGV4xdRUI/4LBMrDDooKAAA="
}
}

