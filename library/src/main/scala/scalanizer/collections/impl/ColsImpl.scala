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
      (override val arr: Rep[WArray[A]])
    extends ColOverArray[A](arr) with Def[ColOverArray[A]] {
    implicit val eA = arr.elem.asInstanceOf[WArrayElem[A, _]].eT
    lazy val selfType = element[ColOverArray[A]]
  }
  // elem for concrete class
  class ColOverArrayElem[A](val iso: Iso[ColOverArrayData[A], ColOverArray[A]])(implicit override val eA: Elem[A])
    extends ColElem[A, ColOverArray[A]]
    with ConcreteElem[ColOverArrayData[A], ColOverArray[A]] {
    override lazy val parent: Option[Elem[_]] = Some(colElement(element[A]))
    override lazy val typeArgs = TypeArgs()

    override def convertCol(x: Rep[Col[A]]) = ColOverArray(x.arr)
    override def getDefaultRep = ColOverArray(element[WArray[A]].defaultRepValue)
    override lazy val tag = {
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
      weakTypeTag[ColOverArrayIso[A]]
    }
    lazy val typeArgs = TypeArgs()
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
      implicit val eA = p.elem.asInstanceOf[ColOverArrayElem[A]].eA
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
  val dump = "H4sIAAAAAAAAALVWS4gcRRiu6dndeS1LdjXL+l7XWXxmJhhDlEHWyeyOJEx2lnR0YQxKTXfNbMd+lNU1a7eEeMtBD4KKB8FDQMnBxSBeRETEB4hIDt7EowQEUSQHA4LBv6of0zOZ2awH51BU1fz1P77v+6t653c06TK06GrYxHbJIhyXVDmvuryonnD0nklWSeeND3eesg5vvaWguRaa2sLuqmu2UC6YrHk0nqtcb6DZumHrazY3uF+0pAuOSo0gRlnEKI+KUUycqjTQglhuMkwpYUO+Du/N1+BhcJnDtkZc7jCXo3sDH2XNMU2iccOxy4Zl9Thum6TcMFwO9hNtR/dfQudQuoH2aY6tMcKJWjOx6xI33M8S4d6I1zm59pu0H+PGPE8xbHBIE2LsC+xPEqr6tmP7FkczYWpNKtICmwLxKGB7zKKmDDPZQBnDog7jUdQMRNhy9Gg5YWPYQHONM3gblyFqt6xyZthd4Yxi7UXcJetgIsynoAaXmJ1TPiWh84LL9YF4HkUIUQpKeVSmVuqjVopRKwnUiiphBjaNV7D4c4M5no+CXyqNkCdcPHITF5EHsmbrxddOa89dUwuWIg57IpmcTCkLju4Zo1pJEKD73ck33atPXziioHwL5Q232nY5wxpPCiEErIBt2+Ey5xhDzLrA4dI4DmWUKtgMCSWnORbFNngK0ZwGqkxDM7gwFnszIUFj0M9wSiLTNCAf1zuuS8XZKqWm//XZL87+cudPs0ogTI+yhNs0uN2lHCnJGjZNKEfhUXCImg/oUh2LzC5dNZ6/8DpXUKqBUt6gwJrtM0BnxWNoOjgRyPe6ceSfn2c6XAnZH1tEFP/zzJdf/XplZUJByiBOOShAhbuGRclxlK45ZgiPGBc4SlWlRsRQkHK5bWid2yWHmNL7f/tD//YgOi0LlUKI8NiT9sDF3OPvfrZMNj5SULYlm7Vu4q5UoaBklbhaC2WdbcKC/cw2NsVspBIzOungnslDIpOYBKQujiWVEgFYRfZvKiq/EPCz7tikWN8o/qV+//aOoEf8Pw+gYsYiOKc2q4xh/yYYFxKNeesu4ETX3sfnz+//8/0XbpGNmW0b3MK0ePA/tGXURf9n28U1BnXd3V+LYRnUtx/U1wQKJUS1ZPjl4XMcTSeNpU2spbvGwSUTuvT30Yv33X7HdQVljqPJDojEHQnMZNvp2XpUPLxWnHj8aLSXHiweisUMW/Ejto3hzgXwOZqPtNTjhll+NtwPFAS/xT4uy2FloRygkPmwEHG0dMwOnPLiw5/uvGxcfrAur4CkahKyOoASi9lQSaHe0iCZQQXuoe+DceWG7h/iMhHpwB4oFyyOYFpWVUuyKsbHxkAmhno8e2I450rfj2im0hh1rBLNxIzo4pUmFnxFBGo49M7K5vGFzWfk1TWtS6Pgn/ieGP3NcwLTiryhH9jlfQaj4ppFuS8mh7558sdXf7j4QcxrLlTEhKCHR3KAl5wlvLlxZUtjKlPDxgTaz117b/2hy59ckY9BXrQ4XFp2/NnTl3R8NYd6yIsUgs/BhAQgNdH1CdKbYlD/Bdooj2EHCwAA"
}
}

