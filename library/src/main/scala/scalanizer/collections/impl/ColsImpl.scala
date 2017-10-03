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
    implicit val eA = arr.elem.typeArgs("T")._1.asElem[A]
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
  val dump = "H4sIAAAAAAAAALVWX4gbRRyebHLNXe76L3oVq9LzTE+sklRfKh4iaZorarwc3dJCLMpkM4nT7u6Ms5MzkdLHPti34pPgQ0EQ5FCkLyIiUhHEh76XvikFQSylDxYLFn8zu7PZXJPL+eA+DDOz8/v3fd9vdjf+RFOBQAuBg13sFz0icdHW83IgC/ZbrNV1yTHS/pVdzV++9PcXFtrTQDtpcIoK2cUu/ZC0Gmienat6VNYF7YQGJwWmsoZ2Vn1JZb/g6U2JDtXCMCUVpjQqTCGyWK6h/Mk+J3bfZz71Yg+lyR6SZuDm0dMCc07EplRenOxo2BBczWDfIYFkIpDo6dC+5DDXJY6kzC9Rz+tK3HRJqUYDCef3OMx3BJHErrg4CEjwPrqAMjU0TZRLGq9n9Lpf5wO/D+elIYW0lN/w/AnCdZ19T6JdUTp1rlKBM1nqcSakCZEFd++xlllmfAwbKF87i9dxCUJ0SrYU1O+A5W42TKMy2VFDsxw753CHrIKl2spCHQFx2wpufaTHUYpzDmJ6SedSHEBTjKEpKmgKNhFUaQerl2uC9foofFJphHrKxQsTXBgPpOq3Ch+dcd6+Z896ljLu6RpnwMeBMZrWZACSP5+4HNw9fuWIhXINlKNBuRlIgR2ZJDrCaxb7PpM63RhCLDrA1+I4vnSUMpwBSDNN1uobsh3mceyDpwjYOWDKpQ6V6rDa2x3xMxJloFJyYo5mAPS43nE9rGzLnLv9H89/f/63J2/stVBaibDHRcJtGtxuUY6WQgW7LpRjSRMcouZCpmzmkb2Ld+k7Vy5JC6VqKNUb1le9eRaYXO4JNBdahFJ9QI/8c3NXW1oR8WOLMPG/y/5w7fdbr2UsZA3jNAMF2FUoyiQnUbrC3AgeNT4uUaqsJjk1zPXUuH/TOrdFDjGlz/5xu/XTYXRGF6qFYPDYlvbARf7lT749SNa+tNB0Q/fqios7WoWKkmMkcBpomq0TEe5n17GrZiOVmG2RNu66plGTmISkLowllRMF2DKoCHrPlD8b8rPKfFJYWSv8Zf/y8YaiR71/DEDFQhg4d5wuC4H7EzA2yKpxfgtwzBX39cWL83c+e/cR3ZjTTSo9zAuH/0Nbmi76P9surjGs68BgrYYlUN88qK8OFGqIKsnwSwm7BHaLqVTEgj4kkUXKBtRM1SXeRJwlmksG1W5iTT41DnZd2Ff3j37+zP4nHlgo+waaaoPYgpEATzVZ128ZEOELJ0lPHjV7mWEQATQssBd/+NYxXNtAokT7jCa7krqlU9F+qER4Fgb4LsWzV4xC90WVKNvi637oVRae/2bjA3r9uRV9lyRhSeBWRIlFPpJkBGgatDcM8TYukHA8/tA1skkUiUjFbWhH0ThCMrqqN5O0qvHVLTBbjWflzTlXhk/mQGSmXNBehDF8YkXiMxzETbw4Rk121DaA5YV7n64eun71lr6qc6oB4Urx43+SgVDiizMCOafyCH9BErhCfqon/wVRwpMIsgoAAA=="
}
}

trait ColsModule extends scalanizer.collections.impl.ColsDefs with scala.wrappers.WrappersModule
