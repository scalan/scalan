package scalan.common

import scalan.Scalan
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait MetaTestsDefs extends scalan.Scalan with MetaTests {
  self: MetaTestsModule =>

  // entityProxy: single proxy for each type family
  implicit def proxyMetaTest[T](p: Rep[MetaTest[T]]): MetaTest[T] = {
    proxyOps[MetaTest[T]](p)(scala.reflect.classTag[MetaTest[T]])
  }

  // familyElem
  class MetaTestElem[T, To <: MetaTest[T]](implicit _eT: Elem[T])
    extends EntityElem[To] {
    def eT = _eT
    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[MetaTest[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[MetaTest[T]] => convertMetaTest(x) }
      tryConvert(element[MetaTest[T]], this, x, conv)
    }

    def convertMetaTest(x: Rep[MetaTest[T]]): Rep[To] = {
      x.elem match {
        case _: MetaTestElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have MetaTestElem[_, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def metaTestElement[T](implicit eT: Elem[T]): Elem[MetaTest[T]] =
    cachedElem[MetaTestElem[T, MetaTest[T]]](eT)

  implicit case object MetaTestCompanionElem extends CompanionElem[MetaTestCompanionCtor] {
    lazy val tag = weakTypeTag[MetaTestCompanionCtor]
    protected def getDefaultRep = MetaTest
  }

  abstract class MetaTestCompanionCtor extends CompanionDef[MetaTestCompanionCtor] with MetaTestCompanion {
    def selfType = MetaTestCompanionElem
    override def toString = "MetaTest"
  }
  implicit def proxyMetaTestCompanionCtor(p: Rep[MetaTestCompanionCtor]): MetaTestCompanionCtor =
    proxyOps[MetaTestCompanionCtor](p)

  lazy val MetaTest: Rep[MetaTestCompanionCtor] = new MetaTestCompanionCtor {
  }

  object MetaTestMethods {
    object test {
      def unapply(d: Def[_]): Option[Rep[MetaTest[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MetaTestElem[_, _]] && method.getName == "test" =>
          Some(receiver).asInstanceOf[Option[Rep[MetaTest[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[MetaTest[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object give {
      def unapply(d: Def[_]): Option[Rep[MetaTest[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MetaTestElem[_, _]] && method.getName == "give" =>
          Some(receiver).asInstanceOf[Option[Rep[MetaTest[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[MetaTest[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object size {
      def unapply(d: Def[_]): Option[Rep[MetaTest[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MetaTestElem[_, _]] && method.getName == "size" =>
          Some(receiver).asInstanceOf[Option[Rep[MetaTest[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[MetaTest[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object MetaTestCompanionMethods {
  }

  case class MT0Ctor
      (override val size: Rep[Int])
    extends MT0(size) with Def[MT0] {
    lazy val selfType = element[MT0]
  }
  // elem for concrete class
  class MT0Elem(val iso: Iso[MT0Data, MT0])
    extends MetaTestElem[Unit, MT0]
    with ConcreteElem[MT0Data, MT0] {
    override lazy val parent: Option[Elem[_]] = Some(metaTestElement(UnitElement))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertMetaTest(x: Rep[MetaTest[Unit]]) = MT0(x.size)
    override def getDefaultRep = MT0(0)
    override lazy val tag = {
      weakTypeTag[MT0]
    }
  }

  // state representation type
  type MT0Data = Int

  // 3) Iso for concrete class
  class MT0Iso
    extends EntityIso[MT0Data, MT0] with Def[MT0Iso] {
    override def from(p: Rep[MT0]) =
      p.size
    override def to(p: Rep[Int]) = {
      val size = p
      MT0(size)
    }
    lazy val eFrom = element[Int]
    lazy val eTo = new MT0Elem(self)
    lazy val selfType = new MT0IsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class MT0IsoElem() extends Elem[MT0Iso] {
    def getDefaultRep = reifyObject(new MT0Iso())
    lazy val tag = {
      weakTypeTag[MT0Iso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class MT0CompanionCtor extends CompanionDef[MT0CompanionCtor] with MT0Companion {
    def selfType = MT0CompanionElem
    override def toString = "MT0Companion"

    @scalan.OverloadId("fromFields")
    def apply(size: Rep[Int]): Rep[MT0] =
      mkMT0(size)

    def unapply(p: Rep[MetaTest[Unit]]) = unmkMT0(p)
  }
  lazy val MT0Rep: Rep[MT0CompanionCtor] = new MT0CompanionCtor
  lazy val MT0: MT0CompanionCtor = proxyMT0Companion(MT0Rep)
  implicit def proxyMT0Companion(p: Rep[MT0CompanionCtor]): MT0CompanionCtor = {
    proxyOps[MT0CompanionCtor](p)
  }

  implicit case object MT0CompanionElem extends CompanionElem[MT0CompanionCtor] {
    lazy val tag = weakTypeTag[MT0CompanionCtor]
    protected def getDefaultRep = MT0Rep
  }

  implicit def proxyMT0(p: Rep[MT0]): MT0 =
    proxyOps[MT0](p)

  implicit class ExtendedMT0(p: Rep[MT0]) {
    def toData: Rep[MT0Data] = {
      isoMT0.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoMT0: Iso[MT0Data, MT0] =
    reifyObject(new MT0Iso())

  case class MT1Ctor[T]
      (override val data: Rep[T], override val size: Rep[Int])
    extends MT1[T](data, size) with Def[MT1[T]] {
    implicit val eT = data.elem
    lazy val selfType = element[MT1[T]]
  }
  // elem for concrete class
  class MT1Elem[T](val iso: Iso[MT1Data[T], MT1[T]])(implicit override val eT: Elem[T])
    extends MetaTestElem[T, MT1[T]]
    with ConcreteElem[MT1Data[T], MT1[T]] {
    override lazy val parent: Option[Elem[_]] = Some(metaTestElement(element[T]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
    override def convertMetaTest(x: Rep[MetaTest[T]]) = // Converter is not generated by meta
!!!("Cannot convert from MetaTest to MT1: missing fields List(data)")
    override def getDefaultRep = MT1(element[T].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[MT1[T]]
    }
  }

  // state representation type
  type MT1Data[T] = (T, Int)

  // 3) Iso for concrete class
  class MT1Iso[T](implicit eT: Elem[T])
    extends EntityIso[MT1Data[T], MT1[T]] with Def[MT1Iso[T]] {
    override def from(p: Rep[MT1[T]]) =
      (p.data, p.size)
    override def to(p: Rep[(T, Int)]) = {
      val Pair(data, size) = p
      MT1(data, size)
    }
    lazy val eFrom = pairElement(element[T], element[Int])
    lazy val eTo = new MT1Elem[T](self)
    lazy val selfType = new MT1IsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class MT1IsoElem[T](eT: Elem[T]) extends Elem[MT1Iso[T]] {
    def getDefaultRep = reifyObject(new MT1Iso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[MT1Iso[T]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class MT1CompanionCtor extends CompanionDef[MT1CompanionCtor] {
    def selfType = MT1CompanionElem
    override def toString = "MT1Companion"
    @scalan.OverloadId("fromData")
    def apply[T](p: Rep[MT1Data[T]]): Rep[MT1[T]] = {
      implicit val eT = p._1.elem
      isoMT1[T].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[T](data: Rep[T], size: Rep[Int]): Rep[MT1[T]] =
      mkMT1(data, size)

    def unapply[T](p: Rep[MetaTest[T]]) = unmkMT1(p)
  }
  lazy val MT1Rep: Rep[MT1CompanionCtor] = new MT1CompanionCtor
  lazy val MT1: MT1CompanionCtor = proxyMT1Companion(MT1Rep)
  implicit def proxyMT1Companion(p: Rep[MT1CompanionCtor]): MT1CompanionCtor = {
    proxyOps[MT1CompanionCtor](p)
  }

  implicit case object MT1CompanionElem extends CompanionElem[MT1CompanionCtor] {
    lazy val tag = weakTypeTag[MT1CompanionCtor]
    protected def getDefaultRep = MT1Rep
  }

  implicit def proxyMT1[T](p: Rep[MT1[T]]): MT1[T] =
    proxyOps[MT1[T]](p)

  implicit class ExtendedMT1[T](p: Rep[MT1[T]]) {
    def toData: Rep[MT1Data[T]] = {
      implicit val eT = p.data.elem
      isoMT1(eT).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoMT1[T](implicit eT: Elem[T]): Iso[MT1Data[T], MT1[T]] =
    reifyObject(new MT1Iso[T]()(eT))

  case class MT2Ctor[A, B]
      (override val indices: Rep[A], override val values: Rep[B], override val size: Rep[Int])
    extends MT2[A, B](indices, values, size) with Def[MT2[A, B]] {
    implicit val eA = indices.elem;
implicit val eB = values.elem
    lazy val selfType = element[MT2[A, B]]
  }
  // elem for concrete class
  class MT2Elem[A, B](val iso: Iso[MT2Data[A, B], MT2[A, B]])(implicit val eA: Elem[A], val eB: Elem[B])
    extends MetaTestElem[(A, B), MT2[A, B]]
    with ConcreteElem[MT2Data[A, B], MT2[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(metaTestElement(pairElement(element[A],element[B])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant))
    override def convertMetaTest(x: Rep[MetaTest[(A, B)]]) = // Converter is not generated by meta
!!!("Cannot convert from MetaTest to MT2: missing fields List(indices, values)")
    override def getDefaultRep = MT2(element[A].defaultRepValue, element[B].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[MT2[A, B]]
    }
  }

  // state representation type
  type MT2Data[A, B] = (A, (B, Int))

  // 3) Iso for concrete class
  class MT2Iso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends EntityIso[MT2Data[A, B], MT2[A, B]] with Def[MT2Iso[A, B]] {
    override def from(p: Rep[MT2[A, B]]) =
      (p.indices, p.values, p.size)
    override def to(p: Rep[(A, (B, Int))]) = {
      val Pair(indices, Pair(values, size)) = p
      MT2(indices, values, size)
    }
    lazy val eFrom = pairElement(element[A], pairElement(element[B], element[Int]))
    lazy val eTo = new MT2Elem[A, B](self)
    lazy val selfType = new MT2IsoElem[A, B](eA, eB)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eA
      case 1 => eB
    }
  }
  case class MT2IsoElem[A, B](eA: Elem[A], eB: Elem[B]) extends Elem[MT2Iso[A, B]] {
    def getDefaultRep = reifyObject(new MT2Iso[A, B]()(eA, eB))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[MT2Iso[A, B]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class MT2CompanionCtor extends CompanionDef[MT2CompanionCtor] {
    def selfType = MT2CompanionElem
    override def toString = "MT2Companion"
    @scalan.OverloadId("fromData")
    def apply[A, B](p: Rep[MT2Data[A, B]]): Rep[MT2[A, B]] = {
      implicit val eA = p._1.elem;
implicit val eB = p._2.elem
      isoMT2[A, B].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A, B](indices: Rep[A], values: Rep[B], size: Rep[Int]): Rep[MT2[A, B]] =
      mkMT2(indices, values, size)

    def unapply[A, B](p: Rep[MetaTest[(A, B)]]) = unmkMT2(p)
  }
  lazy val MT2Rep: Rep[MT2CompanionCtor] = new MT2CompanionCtor
  lazy val MT2: MT2CompanionCtor = proxyMT2Companion(MT2Rep)
  implicit def proxyMT2Companion(p: Rep[MT2CompanionCtor]): MT2CompanionCtor = {
    proxyOps[MT2CompanionCtor](p)
  }

  implicit case object MT2CompanionElem extends CompanionElem[MT2CompanionCtor] {
    lazy val tag = weakTypeTag[MT2CompanionCtor]
    protected def getDefaultRep = MT2Rep
  }

  implicit def proxyMT2[A, B](p: Rep[MT2[A, B]]): MT2[A, B] =
    proxyOps[MT2[A, B]](p)

  implicit class ExtendedMT2[A, B](p: Rep[MT2[A, B]]) {
    def toData: Rep[MT2Data[A, B]] = {
      implicit val eA = p.indices.elem;
implicit val eB = p.values.elem
      isoMT2(eA, eB).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoMT2[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[MT2Data[A, B], MT2[A, B]] =
    reifyObject(new MT2Iso[A, B]()(eA, eB))

  registerModule(MetaTestsModule)

  object MT0Methods {
    object test {
      def unapply(d: Def[_]): Option[Rep[MT0]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MT0Elem] && method.getName == "test" =>
          Some(receiver).asInstanceOf[Option[Rep[MT0]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[MT0]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object give {
      def unapply(d: Def[_]): Option[Rep[MT0]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MT0Elem] && method.getName == "give" =>
          Some(receiver).asInstanceOf[Option[Rep[MT0]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[MT0]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object eT {
      def unapply(d: Def[_]): Option[Rep[MT0]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MT0Elem] && method.getName == "eT" =>
          Some(receiver).asInstanceOf[Option[Rep[MT0]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[MT0]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object MT0CompanionMethods {
  }

  def mkMT0
    (size: Rep[Int]): Rep[MT0] = {
    new MT0Ctor(size)
  }
  def unmkMT0(p: Rep[MetaTest[Unit]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: MT0Elem @unchecked =>
      Some((p.asRep[MT0].size))
    case _ =>
      None
  }

  object MT1Methods {
    object test {
      def unapply(d: Def[_]): Option[Rep[MT1[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MT1Elem[_]] && method.getName == "test" =>
          Some(receiver).asInstanceOf[Option[Rep[MT1[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[MT1[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object give {
      def unapply(d: Def[_]): Option[Rep[MT1[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MT1Elem[_]] && method.getName == "give" =>
          Some(receiver).asInstanceOf[Option[Rep[MT1[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[MT1[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkMT1[T]
    (data: Rep[T], size: Rep[Int]): Rep[MT1[T]] = {
    new MT1Ctor[T](data, size)
  }
  def unmkMT1[T](p: Rep[MetaTest[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: MT1Elem[T] @unchecked =>
      Some((p.asRep[MT1[T]].data, p.asRep[MT1[T]].size))
    case _ =>
      None
  }

  object MT2Methods {
    object test {
      def unapply(d: Def[_]): Option[Rep[MT2[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MT2Elem[_, _]] && method.getName == "test" =>
          Some(receiver).asInstanceOf[Option[Rep[MT2[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[MT2[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object give {
      def unapply(d: Def[_]): Option[Rep[MT2[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MT2Elem[_, _]] && method.getName == "give" =>
          Some(receiver).asInstanceOf[Option[Rep[MT2[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[MT2[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkMT2[A, B]
    (indices: Rep[A], values: Rep[B], size: Rep[Int]): Rep[MT2[A, B]] = {
    new MT2Ctor[A, B](indices, values, size)
  }
  def unmkMT2[A, B](p: Rep[MetaTest[(A, B)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: MT2Elem[A, B] @unchecked =>
      Some((p.asRep[MT2[A, B]].indices, p.asRep[MT2[A, B]].values, p.asRep[MT2[A, B]].size))
    case _ =>
      None
  }
}

object MetaTestsModule extends scalan.ModuleInfo("scalan.common", "MetaTests")
}

