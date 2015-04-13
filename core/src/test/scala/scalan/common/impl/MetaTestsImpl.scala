
package scalan.common
package impl

import scalan.{Scalan, ScalanSeq, ScalanExp}
import scala.reflect.runtime.universe._
import scala.reflect._
import scalan.common.Default

// Abs -----------------------------------
trait MetaTestsAbs extends MetaTests with Scalan {
  self: MetaTestsDsl =>

  // single proxy for each type family
  implicit def proxyMetaTest[T](p: Rep[MetaTest[T]]): MetaTest[T] = {
    proxyOps[MetaTest[T]](p)(classTag[MetaTest[T]])
  }

  class MetaTestElem[T, To <: MetaTest[T]](implicit val elem: Elem[T])
    extends EntityElem[To] {
    override def isEntityType = true
    override def tag = {
      implicit val tagT = elem.tag
      weakTypeTag[MetaTest[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = convertMetaTest(x.asRep[MetaTest[T]])
    def convertMetaTest(x : Rep[MetaTest[T]]): Rep[To] = {
      assert(x.selfType1.isInstanceOf[MetaTestElem[_,_]])
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def metaTestElement[T](implicit elem: Elem[T]) =
    new MetaTestElem[T, MetaTest[T]]()(elem)

  trait MetaTestCompanionElem extends CompanionElem[MetaTestCompanionAbs]
  implicit lazy val MetaTestCompanionElem: MetaTestCompanionElem = new MetaTestCompanionElem {
    lazy val tag = weakTypeTag[MetaTestCompanionAbs]
    protected def getDefaultRep = MetaTest
  }

  abstract class MetaTestCompanionAbs extends CompanionBase[MetaTestCompanionAbs] with MetaTestCompanion {
    override def toString = "MetaTest"
  }
  def MetaTest: Rep[MetaTestCompanionAbs]
  implicit def proxyMetaTestCompanion(p: Rep[MetaTestCompanion]): MetaTestCompanion = {
    proxyOps[MetaTestCompanion](p)
  }

  // elem for concrete class
  class MT0Elem(val iso: Iso[MT0Data, MT0])
    extends MetaTestElem[Unit, MT0]
    with ConcreteElem[MT0Data, MT0] {
    override def convertMetaTest(x: Rep[MetaTest[Unit]]) = MT0(x.size)
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
    override lazy val tag = super[ConcreteElem].tag
  }

  // state representation type
  type MT0Data = Int

  // 3) Iso for concrete class
  class MT0Iso
    extends Iso[MT0Data, MT0] {
    override def from(p: Rep[MT0]) =
      p.size
    override def to(p: Rep[Int]) = {
      val size = p
      MT0(size)
    }
    lazy val tag = {
      weakTypeTag[MT0]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[MT0]](MT0(0))
    lazy val eTo = new MT0Elem(this)
  }
  // 4) constructor and deconstructor
  abstract class MT0CompanionAbs extends CompanionBase[MT0CompanionAbs] with MT0Companion {
    override def toString = "MT0"

    def apply(size: Rep[Int]): Rep[MT0] =
      mkMT0(size)
  }
  object MT0Matcher {
    def unapply(p: Rep[MetaTest[Unit]]) = unmkMT0(p)
  }
  def MT0: Rep[MT0CompanionAbs]
  implicit def proxyMT0Companion(p: Rep[MT0CompanionAbs]): MT0CompanionAbs = {
    proxyOps[MT0CompanionAbs](p)
  }

  class MT0CompanionElem extends CompanionElem[MT0CompanionAbs] {
    lazy val tag = weakTypeTag[MT0CompanionAbs]
    protected def getDefaultRep = MT0
  }
  implicit lazy val MT0CompanionElem: MT0CompanionElem = new MT0CompanionElem

  implicit def proxyMT0(p: Rep[MT0]): MT0 =
    proxyOps[MT0](p)

  implicit class ExtendedMT0(p: Rep[MT0]) {
    def toData: Rep[MT0Data] = isoMT0.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoMT0: Iso[MT0Data, MT0] =
    new MT0Iso

  // 6) smart constructor and deconstructor
  def mkMT0(size: Rep[Int]): Rep[MT0]
  def unmkMT0(p: Rep[MetaTest[Unit]]): Option[(Rep[Int])]

  // elem for concrete class
  class MT1Elem[T](val iso: Iso[MT1Data[T], MT1[T]])(implicit elem: Elem[T])
    extends MetaTestElem[T, MT1[T]]
    with ConcreteElem[MT1Data[T], MT1[T]] {
    override def convertMetaTest(x: Rep[MetaTest[T]]) = // Converter is not generated by meta
!!!("Cannot convert from MetaTest to MT1: missing fields List(data)")
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
    override lazy val tag = super[ConcreteElem].tag
  }

  // state representation type
  type MT1Data[T] = (T, Int)

  // 3) Iso for concrete class
  class MT1Iso[T](implicit elem: Elem[T])
    extends Iso[MT1Data[T], MT1[T]] {
    override def from(p: Rep[MT1[T]]) =
      (p.data, p.size)
    override def to(p: Rep[(T, Int)]) = {
      val Pair(data, size) = p
      MT1(data, size)
    }
    lazy val tag = {
      implicit val tagT = elem.tag
      weakTypeTag[MT1[T]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[MT1[T]]](MT1(element[T].defaultRepValue, 0))
    lazy val eTo = new MT1Elem[T](this)
  }
  // 4) constructor and deconstructor
  abstract class MT1CompanionAbs extends CompanionBase[MT1CompanionAbs] with MT1Companion {
    override def toString = "MT1"

    def apply[T](p: Rep[MT1Data[T]])(implicit elem: Elem[T]): Rep[MT1[T]] =
      isoMT1(elem).to(p)
    def apply[T](data: Rep[T], size: Rep[Int])(implicit elem: Elem[T]): Rep[MT1[T]] =
      mkMT1(data, size)
  }
  object MT1Matcher {
    def unapply[T:Elem](p: Rep[MetaTest[T]]) = unmkMT1(p)
  }
  def MT1: Rep[MT1CompanionAbs]
  implicit def proxyMT1Companion(p: Rep[MT1CompanionAbs]): MT1CompanionAbs = {
    proxyOps[MT1CompanionAbs](p)
  }

  class MT1CompanionElem extends CompanionElem[MT1CompanionAbs] {
    lazy val tag = weakTypeTag[MT1CompanionAbs]
    protected def getDefaultRep = MT1
  }
  implicit lazy val MT1CompanionElem: MT1CompanionElem = new MT1CompanionElem

  implicit def proxyMT1[T](p: Rep[MT1[T]]): MT1[T] =
    proxyOps[MT1[T]](p)

  implicit class ExtendedMT1[T](p: Rep[MT1[T]])(implicit elem: Elem[T]) {
    def toData: Rep[MT1Data[T]] = isoMT1(elem).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoMT1[T](implicit elem: Elem[T]): Iso[MT1Data[T], MT1[T]] =
    new MT1Iso[T]

  // 6) smart constructor and deconstructor
  def mkMT1[T](data: Rep[T], size: Rep[Int])(implicit elem: Elem[T]): Rep[MT1[T]]
  def unmkMT1[T:Elem](p: Rep[MetaTest[T]]): Option[(Rep[T], Rep[Int])]

  // elem for concrete class
  class MT2Elem[T, R](val iso: Iso[MT2Data[T, R], MT2[T, R]])(implicit eT: Elem[T], eR: Elem[R])
    extends MetaTestElem[(T, R), MT2[T, R]]
    with ConcreteElem[MT2Data[T, R], MT2[T, R]] {
    override def convertMetaTest(x: Rep[MetaTest[(T, R)]]) = // Converter is not generated by meta
!!!("Cannot convert from MetaTest to MT2: missing fields List(indices, values)")
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
    override lazy val tag = super[ConcreteElem].tag
  }

  // state representation type
  type MT2Data[T, R] = (T, (R, Int))

  // 3) Iso for concrete class
  class MT2Iso[T, R](implicit eT: Elem[T], eR: Elem[R])
    extends Iso[MT2Data[T, R], MT2[T, R]]()(pairElement(implicitly[Elem[T]], pairElement(implicitly[Elem[R]], implicitly[Elem[Int]]))) {
    override def from(p: Rep[MT2[T, R]]) =
      (p.indices, p.values, p.size)
    override def to(p: Rep[(T, (R, Int))]) = {
      val Pair(indices, Pair(values, size)) = p
      MT2(indices, values, size)
    }
    lazy val tag = {
      implicit val tagT = eT.tag
      implicit val tagR = eR.tag
      weakTypeTag[MT2[T, R]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[MT2[T, R]]](MT2(element[T].defaultRepValue, element[R].defaultRepValue, 0))
    lazy val eTo = new MT2Elem[T, R](this)
  }
  // 4) constructor and deconstructor
  abstract class MT2CompanionAbs extends CompanionBase[MT2CompanionAbs] with MT2Companion {
    override def toString = "MT2"

    def apply[T, R](p: Rep[MT2Data[T, R]])(implicit eT: Elem[T], eR: Elem[R]): Rep[MT2[T, R]] =
      isoMT2(eT, eR).to(p)
    def apply[T, R](indices: Rep[T], values: Rep[R], size: Rep[Int])(implicit eT: Elem[T], eR: Elem[R]): Rep[MT2[T, R]] =
      mkMT2(indices, values, size)
  }
  object MT2Matcher {
    def unapply[T:Elem, R:Elem](p: Rep[MetaTest[(T, R)]]) = unmkMT2(p)
  }
  def MT2: Rep[MT2CompanionAbs]
  implicit def proxyMT2Companion(p: Rep[MT2CompanionAbs]): MT2CompanionAbs = {
    proxyOps[MT2CompanionAbs](p)
  }

  class MT2CompanionElem extends CompanionElem[MT2CompanionAbs] {
    lazy val tag = weakTypeTag[MT2CompanionAbs]
    protected def getDefaultRep = MT2
  }
  implicit lazy val MT2CompanionElem: MT2CompanionElem = new MT2CompanionElem

  implicit def proxyMT2[T, R](p: Rep[MT2[T, R]]): MT2[T, R] =
    proxyOps[MT2[T, R]](p)

  implicit class ExtendedMT2[T, R](p: Rep[MT2[T, R]])(implicit eT: Elem[T], eR: Elem[R]) {
    def toData: Rep[MT2Data[T, R]] = isoMT2(eT, eR).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoMT2[T, R](implicit eT: Elem[T], eR: Elem[R]): Iso[MT2Data[T, R], MT2[T, R]] =
    new MT2Iso[T, R]

  // 6) smart constructor and deconstructor
  def mkMT2[T, R](indices: Rep[T], values: Rep[R], size: Rep[Int])(implicit eT: Elem[T], eR: Elem[R]): Rep[MT2[T, R]]
  def unmkMT2[T:Elem, R:Elem](p: Rep[MetaTest[(T, R)]]): Option[(Rep[T], Rep[R], Rep[Int])]
}

// Seq -----------------------------------
trait MetaTestsSeq extends MetaTestsDsl with ScalanSeq {
  self: MetaTestsDslSeq =>
  lazy val MetaTest: Rep[MetaTestCompanionAbs] = new MetaTestCompanionAbs with UserTypeSeq[MetaTestCompanionAbs] {
    lazy val selfType = element[MetaTestCompanionAbs]
  }

  case class SeqMT0
      (override val size: Rep[Int])

    extends MT0(size)
        with UserTypeSeq[MT0] {
    lazy val selfType = element[MT0]
  }
  lazy val MT0 = new MT0CompanionAbs with UserTypeSeq[MT0CompanionAbs] {
    lazy val selfType = element[MT0CompanionAbs]
  }

  def mkMT0
      (size: Rep[Int]): Rep[MT0] =
      new SeqMT0(size)
  def unmkMT0(p: Rep[MetaTest[Unit]]) = p match {
    case p: MT0 @unchecked =>
      Some((p.size))
    case _ => None
  }

  case class SeqMT1[T]
      (override val data: Rep[T], override val size: Rep[Int])
      (implicit elem: Elem[T])
    extends MT1[T](data, size)
        with UserTypeSeq[MT1[T]] {
    lazy val selfType = element[MT1[T]]
  }
  lazy val MT1 = new MT1CompanionAbs with UserTypeSeq[MT1CompanionAbs] {
    lazy val selfType = element[MT1CompanionAbs]
  }

  def mkMT1[T]
      (data: Rep[T], size: Rep[Int])(implicit elem: Elem[T]): Rep[MT1[T]] =
      new SeqMT1[T](data, size)
  def unmkMT1[T:Elem](p: Rep[MetaTest[T]]) = p match {
    case p: MT1[T] @unchecked =>
      Some((p.data, p.size))
    case _ => None
  }

  case class SeqMT2[T, R]
      (override val indices: Rep[T], override val values: Rep[R], override val size: Rep[Int])
      (implicit eT: Elem[T], eR: Elem[R])
    extends MT2[T, R](indices, values, size)
        with UserTypeSeq[MT2[T, R]] {
    lazy val selfType = element[MT2[T, R]]
  }
  lazy val MT2 = new MT2CompanionAbs with UserTypeSeq[MT2CompanionAbs] {
    lazy val selfType = element[MT2CompanionAbs]
  }

  def mkMT2[T, R]
      (indices: Rep[T], values: Rep[R], size: Rep[Int])(implicit eT: Elem[T], eR: Elem[R]): Rep[MT2[T, R]] =
      new SeqMT2[T, R](indices, values, size)
  def unmkMT2[T:Elem, R:Elem](p: Rep[MetaTest[(T, R)]]) = p match {
    case p: MT2[T, R] @unchecked =>
      Some((p.indices, p.values, p.size))
    case _ => None
  }
}

// Exp -----------------------------------
trait MetaTestsExp extends MetaTestsDsl with ScalanExp {
  self: MetaTestsDslExp =>
  lazy val MetaTest: Rep[MetaTestCompanionAbs] = new MetaTestCompanionAbs with UserTypeDef[MetaTestCompanionAbs] {
    lazy val selfType = element[MetaTestCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpMT0
      (override val size: Rep[Int])

    extends MT0(size) with UserTypeDef[MT0] {
    lazy val selfType = element[MT0]
    override def mirror(t: Transformer) = ExpMT0(t(size))
  }

  lazy val MT0: Rep[MT0CompanionAbs] = new MT0CompanionAbs with UserTypeDef[MT0CompanionAbs] {
    lazy val selfType = element[MT0CompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object MT0Methods {
    object test {
      def unapply(d: Def[_]): Option[Rep[MT0]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MT0Elem] && method.getName == "test" =>
          Some(receiver).asInstanceOf[Option[Rep[MT0]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MT0]] = exp match {
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
      def unapply(exp: Exp[_]): Option[Rep[MT0]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elem {
      def unapply(d: Def[_]): Option[Rep[MT0]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MT0Elem] && method.getName == "elem" =>
          Some(receiver).asInstanceOf[Option[Rep[MT0]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MT0]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object MT0CompanionMethods {
  }

  def mkMT0
    (size: Rep[Int]): Rep[MT0] =
    new ExpMT0(size)
  def unmkMT0(p: Rep[MetaTest[Unit]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: MT0Elem @unchecked =>
      Some((p.asRep[MT0].size))
    case _ =>
      None
  }

  case class ExpMT1[T]
      (override val data: Rep[T], override val size: Rep[Int])
      (implicit elem: Elem[T])
    extends MT1[T](data, size) with UserTypeDef[MT1[T]] {
    lazy val selfType = element[MT1[T]]
    override def mirror(t: Transformer) = ExpMT1[T](t(data), t(size))
  }

  lazy val MT1: Rep[MT1CompanionAbs] = new MT1CompanionAbs with UserTypeDef[MT1CompanionAbs] {
    lazy val selfType = element[MT1CompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object MT1Methods {
    object test {
      def unapply(d: Def[_]): Option[Rep[MT1[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MT1Elem[_]] && method.getName == "test" =>
          Some(receiver).asInstanceOf[Option[Rep[MT1[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MT1[T]] forSome {type T}] = exp match {
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
      def unapply(exp: Exp[_]): Option[Rep[MT1[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object MT1CompanionMethods {
  }

  def mkMT1[T]
    (data: Rep[T], size: Rep[Int])(implicit elem: Elem[T]): Rep[MT1[T]] =
    new ExpMT1[T](data, size)
  def unmkMT1[T:Elem](p: Rep[MetaTest[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: MT1Elem[T] @unchecked =>
      Some((p.asRep[MT1[T]].data, p.asRep[MT1[T]].size))
    case _ =>
      None
  }

  case class ExpMT2[T, R]
      (override val indices: Rep[T], override val values: Rep[R], override val size: Rep[Int])
      (implicit eT: Elem[T], eR: Elem[R])
    extends MT2[T, R](indices, values, size) with UserTypeDef[MT2[T, R]] {
    lazy val selfType = element[MT2[T, R]]
    override def mirror(t: Transformer) = ExpMT2[T, R](t(indices), t(values), t(size))
  }

  lazy val MT2: Rep[MT2CompanionAbs] = new MT2CompanionAbs with UserTypeDef[MT2CompanionAbs] {
    lazy val selfType = element[MT2CompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object MT2Methods {
    object test {
      def unapply(d: Def[_]): Option[Rep[MT2[T, R]] forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MT2Elem[_, _]] && method.getName == "test" =>
          Some(receiver).asInstanceOf[Option[Rep[MT2[T, R]] forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MT2[T, R]] forSome {type T; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object give {
      def unapply(d: Def[_]): Option[Rep[MT2[T, R]] forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MT2Elem[_, _]] && method.getName == "give" =>
          Some(receiver).asInstanceOf[Option[Rep[MT2[T, R]] forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MT2[T, R]] forSome {type T; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object MT2CompanionMethods {
  }

  def mkMT2[T, R]
    (indices: Rep[T], values: Rep[R], size: Rep[Int])(implicit eT: Elem[T], eR: Elem[R]): Rep[MT2[T, R]] =
    new ExpMT2[T, R](indices, values, size)
  def unmkMT2[T:Elem, R:Elem](p: Rep[MetaTest[(T, R)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: MT2Elem[T, R] @unchecked =>
      Some((p.asRep[MT2[T, R]].indices, p.asRep[MT2[T, R]].values, p.asRep[MT2[T, R]].size))
    case _ =>
      None
  }

  object MetaTestMethods {
    object test {
      def unapply(d: Def[_]): Option[Rep[MetaTest[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MetaTestElem[_, _]] && method.getName == "test" =>
          Some(receiver).asInstanceOf[Option[Rep[MetaTest[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MetaTest[T]] forSome {type T}] = exp match {
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
      def unapply(exp: Exp[_]): Option[Rep[MetaTest[T]] forSome {type T}] = exp match {
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
      def unapply(exp: Exp[_]): Option[Rep[MetaTest[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object MetaTestCompanionMethods {
  }
}
