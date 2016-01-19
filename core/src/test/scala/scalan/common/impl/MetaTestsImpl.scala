package scalan.common

import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait MetaTestsAbs extends scalan.ScalanDsl with MetaTests {
  self: MetaTestsDsl =>

  // single proxy for each type family
  implicit def proxyMetaTest[T](p: Rep[MetaTest[T]]): MetaTest[T] = {
    proxyOps[MetaTest[T]](p)(scala.reflect.classTag[MetaTest[T]])
  }

  // familyElem
  class MetaTestElem[T, To <: MetaTest[T]](implicit _elem: Elem[T])
    extends EntityElem[To] {
    def elem = _elem
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(elem))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagT = elem.tag
      weakTypeTag[MetaTest[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[MetaTest[T]] => convertMetaTest(x) }
      tryConvert(element[MetaTest[T]], this, x, conv)
    }

    def convertMetaTest(x: Rep[MetaTest[T]]): Rep[To] = {
      x.selfType1 match {
        case _: MetaTestElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have MetaTestElem[_, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def metaTestElement[T](implicit elem: Elem[T]): Elem[MetaTest[T]] =
    cachedElem[MetaTestElem[T, MetaTest[T]]](elem)

  implicit case object MetaTestCompanionElem extends CompanionElem[MetaTestCompanionAbs] {
    lazy val tag = weakTypeTag[MetaTestCompanionAbs]
    protected def getDefaultRep = MetaTest
  }

  abstract class MetaTestCompanionAbs extends CompanionDef[MetaTestCompanionAbs] with MetaTestCompanion {
    def selfType = MetaTestCompanionElem
    override def toString = "MetaTest"
  }
  def MetaTest: Rep[MetaTestCompanionAbs]
  implicit def proxyMetaTestCompanionAbs(p: Rep[MetaTestCompanionAbs]): MetaTestCompanionAbs =
    proxyOps[MetaTestCompanionAbs](p)

  abstract class AbsMT0
      (size: Rep[Int])
    extends MT0(size) with Def[MT0] {
    lazy val selfType = element[MT0]
  }
  // elem for concrete class
  class MT0Elem(val iso: Iso[MT0Data, MT0])
    extends MetaTestElem[Unit, MT0]
    with ConcreteElem[MT0Data, MT0] {
    override lazy val parent: Option[Elem[_]] = Some(metaTestElement(UnitElement))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }

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
    def isEntityType = true
    def getDefaultRep = reifyObject(new MT0Iso())
    lazy val tag = {
      weakTypeTag[MT0Iso]
    }
  }
  // 4) constructor and deconstructor
  class MT0CompanionAbs extends CompanionDef[MT0CompanionAbs] with MT0Companion {
    def selfType = MT0CompanionElem
    override def toString = "MT0"

    def apply(size: Rep[Int]): Rep[MT0] =
      mkMT0(size)
  }
  object MT0Matcher {
    def unapply(p: Rep[MetaTest[Unit]]) = unmkMT0(p)
  }
  lazy val MT0: Rep[MT0CompanionAbs] = new MT0CompanionAbs
  implicit def proxyMT0Companion(p: Rep[MT0CompanionAbs]): MT0CompanionAbs = {
    proxyOps[MT0CompanionAbs](p)
  }

  implicit case object MT0CompanionElem extends CompanionElem[MT0CompanionAbs] {
    lazy val tag = weakTypeTag[MT0CompanionAbs]
    protected def getDefaultRep = MT0
  }

  implicit def proxyMT0(p: Rep[MT0]): MT0 =
    proxyOps[MT0](p)

  implicit class ExtendedMT0(p: Rep[MT0]) {
    def toData: Rep[MT0Data] = isoMT0.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoMT0: Iso[MT0Data, MT0] =
    reifyObject(new MT0Iso())

  // 6) smart constructor and deconstructor
  def mkMT0(size: Rep[Int]): Rep[MT0]
  def unmkMT0(p: Rep[MetaTest[Unit]]): Option[(Rep[Int])]

  abstract class AbsMT1[T]
      (data: Rep[T], size: Rep[Int])(implicit elem: Elem[T])
    extends MT1[T](data, size) with Def[MT1[T]] {
    lazy val selfType = element[MT1[T]]
  }
  // elem for concrete class
  class MT1Elem[T](val iso: Iso[MT1Data[T], MT1[T]])(implicit override val elem: Elem[T])
    extends MetaTestElem[T, MT1[T]]
    with ConcreteElem[MT1Data[T], MT1[T]] {
    override lazy val parent: Option[Elem[_]] = Some(metaTestElement(element[T]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(elem))
    }

    override def convertMetaTest(x: Rep[MetaTest[T]]) = // Converter is not generated by meta
!!!("Cannot convert from MetaTest to MT1: missing fields List(data)")
    override def getDefaultRep = MT1(element[T].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagT = elem.tag
      weakTypeTag[MT1[T]]
    }
  }

  // state representation type
  type MT1Data[T] = (T, Int)

  // 3) Iso for concrete class
  class MT1Iso[T](implicit elem: Elem[T])
    extends EntityIso[MT1Data[T], MT1[T]] with Def[MT1Iso[T]] {
    override def from(p: Rep[MT1[T]]) =
      (p.data, p.size)
    override def to(p: Rep[(T, Int)]) = {
      val Pair(data, size) = p
      MT1(data, size)
    }
    lazy val eFrom = pairElement(element[T], element[Int])
    lazy val eTo = new MT1Elem[T](self)
    lazy val selfType = new MT1IsoElem[T](elem)
    def productArity = 1
    def productElement(n: Int) = elem
  }
  case class MT1IsoElem[T](elem: Elem[T]) extends Elem[MT1Iso[T]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new MT1Iso[T]()(elem))
    lazy val tag = {
      implicit val tagT = elem.tag
      weakTypeTag[MT1Iso[T]]
    }
  }
  // 4) constructor and deconstructor
  class MT1CompanionAbs extends CompanionDef[MT1CompanionAbs] {
    def selfType = MT1CompanionElem
    override def toString = "MT1"
    def apply[T](p: Rep[MT1Data[T]])(implicit elem: Elem[T]): Rep[MT1[T]] =
      isoMT1(elem).to(p)
    def apply[T](data: Rep[T], size: Rep[Int])(implicit elem: Elem[T]): Rep[MT1[T]] =
      mkMT1(data, size)
  }
  object MT1Matcher {
    def unapply[T](p: Rep[MetaTest[T]]) = unmkMT1(p)
  }
  lazy val MT1: Rep[MT1CompanionAbs] = new MT1CompanionAbs
  implicit def proxyMT1Companion(p: Rep[MT1CompanionAbs]): MT1CompanionAbs = {
    proxyOps[MT1CompanionAbs](p)
  }

  implicit case object MT1CompanionElem extends CompanionElem[MT1CompanionAbs] {
    lazy val tag = weakTypeTag[MT1CompanionAbs]
    protected def getDefaultRep = MT1
  }

  implicit def proxyMT1[T](p: Rep[MT1[T]]): MT1[T] =
    proxyOps[MT1[T]](p)

  implicit class ExtendedMT1[T](p: Rep[MT1[T]])(implicit elem: Elem[T]) {
    def toData: Rep[MT1Data[T]] = isoMT1(elem).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoMT1[T](implicit elem: Elem[T]): Iso[MT1Data[T], MT1[T]] =
    reifyObject(new MT1Iso[T]()(elem))

  // 6) smart constructor and deconstructor
  def mkMT1[T](data: Rep[T], size: Rep[Int])(implicit elem: Elem[T]): Rep[MT1[T]]
  def unmkMT1[T](p: Rep[MetaTest[T]]): Option[(Rep[T], Rep[Int])]

  abstract class AbsMT2[T, R]
      (indices: Rep[T], values: Rep[R], size: Rep[Int])(implicit eT: Elem[T], eR: Elem[R])
    extends MT2[T, R](indices, values, size) with Def[MT2[T, R]] {
    lazy val selfType = element[MT2[T, R]]
  }
  // elem for concrete class
  class MT2Elem[T, R](val iso: Iso[MT2Data[T, R], MT2[T, R]])(implicit val eT: Elem[T], val eR: Elem[R])
    extends MetaTestElem[(T, R), MT2[T, R]]
    with ConcreteElem[MT2Data[T, R], MT2[T, R]] {
    override lazy val parent: Option[Elem[_]] = Some(metaTestElement(pairElement(element[T],element[R])))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT), "R" -> Left(eR))
    }

    override def convertMetaTest(x: Rep[MetaTest[(T, R)]]) = // Converter is not generated by meta
!!!("Cannot convert from MetaTest to MT2: missing fields List(indices, values)")
    override def getDefaultRep = MT2(element[T].defaultRepValue, element[R].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagT = eT.tag
      implicit val tagR = eR.tag
      weakTypeTag[MT2[T, R]]
    }
  }

  // state representation type
  type MT2Data[T, R] = (T, (R, Int))

  // 3) Iso for concrete class
  class MT2Iso[T, R](implicit eT: Elem[T], eR: Elem[R])
    extends EntityIso[MT2Data[T, R], MT2[T, R]] with Def[MT2Iso[T, R]] {
    override def from(p: Rep[MT2[T, R]]) =
      (p.indices, p.values, p.size)
    override def to(p: Rep[(T, (R, Int))]) = {
      val Pair(indices, Pair(values, size)) = p
      MT2(indices, values, size)
    }
    lazy val eFrom = pairElement(element[T], pairElement(element[R], element[Int]))
    lazy val eTo = new MT2Elem[T, R](self)
    lazy val selfType = new MT2IsoElem[T, R](eT, eR)
    def productArity = 2
    def productElement(n: Int) = (eT, eR).productElement(n)
  }
  case class MT2IsoElem[T, R](eT: Elem[T], eR: Elem[R]) extends Elem[MT2Iso[T, R]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new MT2Iso[T, R]()(eT, eR))
    lazy val tag = {
      implicit val tagT = eT.tag
      implicit val tagR = eR.tag
      weakTypeTag[MT2Iso[T, R]]
    }
  }
  // 4) constructor and deconstructor
  class MT2CompanionAbs extends CompanionDef[MT2CompanionAbs] {
    def selfType = MT2CompanionElem
    override def toString = "MT2"
    def apply[T, R](p: Rep[MT2Data[T, R]])(implicit eT: Elem[T], eR: Elem[R]): Rep[MT2[T, R]] =
      isoMT2(eT, eR).to(p)
    def apply[T, R](indices: Rep[T], values: Rep[R], size: Rep[Int])(implicit eT: Elem[T], eR: Elem[R]): Rep[MT2[T, R]] =
      mkMT2(indices, values, size)
  }
  object MT2Matcher {
    def unapply[T, R](p: Rep[MetaTest[(T, R)]]) = unmkMT2(p)
  }
  lazy val MT2: Rep[MT2CompanionAbs] = new MT2CompanionAbs
  implicit def proxyMT2Companion(p: Rep[MT2CompanionAbs]): MT2CompanionAbs = {
    proxyOps[MT2CompanionAbs](p)
  }

  implicit case object MT2CompanionElem extends CompanionElem[MT2CompanionAbs] {
    lazy val tag = weakTypeTag[MT2CompanionAbs]
    protected def getDefaultRep = MT2
  }

  implicit def proxyMT2[T, R](p: Rep[MT2[T, R]]): MT2[T, R] =
    proxyOps[MT2[T, R]](p)

  implicit class ExtendedMT2[T, R](p: Rep[MT2[T, R]])(implicit eT: Elem[T], eR: Elem[R]) {
    def toData: Rep[MT2Data[T, R]] = isoMT2(eT, eR).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoMT2[T, R](implicit eT: Elem[T], eR: Elem[R]): Iso[MT2Data[T, R], MT2[T, R]] =
    reifyObject(new MT2Iso[T, R]()(eT, eR))

  // 6) smart constructor and deconstructor
  def mkMT2[T, R](indices: Rep[T], values: Rep[R], size: Rep[Int])(implicit eT: Elem[T], eR: Elem[R]): Rep[MT2[T, R]]
  def unmkMT2[T, R](p: Rep[MetaTest[(T, R)]]): Option[(Rep[T], Rep[R], Rep[Int])]

  registerModule(MetaTests_Module)
}

// Seq -----------------------------------
trait MetaTestsSeq extends scalan.ScalanDslStd with MetaTestsDsl {
  self: MetaTestsDslSeq =>
  lazy val MetaTest: Rep[MetaTestCompanionAbs] = new MetaTestCompanionAbs {
  }

  case class SeqMT0
      (override val size: Rep[Int])
    extends AbsMT0(size) {
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
      (override val data: Rep[T], override val size: Rep[Int])(implicit elem: Elem[T])
    extends AbsMT1[T](data, size) {
  }

  def mkMT1[T]
    (data: Rep[T], size: Rep[Int])(implicit elem: Elem[T]): Rep[MT1[T]] =
    new SeqMT1[T](data, size)
  def unmkMT1[T](p: Rep[MetaTest[T]]) = p match {
    case p: MT1[T] @unchecked =>
      Some((p.data, p.size))
    case _ => None
  }

  case class SeqMT2[T, R]
      (override val indices: Rep[T], override val values: Rep[R], override val size: Rep[Int])(implicit eT: Elem[T], eR: Elem[R])
    extends AbsMT2[T, R](indices, values, size) {
  }

  def mkMT2[T, R]
    (indices: Rep[T], values: Rep[R], size: Rep[Int])(implicit eT: Elem[T], eR: Elem[R]): Rep[MT2[T, R]] =
    new SeqMT2[T, R](indices, values, size)
  def unmkMT2[T, R](p: Rep[MetaTest[(T, R)]]) = p match {
    case p: MT2[T, R] @unchecked =>
      Some((p.indices, p.values, p.size))
    case _ => None
  }
}

// Exp -----------------------------------
trait MetaTestsExp extends scalan.ScalanDslExp with MetaTestsDsl {
  self: MetaTestsDslExp =>
  lazy val MetaTest: Rep[MetaTestCompanionAbs] = new MetaTestCompanionAbs {
  }

  case class ExpMT0
      (override val size: Rep[Int])
    extends AbsMT0(size)

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
      (override val data: Rep[T], override val size: Rep[Int])(implicit elem: Elem[T])
    extends AbsMT1[T](data, size)

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

  def mkMT1[T]
    (data: Rep[T], size: Rep[Int])(implicit elem: Elem[T]): Rep[MT1[T]] =
    new ExpMT1[T](data, size)
  def unmkMT1[T](p: Rep[MetaTest[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: MT1Elem[T] @unchecked =>
      Some((p.asRep[MT1[T]].data, p.asRep[MT1[T]].size))
    case _ =>
      None
  }

  case class ExpMT2[T, R]
      (override val indices: Rep[T], override val values: Rep[R], override val size: Rep[Int])(implicit eT: Elem[T], eR: Elem[R])
    extends AbsMT2[T, R](indices, values, size)

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

  def mkMT2[T, R]
    (indices: Rep[T], values: Rep[R], size: Rep[Int])(implicit eT: Elem[T], eR: Elem[R]): Rep[MT2[T, R]] =
    new ExpMT2[T, R](indices, values, size)
  def unmkMT2[T, R](p: Rep[MetaTest[(T, R)]]) = p.elem.asInstanceOf[Elem[_]] match {
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

object MetaTests_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWS4gcRRiu7pnZeZrNw2dAd11H1wSd2QQlhz2EdTPRyOzuMD0RGUOkpqdm0rG6ure7ZpjxEDwFMTfxKhjwIuQiniQQBBHEgycRwbOnGAk5GDwo/lX9mJ5Hz24I6UPTVf339//1/d9X1TfuoJTroJdcHVPMSibhuKTJ5w2XF7UK4wYfblntHiVnSOdp67svTnx19FsVLTbRwiXsnnFpE2W9h8rADp81sltFWcx04nLLcTl6viozlHWLUqJzw2JlwzR7HLcoKVcNl69XUbJltYe76ApSquigbjHdIZxomxS7LnH9+QwRFRnhOCvHwx17lIOVxSrKkVU0HGxwKB9yHPTi68TWhsxiQ5OjA35pO7YoC2LShmlbDg9SpAHuktUOhkmGYQIdrl7GfVyGFN2yxh2DdeHLvI31D3CXbEOICE9CwS6hncbQluNEFeVcsgsEnTNtKmcGNkIIOnBSFlEa8VMK+SkJfooacQxMjQ+xeFlzrMEQeZeSQGhgA8Qre0AECKTC2sVPLujv3dfypio+HohS0nKFCwC0FKMG2Qrg8cf6p+69N6+fUlGuiXKGu9FyuYN1Hm25z1YeM2ZxWXNIIHa60K2VuG7JLBsQMyGJrG6ZNmaA5FNZgD5RQze4CBZzBb87MdSnuU2CUGVgK+F6l2PWK3WziSmt3X7m1Rf/rLyrInU8RRYgNRC+E4BylNkClAaQEMK/EAdvk5pjmCDnPnn9+5vn797aTskMh9ukg3uUv4Npj3ji8vONcotU6svHOEqeZwYXU9nB6J6es6qQ39Xbf7V/WEMX1LAr/iL2JwSASLm//Zr/5dhpFWWa0jZnKe42oTFuhRJzx9m0GG+ijNUnjvcm3cdUPM0URtpftt+uKM8J4Jmj5ViD20Q0YV2aSQkIyHt+2LYYKZ6tFf/WfvrshpC7gwreG8/x/xmn/v39QIdLJwCfLjhElrTIUQI2Cp8NcT/CkbIGs+fYTMZzHqxmmeTQyj3j4vVrXHKrDMb3i53WZTDouvzuuTk0B/vW11evPnH3y/ePSL9lWgY3sV1cewC3BeZ4hG5CkoTRPvLUaCxuS8BsYauxthnNujQZDtRCzMSrvBLpwOK0wfxppREmG++LhI/EHg01IhNBx9uY45iOTwLHIMzWjLgVpwqS30xXpUxiErBQgJmsBIN565X0nRjhCws/G7/5QPeerFcfp3dO31JR6m2U6oAz3SpKtaweaweygIOYkwF/I5hTxmUBMsAONkMZyGsZjXo4i8C9WjrHEjZp9GxKXrv5z8WPP3rLlv6a2oVnMhUO6zOFsm+5pA3WNsB3D6WYhb7Y2+dh1PfEeBSqU0ljv5qLA6jPA5jmXor2ZFS04l6bI6Q5AdPwEczjaLyYBGytD7O1jCkmjPDGq37GiekZu+KhIOeMrTH6O/EgDE3WeG0U4wdmw5Vy9JjvNTgRTP/gWAULrsRYUPPPAWDvyv3Pt4///M0f8q8lJ04UOGtZ+Occ/VsZZ7QQpod/4UjJQtQAL8v9H2D08DSbDAAA"
}
}

trait MetaTestsDslSeq extends impl.MetaTestsSeq
trait MetaTestsDslExp extends impl.MetaTestsExp
