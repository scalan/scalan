package scalan.monads

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait ReadersAbs extends scalan.ScalanDsl with Readers {
  self: MonadsDsl =>

  // single proxy for each type family
  implicit def proxyReader[Env, A](p: Rep[Reader[Env, A]]): Reader[Env, A] = {
    proxyOps[Reader[Env, A]](p)(scala.reflect.classTag[Reader[Env, A]])
  }

  // familyElem
  class ReaderElem[Env, A, To <: Reader[Env, A]](implicit _eEnv: Elem[Env], _eA: Elem[A])
    extends EntityElem[To] {
    def eEnv = _eEnv
    def eA = _eA
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("Env" -> Left(eEnv), "A" -> Left(eA))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagEnv = eEnv.tag
      implicit val tagA = eA.tag
      weakTypeTag[Reader[Env, A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Reader[Env, A]] => convertReader(x) }
      tryConvert(element[Reader[Env, A]], this, x, conv)
    }

    def convertReader(x: Rep[Reader[Env, A]]): Rep[To] = {
      x.selfType1 match {
        case _: ReaderElem[_, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have ReaderElem[_, _, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def readerElement[Env, A](implicit eEnv: Elem[Env], eA: Elem[A]): Elem[Reader[Env, A]] =
    cachedElem[ReaderElem[Env, A, Reader[Env, A]]](eEnv, eA)

  implicit case object ReaderCompanionElem extends CompanionElem[ReaderCompanionAbs] {
    lazy val tag = weakTypeTag[ReaderCompanionAbs]
    protected def getDefaultRep = Reader
  }

  abstract class ReaderCompanionAbs extends CompanionDef[ReaderCompanionAbs] with ReaderCompanion {
    def selfType = ReaderCompanionElem
    override def toString = "Reader"
  }
  def Reader: Rep[ReaderCompanionAbs]
  implicit def proxyReaderCompanionAbs(p: Rep[ReaderCompanionAbs]): ReaderCompanionAbs =
    proxyOps[ReaderCompanionAbs](p)

  abstract class AbsReaderBase[Env, A]
      (run: Rep[Env => A])(implicit eEnv: Elem[Env], eA: Elem[A])
    extends ReaderBase[Env, A](run) with Def[ReaderBase[Env, A]] {
    lazy val selfType = element[ReaderBase[Env, A]]
  }
  // elem for concrete class
  class ReaderBaseElem[Env, A](val iso: Iso[ReaderBaseData[Env, A], ReaderBase[Env, A]])(implicit override val eEnv: Elem[Env], override val eA: Elem[A])
    extends ReaderElem[Env, A, ReaderBase[Env, A]]
    with ConcreteElem[ReaderBaseData[Env, A], ReaderBase[Env, A]] {
    override lazy val parent: Option[Elem[_]] = Some(readerElement(element[Env], element[A]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("Env" -> Left(eEnv), "A" -> Left(eA))
    }

    override def convertReader(x: Rep[Reader[Env, A]]) = ReaderBase(x.run)
    override def getDefaultRep = ReaderBase(constFun[Env, A](element[A].defaultRepValue))
    override lazy val tag = {
      implicit val tagEnv = eEnv.tag
      implicit val tagA = eA.tag
      weakTypeTag[ReaderBase[Env, A]]
    }
  }

  // state representation type
  type ReaderBaseData[Env, A] = Env => A

  // 3) Iso for concrete class
  class ReaderBaseIso[Env, A](implicit eEnv: Elem[Env], eA: Elem[A])
    extends EntityIso[ReaderBaseData[Env, A], ReaderBase[Env, A]] with Def[ReaderBaseIso[Env, A]] {
    override def from(p: Rep[ReaderBase[Env, A]]) =
      p.run
    override def to(p: Rep[Env => A]) = {
      val run = p
      ReaderBase(run)
    }
    lazy val eFrom = element[Env => A]
    lazy val eTo = new ReaderBaseElem[Env, A](self)
    lazy val selfType = new ReaderBaseIsoElem[Env, A](eEnv, eA)
    def productArity = 2
    def productElement(n: Int) = (eEnv, eA).productElement(n)
  }
  case class ReaderBaseIsoElem[Env, A](eEnv: Elem[Env], eA: Elem[A]) extends Elem[ReaderBaseIso[Env, A]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new ReaderBaseIso[Env, A]()(eEnv, eA))
    lazy val tag = {
      implicit val tagEnv = eEnv.tag
      implicit val tagA = eA.tag
      weakTypeTag[ReaderBaseIso[Env, A]]
    }
  }
  // 4) constructor and deconstructor
  class ReaderBaseCompanionAbs extends CompanionDef[ReaderBaseCompanionAbs] with ReaderBaseCompanion {
    def selfType = ReaderBaseCompanionElem
    override def toString = "ReaderBase"

    @scalan.OverloadId("fromFields")
    def apply[Env, A](run: Rep[Env => A])(implicit eEnv: Elem[Env], eA: Elem[A]): Rep[ReaderBase[Env, A]] =
      mkReaderBase(run)

    def unapply[Env, A](p: Rep[Reader[Env, A]]) = unmkReaderBase(p)
  }
  lazy val ReaderBaseRep: Rep[ReaderBaseCompanionAbs] = new ReaderBaseCompanionAbs
  lazy val ReaderBase: ReaderBaseCompanionAbs = proxyReaderBaseCompanion(ReaderBaseRep)
  implicit def proxyReaderBaseCompanion(p: Rep[ReaderBaseCompanionAbs]): ReaderBaseCompanionAbs = {
    proxyOps[ReaderBaseCompanionAbs](p)
  }

  implicit case object ReaderBaseCompanionElem extends CompanionElem[ReaderBaseCompanionAbs] {
    lazy val tag = weakTypeTag[ReaderBaseCompanionAbs]
    protected def getDefaultRep = ReaderBase
  }

  implicit def proxyReaderBase[Env, A](p: Rep[ReaderBase[Env, A]]): ReaderBase[Env, A] =
    proxyOps[ReaderBase[Env, A]](p)

  implicit class ExtendedReaderBase[Env, A](p: Rep[ReaderBase[Env, A]])(implicit eEnv: Elem[Env], eA: Elem[A]) {
    def toData: Rep[ReaderBaseData[Env, A]] = isoReaderBase(eEnv, eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoReaderBase[Env, A](implicit eEnv: Elem[Env], eA: Elem[A]): Iso[ReaderBaseData[Env, A], ReaderBase[Env, A]] =
    reifyObject(new ReaderBaseIso[Env, A]()(eEnv, eA))

  // 6) smart constructor and deconstructor
  def mkReaderBase[Env, A](run: Rep[Env => A])(implicit eEnv: Elem[Env], eA: Elem[A]): Rep[ReaderBase[Env, A]]
  def unmkReaderBase[Env, A](p: Rep[Reader[Env, A]]): Option[(Rep[Env => A])]

  registerModule(Readers_Module)
}

// Std -----------------------------------
trait ReadersStd extends scalan.ScalanDslStd with ReadersDsl {
  self: MonadsDslStd =>
  lazy val Reader: Rep[ReaderCompanionAbs] = new ReaderCompanionAbs {
  }

  case class StdReaderBase[Env, A]
      (override val run: Rep[Env => A])(implicit eEnv: Elem[Env], eA: Elem[A])
    extends AbsReaderBase[Env, A](run) {
  }

  def mkReaderBase[Env, A]
    (run: Rep[Env => A])(implicit eEnv: Elem[Env], eA: Elem[A]): Rep[ReaderBase[Env, A]] =
    new StdReaderBase[Env, A](run)
  def unmkReaderBase[Env, A](p: Rep[Reader[Env, A]]) = p match {
    case p: ReaderBase[Env, A] @unchecked =>
      Some((p.run))
    case _ => None
  }
}

// Exp -----------------------------------
trait ReadersExp extends scalan.ScalanDslExp with ReadersDsl {
  self: MonadsDslExp =>
  lazy val Reader: Rep[ReaderCompanionAbs] = new ReaderCompanionAbs {
  }

  case class ExpReaderBase[Env, A]
      (override val run: Rep[Env => A])(implicit eEnv: Elem[Env], eA: Elem[A])
    extends AbsReaderBase[Env, A](run)

  object ReaderBaseMethods {
  }

  object ReaderBaseCompanionMethods {
  }

  def mkReaderBase[Env, A]
    (run: Rep[Env => A])(implicit eEnv: Elem[Env], eA: Elem[A]): Rep[ReaderBase[Env, A]] =
    new ExpReaderBase[Env, A](run)
  def unmkReaderBase[Env, A](p: Rep[Reader[Env, A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ReaderBaseElem[Env, A] @unchecked =>
      Some((p.asRep[ReaderBase[Env, A]].run))
    case _ =>
      None
  }

  object ReaderMethods {
    object run {
      def unapply(d: Def[_]): Option[Rep[Reader[Env, A]] forSome {type Env; type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ReaderElem[_, _, _]] && method.getName == "run" =>
          Some(receiver).asInstanceOf[Option[Rep[Reader[Env, A]] forSome {type Env; type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Reader[Env, A]] forSome {type Env; type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ReaderCompanionMethods {
    object ask {
      def unapply(d: Def[_]): Option[Unit forSome {type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem == ReaderCompanionElem && method.getName == "ask" =>
          Some(()).asInstanceOf[Option[Unit forSome {type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object Readers_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWTYgcRRR+87fzt2aj6w+6JLtZxp8EMxP0kMMgYTKZlQ2zP2znIGMw1HTXTjp2V7ddNcOMh3jbg97EiwcPQUWEIIh3Twoi4kFCEPTiwVNMCHtIToqvqn93md4YxT4UVdWv33v1fd97XddvQ4F78BzXiUVY3aaC1DU1b3FR0zpMmGKy5hhDi56j29V7zWO/7rRXszDXg5nLhJ/jVg/K/qQzdqO5JowulAnTKReOxwUc66oIDd2xLKoL02EN07aHgvQt2uiaXDS7kO87xuQtuAqZLhzWHaZ7VFCtbRHOKQ/2S1RmZEbrslpPNtw4BmvIUzQSp7jgEVNg+hjjsG+/RV1twhw2sQUcClLbcGVaaFM0bdfxRBiiiO4uO0a4zDOCG/BY9woZkQaGGDQ04ZlsgF9WXaK/SQZ0HU2keR4T5tTavjBx1TqHJlwYCNCq7VrKY27sAgBS8JLKoh4DVI8AqkuAahr1TGKZbxP5ctNzxhPwn0wOYOyiixcf4CL0QDvMqL17UX/9vla1s/LjsUylqBKaQUeLKXJQXCCQ3229z3dfvXY6C5UeVEze6nPhEV0kOQ/gqhLGHKFyjhAk3gDpWk6jS0Vpoc0+TZR1x3YJQ08BlrNIlGXqppDGcm82oCcF+6JwaWiaGbuZ6LxLKedVwmkTy9q89fTJZ//ovJaF7N4QZXSpofK90KmAmS1KDOoFzuU4JyDXYaMYY9zItNRSDuVxPBYPyCbC5flbd4xvT8HFbIRmEPyfEYguCvznm9Ubx89kodRTel+xyKCHgPKORe0Nr+0w0YOSM6Ke/6Y4IpacTSW0aNBtMrREAHMSnxziI2AptTJdKsFrqiLIhABUfR2vO4zWVjZr97TvP7guZerBrP/GL9W/zNN//nJoWygFI8TekEVwY4FHYBxNY9elK0Om31j9cH7uyKXfFLczhmMTUwlsoQsFD8tbHWUhAPchqaz4+WqOTR9d3jXfuPaeUKRlxns7yEb/ClZsU3139AD+wk725c7OE3c/uTSvCrDUN4VN3Nqphyi/sFr+x/KCCBUfqafitRwWkbJ5v1TOEk7byeCLia8SdfRMJhSJMhKQx042CmnIS+mmVJ3PynQnWdo6wMUUcgVU4ryVk0hqR9Klhsg8udV93Lp95ussFM5DYRvLiaPG+s6QGSHk+NsTdCzOhnuZvZAjxMQjdgSxepYgxmy/PNvTTKacKXHok7APRNTb3p3/1OHKiX9NQhJqfSJI4MHKmfNTmKKauAMnqUnF4t/BJcfV2CYwLAa4CHgk1IHDiMGDs3mwnCIPLSgkRPrq/Y/WT/z41e+qG1VkSWIXZNFlJBZD1O0DuMtrKhbeLRLZoqRlkUYJHE9LQBjyRkJtDOU3i93zn5cXvuh/rNpVSV5aqG7xqJVPv8itEbepbiIvHHATQaNax3bxWomTl7955ad3fvjsU9XD/wZUl4IOjQoAAA=="
}
}

trait ReadersDslStd extends impl.ReadersStd {self: MonadsDslStd =>}
trait ReadersDslExp extends impl.ReadersExp {self: MonadsDslExp =>}
