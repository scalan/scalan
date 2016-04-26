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
  val dump = "H4sIAAAAAAAAALVWTWwbRRR+duw4tkMTCP9VmxCZv0Dtih56iFDkOg5K5fwoW1RkKqrx7tjdsju77Iwtm0OROPQAnBBC4sChEohLL4g74gASqhASqEJInDhwKkWoB3oC8Wb215E3pSB8GO3MPr+f7/vem716E/Lcgye4TizCqjYVpKqp5zoXFa3JhClGW47Rt+g67X73zBdO6c13trMw34bpC4Svc6sNRf+hOXSjZ00YLSgSplMuHI8LeKylItR0x7KoLkyH1Uzb7gvSsWitZXKx2oJcxzFGr8ElyLRgXneY7lFBtYZFOKc8OJ+hMiMz2hfVfrTjxjFYTVZRS1RxxiOmwPQxxrxvv0ddbcQcNrIFHApS23FlWmhTpkMXa9i0XUuFmWpBwbRdxxNh1AJGuOAY4TbHCB7Afa2LZEBqGLVX04Rnsp505hL9VdKj22gizXNYA6dW98zIpYHzMhfGWLyhCwDIynMqsWqMWTXCrCoxq2jUM4llvk7ky13PGY7A/2WmAIYuunj2Di5CD7TJjMpb5/SXb2tlOyv/PJSpFFRC0+hoMUUhih7E9uu9d/mtF66czEKpDSWT1ztceEQXSRkEcJUJY45QOUcIEq+HDC6nMaii1NFmn0yKumO7hKGnAMtZJMoydVNIY3k2G9CTgn1BuDQ0zQzdTFTvUkq9SksNYlm7Nx459vivzZeykB0PUUSXGjaDFzoVML1HiUG9wLlc5wRMNdkgxhgPMnW1lUtxGK+FA7KJcHnyxm/GV8fhXDZCMwj+zwhEF3n+4w/l60+vZWGmrfS+YZFeGwHlTYvaO17DYaINM86Aev6bwoBY8mkioQWDdknfEgHMSXymEB8BS6nN6lIJ3qpqgkwIQNnX8bbDaGVjt/KHdu29q1KmHsz6b/zu/cs8+edPh7pCKRgh9vosght7PgLjaBq7Lt3oM/365gcLc0fO/6y4nTYcm5hKYIdbkPewvVUphwNw75LKkp+v5tj03uVb5itX3haKtMxwfILsdC5ix66q/x09gL9wuH16+fIDv390fkE14EzHFDZxK8fvov3Cbvkf2wsiVHykHor3cllEyhb8VjlFOG0kgy8m/pXoo0czoUiUkYAcTrJBSENOSjel63xWJjvJ0voBLiaQK6AU562cRFI7ki41RObBvdb91s21z7OQPw35LrYTR411nD4zQsjxJhR0KE6FZ5lxyBFi4hE7glj9liDGbL88G5NMJtSUKPoY7AMR9TZ+8p8mXDFx1yQkofYrQQJ3Vs6cn8IE1cQTOElNKhb/Di65bsb+V5D6agr161S3iEcNeeNTG79I/GY88f7a2dMPn31RjYNZQxn5b6JxOfn7aYu4q+q2f+qA2x6NKk3bxa85fDjx5fPfv/HNJx+rORlDKaAQ8CjgnjB5hxGDRzUtp9SkBY2Pyrh0+8PtlW8/+0VNz5IcITi1WfTxFIs3up0CeRS3VCz8Fkqgiy0oh0pCDEQu9G+eTSIBugoAAA=="
}
}

trait ReadersDslStd extends impl.ReadersStd {self: MonadsDslStd =>}
trait ReadersDslExp extends impl.ReadersExp {self: MonadsDslExp =>}
