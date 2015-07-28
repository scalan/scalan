package scalan.monads

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}

package impl {
// Abs -----------------------------------
trait ReadersAbs extends Readers with scalan.Scalan {
  self: MonadsDsl =>

  // single proxy for each type family
  implicit def proxyReader[Env, A](p: Rep[Reader[Env, A]]): Reader[Env, A] = {
    proxyOps[Reader[Env, A]](p)(scala.reflect.classTag[Reader[Env, A]])
  }

  // familyElem
  class ReaderElem[Env, A, To <: Reader[Env, A]](implicit val eEnv: Elem[Env], val eA: Elem[A])
    extends EntityElem[To] {
    val parent: Option[Elem[_]] = None
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagEnv = eEnv.tag
      implicit val tagA = eA.tag
      weakTypeTag[Reader[Env, A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Reader[Env, A]] => convertReader(x) }
      tryConvert(element[Reader[Env, A]], this, x, conv)
    }

    def convertReader(x : Rep[Reader[Env, A]]): Rep[To] = {
      assert(x.selfType1 match { case _: ReaderElem[_, _, _] => true; case _ => false })
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def readerElement[Env, A](implicit eEnv: Elem[Env], eA: Elem[A]): Elem[Reader[Env, A]] =
    new ReaderElem[Env, A, Reader[Env, A]]

  implicit case object ReaderCompanionElem extends CompanionElem[ReaderCompanionAbs] {
    lazy val tag = weakTypeTag[ReaderCompanionAbs]
    protected def getDefaultRep = Reader
  }

  abstract class ReaderCompanionAbs extends CompanionBase[ReaderCompanionAbs] with ReaderCompanion {
    override def toString = "Reader"
  }
  def Reader: Rep[ReaderCompanionAbs]
  implicit def proxyReaderCompanion(p: Rep[ReaderCompanion]): ReaderCompanion =
    proxyOps[ReaderCompanion](p)

  // elem for concrete class
  class ReaderBaseElem[Env, A](val iso: Iso[ReaderBaseData[Env, A], ReaderBase[Env, A]])(implicit eEnv: Elem[Env], eA: Elem[A])
    extends ReaderElem[Env, A, ReaderBase[Env, A]]
    with ConcreteElem[ReaderBaseData[Env, A], ReaderBase[Env, A]] {
    override val parent: Option[Elem[_]] = Some(readerElement(element[Env], element[A]))

    override def convertReader(x: Rep[Reader[Env, A]]) = ReaderBase(x.run)
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
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
    extends Iso[ReaderBaseData[Env, A], ReaderBase[Env, A]] {
    override def from(p: Rep[ReaderBase[Env, A]]) =
      p.run
    override def to(p: Rep[Env => A]) = {
      val run = p
      ReaderBase(run)
    }
    lazy val defaultRepTo: Rep[ReaderBase[Env, A]] = ReaderBase(fun { (x: Rep[Env]) => element[A].defaultRepValue })
    lazy val eTo = new ReaderBaseElem[Env, A](this)
  }
  // 4) constructor and deconstructor
  abstract class ReaderBaseCompanionAbs extends CompanionBase[ReaderBaseCompanionAbs] with ReaderBaseCompanion {
    override def toString = "ReaderBase"

    def apply[Env, A](run: Rep[Env => A])(implicit eEnv: Elem[Env], eA: Elem[A]): Rep[ReaderBase[Env, A]] =
      mkReaderBase(run)
  }
  object ReaderBaseMatcher {
    def unapply[Env, A](p: Rep[Reader[Env, A]]) = unmkReaderBase(p)
  }
  def ReaderBase: Rep[ReaderBaseCompanionAbs]
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
    new ReaderBaseIso[Env, A]

  // 6) smart constructor and deconstructor
  def mkReaderBase[Env, A](run: Rep[Env => A])(implicit eEnv: Elem[Env], eA: Elem[A]): Rep[ReaderBase[Env, A]]
  def unmkReaderBase[Env, A](p: Rep[Reader[Env, A]]): Option[(Rep[Env => A])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(Readers_Module.dump))
}

// Seq -----------------------------------
trait ReadersSeq extends ReadersDsl with scalan.ScalanSeq {
  self: MonadsDslSeq =>
  lazy val Reader: Rep[ReaderCompanionAbs] = new ReaderCompanionAbs with UserTypeSeq[ReaderCompanionAbs] {
    lazy val selfType = element[ReaderCompanionAbs]
  }

  case class SeqReaderBase[Env, A]
      (override val run: Rep[Env => A])
      (implicit eEnv: Elem[Env], eA: Elem[A])
    extends ReaderBase[Env, A](run)
        with UserTypeSeq[ReaderBase[Env, A]] {
    lazy val selfType = element[ReaderBase[Env, A]]
  }
  lazy val ReaderBase = new ReaderBaseCompanionAbs with UserTypeSeq[ReaderBaseCompanionAbs] {
    lazy val selfType = element[ReaderBaseCompanionAbs]
  }

  def mkReaderBase[Env, A]
      (run: Rep[Env => A])(implicit eEnv: Elem[Env], eA: Elem[A]): Rep[ReaderBase[Env, A]] =
      new SeqReaderBase[Env, A](run)
  def unmkReaderBase[Env, A](p: Rep[Reader[Env, A]]) = p match {
    case p: ReaderBase[Env, A] @unchecked =>
      Some((p.run))
    case _ => None
  }
}

// Exp -----------------------------------
trait ReadersExp extends ReadersDsl with scalan.ScalanExp {
  self: MonadsDslExp =>
  lazy val Reader: Rep[ReaderCompanionAbs] = new ReaderCompanionAbs with UserTypeDef[ReaderCompanionAbs] {
    lazy val selfType = element[ReaderCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpReaderBase[Env, A]
      (override val run: Rep[Env => A])
      (implicit eEnv: Elem[Env], eA: Elem[A])
    extends ReaderBase[Env, A](run) with UserTypeDef[ReaderBase[Env, A]] {
    lazy val selfType = element[ReaderBase[Env, A]]
    override def mirror(t: Transformer) = ExpReaderBase[Env, A](t(run))
  }

  lazy val ReaderBase: Rep[ReaderBaseCompanionAbs] = new ReaderBaseCompanionAbs with UserTypeDef[ReaderBaseCompanionAbs] {
    lazy val selfType = element[ReaderBaseCompanionAbs]
    override def mirror(t: Transformer) = this
  }

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

object Readers_Module {
  val packageName = "scalan.monads"
  val name = "Readers"
  val dump = "H4sIAAAAAAAAALVWz28bRRR+3jhx7ISm0AMqVZsSmVatqB1xKVIOyEkcWslNomwPyK1A492xs2F3ZjMztmwOvQPiUnFBCKEeuPXG34CEOHCqKBInDpwKSFSUnkC8mf1lF29ohfBhtDPz9v34vu+99d1fYFYKOCcd4hNWC6giNds8N6Sq2k2mPDW6xt2+Tzdpd/Pg/uvii/2PLFhqw9w+kZvSb0M5emgOw/TZpoctKBPmUKm4kApebpkIdYf7PnWUx1ndC4K+Ih2f1lueVGstKHa4OzqEW1BowXGHM0dQRe0Nn0hJZXw+T3VGXrovm/1oJ8xisLquoj5WxXVBPIXpY4zjkf0eDe0R42wUKDgWp7YT6rTQpuQFIRcqCVFCd/vcTbZFRvAAXmgdkAGpY4he3VbCYz18cyEkzrukR7fRRJsXMWFJ/e71UWj2My2oSHqIAF0NQt+cDEMAQAZeM0nUMnxqKT41jU/VpsIjvvce0Ze7gg9HEP0KMwDDEF28+i8uEg+0ydzqBzedG4/thcDSLw91KiVT4Rw6Ws5Rg6ECcfx677Z8+OadyxZU2lDxZKMjlSCOGqc8RmuBMMaVyTkFkIgesrWSx5aJ0kCbJyRRdngQEoaeYigXkSffczyljfXZYsxODvQlFdLEtDAMC2m9Z3PqNbrZIL6/++DkpVd+br5lgTUZoowubRS+SJwqmNujxKUidq7XJQUzTTbIMMaDQsNs9VIeZmvpiGxSXM4/+NX9ahVuWimacfCnIxBdzMrvv1u4d+ENC+bbRu5bPum1EVDZ9GmwIzY4U22Y5wMqopvSgPj6aSqhJZd2Sd9XMczj+MwgPgrO5jZmSDV4a6YJCgkAC5GOtzmj1a3d6h/2Nx/f1TIVsBjdRJ36l3f5zx+OdZVRMEIs+iyFG/s7BeNMHrsh3eoz597VT04snX7nR8PtnMsD4hmBnWrBrMDuNqWcisF9RiorUb42D+jzKw+9t+98qAxpheHkANnpHGDHrpn3zhzBXzLIHrVXrd9P3v/cgjLS1PFUQMLq6lO23//YUpAikS3LyM2JqCfWiaQb4xGXMyBfHGuYlwqJGoyRgiKOrEGCd1FrNKe9IvinO7Fo4wgXU1hUUMnyNk5STZ3O1xTCcfvGuSvit0/ftzRksx3eZ26CL37WFB2q9eSsMIkv4kkECRI8M4ye1N36+NWU3MeKuwSTlZb3qNf19Fdh8vw/Da5Y85npxTh2jiKWomBT1JCN0HHI/1Hzs8Gh1yuZTWxYiitW8FzCJ2fElXEBAlZyaLbjLsBWvPX4s+2L3375kxkfFd1POLZY+uchIzcdzwkN10ws/C8wli1KU3eYyfRvymkdMpsJAAA="
}
}

trait ReadersDslSeq extends impl.ReadersSeq {self: MonadsDslSeq =>}
trait ReadersDslExp extends impl.ReadersExp {self: MonadsDslExp =>}
