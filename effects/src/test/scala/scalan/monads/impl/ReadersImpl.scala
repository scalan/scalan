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
    lazy val typeArgs = TypeArgs("Env" -> (eEnv -> scalan.util.Invariant), "A" -> (eA -> scalan.util.Invariant))
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
    override lazy val typeArgs = TypeArgs("Env" -> (eEnv -> scalan.util.Invariant), "A" -> (eA -> scalan.util.Invariant))

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
    def productElement(n: Int) = n match {
      case 0 => eEnv
      case 1 => eA
    }
  }
  case class ReaderBaseIsoElem[Env, A](eEnv: Elem[Env], eA: Elem[A]) extends Elem[ReaderBaseIso[Env, A]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new ReaderBaseIso[Env, A]()(eEnv, eA))
    lazy val tag = {
      implicit val tagEnv = eEnv.tag
      implicit val tagA = eA.tag
      weakTypeTag[ReaderBaseIso[Env, A]]
    }
    lazy val typeArgs = TypeArgs("Env" -> (eEnv -> scalan.util.Invariant), "A" -> (eA -> scalan.util.Invariant))
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
  val dump = "H4sIAAAAAAAAALVWTWwbRRR+dhw7tkMTGtEWqjQhcvkJ1K7ooUgRQm7ikFTOj7KFFrdqNd4dO1t2Z5fdsWtzKLdKwA0hJJA4FIG4REioF8QZJIRQD4gbJw6cWirUA5WQingz++vKmxIh9rDamZ19783383Z2fodR14GnXJUYhJVNyklZkc9Vl5eUGuM6769ZWsegS7R1ceXTv86b7xxMw2QDstvEXXKNBuS9h1rPDp8VrtUhT5hKXW45Locn6zJDRbUMg6pct1hFN80OJ02DVuq6yxfqkGlaWv9NuAqpOkyqFlMdyqmyaBDXpa4/P0ZFRXo4zstxf8OOcrCK2EUltoszDtE5lo85Jr31W9RW+sxifZPDPr+0DVuUhWuKtGfjHlZN25BpRuqQ003bcniQNYcZti0tGGYYwQnYX79MuqSCWdsVhTs6a4tgNlHfIG26jkvE8gzuwaVG60zfpn7wosu1gXw9GwCQlRdkYeUIs3KIWVlgVlKooxNDf4uIl5uO1euDd6VGAHo2hnj+ISGCCLTGtNK7F9Tz95SimRYf90QpOVlQFgPNJChE0oPYfr/1vnv3lesn01BoQEF3q02XO0TlcRn4cBUJYxaXNYcIEqeNDM4lMSizVHHNAzLJq5ZpE4aRfCzHkShDV3UuFou5cZ+eBOxz3KbB0lTPToX7nU3Yr9TSIjGMzVuPHzt6u3YuDenBFHkMqaAZnCAoh+wWJRp1/ODiPsFhpMa6EcY4karKobjle9E9t0s1IS5P37qjfXccLqRDNP3k/45ADLH/xY+/OUo3v0zDWEPqfdkgbUmlgGuJumoDxqwudbz5XJcY4mkonTmNtkjH4D7IcXRGEB0Os4lWtamAbkFaIBVsv+ipeN1itLS8WfpT+eGDHSFSB8a9N553/9ZP3v9lX4tL/SLAToeFYKPjQyiOJHFr0+UOU39e/WhqYvrSr5LZrGaZRJfyOlyHUQfNLbdy2Id2j0QWvHoVy6SPzt3VL15/j0vKUr3B/rHRvIx+XZDfHdmFvaC1fXXt2mN/fHZpStpvrKlzk9il43swX+CV/9FcEKLiIXUwGovbDFI25RnlFHHpYjz5TOyrmIueSAUikYs4ZLCPdQMaMjWDmgme81gZHiRNq7uEGEIuh0JUtwwSSm06WWqIzOu3tfKhO9NX0pA9DaMttJOLGmtaHaYFkON/kNMePxXMpQYhR4iJQ8zw99gl2M+Rcg4HAot1uG5UXvPnPWPhNQsRrnEJO3DAL1l8V15lXkReeu7rnSv6zWeXpblkvsVhkWLwrMQxioF4DB4gBfU7OPOf+mU+9ueKSUyO5/0CHq7ECa+EISqM+nmc6kRABiS3snfgxL0eZZpHhsoJolqiqkEcqomTBDXxpOPZ/MSHL589fejsq7LRjGtykfcmbMTDz2VrxF6Qp4hndjlF4KJSzbTxlIgPJ7596ae3f/zicymSCFQOOZ9RDo8ExVuMaG64p7mEPSl+S0GNXL33yfr8zRu/yb5cEM0J/wcsPJRFtgj/er5Q8msyF56xYoJBc4t2FZOF9Nf2P/eyfsYSCwAA"
}
}

trait ReadersDslStd extends impl.ReadersStd {self: MonadsDslStd =>}
trait ReadersDslExp extends impl.ReadersExp {self: MonadsDslExp =>}
