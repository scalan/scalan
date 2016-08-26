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
    lazy val typeArgs = TypeArgs("Env" -> eEnv, "A" -> eA)
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
    override lazy val typeArgs = TypeArgs("Env" -> eEnv, "A" -> eA)

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
    lazy val typeArgs = TypeArgs("Env" -> eEnv, "A" -> eA)
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
  val dump = "H4sIAAAAAAAAALVWTYgcRRR+Mzu7szOzZtes/yHZdZmorGYmmEOERcJkd1Y2zP6wHYmMwVDTXTPp2F3ddtUMPR4ieMhBPYkICh4CipcgiDcP4iGCiAiKN08ePMWI5GBOiq+qf2eZ3hjFORRdNa/fz/d971VfuwmT3IPHuE4swmo2FaSmqecGF1WtyYQphpuO0bfoGu1+/+SXTvn1t7byMNeGqYuEr3GrDaXgoem78bMmjBaUCNMpF47HBTzaUhHqumNZVBemw+qmbfcF6Vi03jK5WGlBoeMYw1fgMuRaMKc7TPeooNqqRTinPDyfpjIjM96X1H647SYxWF1WUU9VcdYjpsD0McZcYL9LXW3IHDa0BRwIU9t2ZVpoU6G+izVs2K6lwky0oGjaruOJKGoRI1x0jGhbYAQP4GDrEhmQOkbt1TXhmawnnblEf5n06BaaSPMC1sCp1T07dGnovMKFMRLPdwEAWXlaJVZLMKvFmNUkZlWNeiaxzFeJ/HPHc/whBL/cBIDvooun7uAi8kCbzKi+cV5/8bZWsfPyZV+mUlQJTaGjhQyFKHoQ26933+a3nrt6Mg/lNpRN3uhw4RFdpGUQwlUhjDlC5RwjSLweMriUxaCK0kCbPTIp6Y7tEoaeQixnkCjL1E0hjeXZTEhPBvZF4dLINOe7ubjexYx6lZZWiWXt3Hj42NFfmy/kIT8aooQuNWwGL3IqYGqXEoN6oXO5zgqYaLJBgjEe5BpqK5eSn6zFfbKJcXn8xm/GV8fhfD5GMwz+zwhEFwefef/zo3TnkzxMt5Xe1y3SU1RKuNYo19sw7QyoF5wXB8SST2PpLBq0S/qWCEFOozOB6AhYzGxVl0roVlQL5KLyK4GKtxxGq+s71T+0b965JkXqwUzwT9C7f5kn//zpQFco/SLAXp/FYGPHx1AcyeLWpet9pv+48d787OELPytmpwzHJqaS16EWTHrY3KqUQyG0d0lkOchXc2x679It86WrbwpFWc4fnR/bnUvYryvqvSP7sBeNtk+vXLn/9w8vzKv2m+6YwiZu9fhdNF/UK/9jc0GMSoDUg8leLgtI2XzQKKcJp6vp4Aupt1Jd9EguEokyElDAOTaIaCg0LWpn9FzAyngnedrYx8UYcgWUk7yVk1hqh7Olhsg8sNu6z7p56os8TJ6ByS62E0eNdZw+MyLI8R4U1Beno7PcKOQIMfGIHUOsfouQYLZXnqvjTMbUlCr6GOwBEfU2evKf5lspddOkJKH2y2ECd1bObJDCGNUk8zdNTSYW/w4uuW4k/peR+loG9WtUt4hHDXnfUxu/R4JmPPHuqXNnHjr3vBoHM4YyCv6Jx+X4r6dN4q6ou/6Jfe56NKo2bRe/5fDhxPVnf3jt248/UnMygVJAMeRRwD1R8g4jBo9rWsqoSQsbH5Vx+fYHW8vfffaLmp5lOUJwarP40ykRb3w3hfIobapY+CWUQhdbUA6VlBiIXOjf5Jo1ObgKAAA="
}
}

trait ReadersDslStd extends impl.ReadersStd {self: MonadsDslStd =>}
trait ReadersDslExp extends impl.ReadersExp {self: MonadsDslExp =>}
