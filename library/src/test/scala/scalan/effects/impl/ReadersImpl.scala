package scalan.monads

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait ReadersAbs extends Readers with scalan.Scalan {
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
        case e => !!!(s"Expected $x to have ReaderElem[_, _, _], but got $e")
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
  implicit def proxyReaderCompanion(p: Rep[ReaderCompanion]): ReaderCompanion =
    proxyOps[ReaderCompanion](p)

  abstract class AbsReaderBase[Env, A]
      (run: Rep[Env => A])(implicit eEnv: Elem[Env], eA: Elem[A])
    extends ReaderBase[Env, A](run) with Def[ReaderBase[Env, A]] {
    lazy val selfType = element[ReaderBase[Env, A]]
  }
  // elem for concrete class
  class ReaderBaseElem[Env, A](val iso: Iso[ReaderBaseData[Env, A], ReaderBase[Env, A]])(implicit eEnv: Elem[Env], eA: Elem[A])
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
    extends Iso[ReaderBaseData[Env, A], ReaderBase[Env, A]] {
    override def from(p: Rep[ReaderBase[Env, A]]) =
      p.run
    override def to(p: Rep[Env => A]) = {
      val run = p
      ReaderBase(run)
    }
    lazy val eTo = new ReaderBaseElem[Env, A](this)
  }
  // 4) constructor and deconstructor
  class ReaderBaseCompanionAbs extends CompanionDef[ReaderBaseCompanionAbs] with ReaderBaseCompanion {
    def selfType = ReaderBaseCompanionElem
    override def toString = "ReaderBase"

    def apply[Env, A](run: Rep[Env => A])(implicit eEnv: Elem[Env], eA: Elem[A]): Rep[ReaderBase[Env, A]] =
      mkReaderBase(run)
  }
  object ReaderBaseMatcher {
    def unapply[Env, A](p: Rep[Reader[Env, A]]) = unmkReaderBase(p)
  }
  lazy val ReaderBase: Rep[ReaderBaseCompanionAbs] = new ReaderBaseCompanionAbs
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
    cachedIso[ReaderBaseIso[Env, A]](eEnv, eA)

  // 6) smart constructor and deconstructor
  def mkReaderBase[Env, A](run: Rep[Env => A])(implicit eEnv: Elem[Env], eA: Elem[A]): Rep[ReaderBase[Env, A]]
  def unmkReaderBase[Env, A](p: Rep[Reader[Env, A]]): Option[(Rep[Env => A])]

  registerModule(Readers_Module)
}

// Seq -----------------------------------
trait ReadersSeq extends ReadersDsl with scalan.ScalanSeq {
  self: MonadsDslSeq =>
  lazy val Reader: Rep[ReaderCompanionAbs] = new ReaderCompanionAbs {
  }

  case class SeqReaderBase[Env, A]
      (override val run: Rep[Env => A])(implicit eEnv: Elem[Env], eA: Elem[A])
    extends AbsReaderBase[Env, A](run) {
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
  val dump = "H4sIAAAAAAAAALVWv48bRRR+3rPPZ/vIBYKAJEruOBkQEbEPmhRXRL6LD4Kcu9NtCmQi0Hh37GzYndmbGVs2Rf4A6BANBYKUSOmoaBANEqKgigCJioIqhCICUoF4M/vLd/IeAYSL0czs2/fj+773vLfvQUkKeFY6xCesEVBFGrbZt6Sq222mPDW5wt2hTy/R/lP8i49f/OTUZxYsdWH+OpGXpN+FSrRpj8N0b9P9DlQIc6hUXEgFT3dMhKbDfZ86yuOs6QXBUJGeT5sdT6r1DhR73J3sw00odOC4w5kjqKL2pk+kpDK+X6A6Iy89V8x5shNmMVhTV9GcquKqIJ7C9DHG8ch+j4b2hHE2CRQci1PbCXVaaFP2gpALlYQoo7vr3E2ORUbwAh7r3CAj0sQQg6athMcG+GYtJM5bZEC30USbFzFhSf3+1UloznMdqEq6jwBdDkLf3IxDAEAGXjJJNDJ8Gik+DY1P3abCI773NtEPdwUfTyD6FeYAxiG6eOFvXCQeaJu59XeuOa8/sGuBpV8e61TKpsJ5dLScowZDBeL41d578v7Lty5YUO1C1ZOtnlSCOGqa8hitGmGMK5NzCiARA2RrNY8tE6WFNockUXF4EBKGnmIoF5En33M8pY313WLMTg70ZRXSxLQwDgtpvSs59RrdbBLf37178vwzP7dfs8A6GKKCLm0UvkicKpjfo8SlInau1yUFc202yjDGi0LLHPVSGWdr+YhsUlyeu/uL++UaXLNSNOPgD0cguijJ77+t3Xn+ogULXSP3LZ8MugiobPs02BGbnKkuLPARFdGT8oj4ejeT0LJL+2ToqxjmaXzmEB8FK7mNGVIN3rppgkICQC3S8TZntL61W//d/vr921qmAhajJ1Gn/uld+OOHY31lFIwQiyFL4cb+TsE4m8duSLeGzLlz+YMTS2fe/NFwO+/ygHhGYKc7UBLY3aaU0zG4/5DKapSvzQP66Op9741b7ypDWmF8cIDs9G5gx66b984ewV8yyH7rrlm/nvzuIwsqSFPPUwEJ62sP2X7/Y0tBikS2LCM3J6Ke2CCSbk5HXM6AfHKqYU4VEjUYIwVFHFmjBO+i1mhOe0Xwz3Zi0dYRLmawqKCa5W2cpJo6k68phOOJvc7j/r2Ln1tQehVKfewbiWLq8SFzE5zx703RsdpI7goHcUZciSBBiqv5rUCG2WEdbswymVHTVNHn4RCIKKyDN/9plMVdkJmei6PmaGQpCjZDH9lQnSYht+p/B4xeX8lsYsNyjICCRxLGOSOujAsSsJojBDvuE8T05oMPt8998+lPZsBUdcfhYGPp50VGezrAY2ArV0ws/FqYyhbFq3vQZPoXfV+7wL0JAAA="
}
}

trait ReadersDslSeq extends impl.ReadersSeq {self: MonadsDslSeq =>}
trait ReadersDslExp extends impl.ReadersExp {self: MonadsDslExp =>}
