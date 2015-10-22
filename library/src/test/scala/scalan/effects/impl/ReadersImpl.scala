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
    lazy val entityDef: STraitOrClassDef = {
      val module = getModules("Readers")
      module.entities.find(_.name == "Reader").get
    }
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

  // elem for concrete class
  class ReaderBaseElem[Env, A](val iso: Iso[ReaderBaseData[Env, A], ReaderBase[Env, A]])(implicit eEnv: Elem[Env], eA: Elem[A])
    extends ReaderElem[Env, A, ReaderBase[Env, A]]
    with ConcreteElem[ReaderBaseData[Env, A], ReaderBase[Env, A]] {
    override lazy val parent: Option[Elem[_]] = Some(readerElement(element[Env], element[A]))
    override lazy val entityDef = {
      val module = getModules("Readers")
      module.concreteSClasses.find(_.name == "ReaderBase").get
    }
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

  registerModule(scalan.meta.ScalanCodegen.loadModule(Readers_Module.dump))
}

// Seq -----------------------------------
trait ReadersSeq extends ReadersDsl with scalan.ScalanSeq {
  self: MonadsDslSeq =>
  lazy val Reader: Rep[ReaderCompanionAbs] = new ReaderCompanionAbs with Def[ReaderCompanionAbs] {
  }

  case class SeqReaderBase[Env, A]
      (override val run: Rep[Env => A])
      (implicit eEnv: Elem[Env], eA: Elem[A])
    extends ReaderBase[Env, A](run)
        with Def[ReaderBase[Env, A]] {
    lazy val selfType = element[ReaderBase[Env, A]]
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
  lazy val Reader: Rep[ReaderCompanionAbs] = new ReaderCompanionAbs with Def[ReaderCompanionAbs] {
  }

  case class ExpReaderBase[Env, A]
      (override val run: Rep[Env => A])
      (implicit eEnv: Elem[Env], eA: Elem[A])
    extends ReaderBase[Env, A](run) with Def[ReaderBase[Env, A]] {
    lazy val selfType = element[ReaderBase[Env, A]]
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
  val dump = "H4sIAAAAAAAAALVWPYwbRRR+3rPPZ/vIBYKCQpTccXJARMQ+0aS4IvJdfBDk+9FtCuREoPHu2NmwO7M3M7ZsihSU0CEaCoTSp6OhokNCFFQRIFFRUIVQREAqEG9m/3wn7xFAuBjNzL59P9/3vee99xBKUsCL0iE+YY2AKtKwzb4lVd1uM+WpyTZ3hz69Svvvnf7c2WYb0oKlLszfIvKq9LtQiTbtcZjubXrQgQphDpWKC6nghY6J0HS471NHeZw1vSAYKtLzabPjSbXegWKPu5MDuAOFDpx0OHMEVdTe9ImUVMb3C1Rn5KXnijlPdsMsBmvqKppTVVwXxFOYPsY4Gdnv09CeMM4mgYITcWq7oU4LbcpeEHKhkhBldHeLu8mxyAhewDOd22REmhhi0LSV8NgA36yFxHmHDOgOmmjzIiYsqd+/PgnNea4DVUkPEKBrQeibm3EIAMjAqyaJRoZPI8WnofGp21R4xPfeJfrhnuDjCUS/whzAOEQXr/yNi8QDbTO3/v5N58ZjuxZY+uWxTqVsKpxHR8s5ajBUII5f7X8oH71297IF1S5UPdnqSSWIo6Ypj9GqEca4MjmnABIxQLZW89gyUVpoc0QSFYcHIWHoKYZyEXnyPcdT2ljfLcbs5EBfViFNTAvjsJDWu5JTr9HNJvH9vQdnLl34uf2mBdbhEBV0aaPwReJUwfw+JS4VsXO9LimYa7NRhjFeFFrmqJfKOFvLx2ST4vLSg1/cL9fgppWiGQd/MgLRRUl+/23t/stXLFjoGrlv+WTQRUBl26fBrtjkTHVhgY+oiJ6UR8TXu5mEll3aJ0NfxTBP4zOH+ChYyW3MkGrw1k0TFBIAapGOdzij9a29+u/21x/d0zIVsBg9iTr1T+/yHz+c6CujYIRYDFkKN/Z3Csb5PHZDujVkzv1rH59aOvf2j4bbeZcHxDMCO9uBksDuNqWcjcH9h1RWo3xtHtCnVx95b939QBnSCuPDA2S3dxs7dt28d/4Y/pJB9lt3zfr1zHefWlBBmnqeCkhYX3vC9vsfWwpSJLJlGbk5FfXEBpF0czricgbkc1MN83whUYMxUlDEkTVK8C5qjea0VwT/bCcWbR3jYgaLCqpZ3sZJqqlz+ZpCOE7vd571H175woLSG1DqY99IFFOPD5mb4Ix/b4qO1UZyVziMM+JKBAlSXM1vBTLMjupwY5bJjJqmir4ER0BEYR2++U+jLO6CzPRiHDVHI0tRsBn6yIbqNAm5Vf87YPT6emYTG5ZjBBQ8lTDOGXFlXJCA1Rwh2HGfIKZ3Hn+yc/Gbz34yA6aqOw4HG0s/LzLa0wEeA1vZNrHwa2EqWxSv7kGT6V+6KQ2ovQkAAA=="
}
}

trait ReadersDslSeq extends impl.ReadersSeq {self: MonadsDslSeq =>}
trait ReadersDslExp extends impl.ReadersExp {self: MonadsDslExp =>}
