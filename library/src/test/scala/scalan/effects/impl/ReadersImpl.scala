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
    override def convert(x: Rep[Reifiable[_]]) = {
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
    cachedIso[ReaderBaseIso[Env, A]](eEnv, eA)

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
  }

  case class ExpReaderBase[Env, A]
      (override val run: Rep[Env => A])
      (implicit eEnv: Elem[Env], eA: Elem[A])
    extends ReaderBase[Env, A](run) with UserTypeDef[ReaderBase[Env, A]] {
    lazy val selfType = element[ReaderBase[Env, A]]
  }

  lazy val ReaderBase: Rep[ReaderBaseCompanionAbs] = new ReaderBaseCompanionAbs with UserTypeDef[ReaderBaseCompanionAbs] {
    lazy val selfType = element[ReaderBaseCompanionAbs]
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
  val dump = "H4sIAAAAAAAAALVWPYwbRRR+3vOdz/aRCwQFhSi542RARMQ+0aS4IvJdfBDk+5E3BTIRaLw7dibszuztjC2bIgUldIiGAqH06Wio6JAQBVUESFQUVCEUEZAKxJvZPzvyHgHEFqOd2bfv5/u+93bv3IdFGcIL0iEe4XWfKlK3zX1Tqprd4oqpyZ5whx69Qvvvnf7c2ePb0oLVLizdIPKK9LpQjm5a4yC9t+lRG8qEO1QqEUoFz7VNhIYjPI86igneYL4/VKTn0UabSbXVhmJPuJMjuAWFNpx0BHdCqqi94xEpqYzPl6nOiKX7stlPDoIsBm/oKhpTVVwLCVOYPsY4Gdl3aGBPuOATX8GJOLWDQKeFNiXmByJUSYgSursh3GRb5AQP4Kn2TTIiDQwxaNgqZHyAb1YD4rxDBnQfTbR5EROW1OtfmwRmv9CGiqRHCNBVP/DMyTgAAGTgFZNEPcOnnuJT1/jUbBoy4rF3iX54GIrxBKKrsAAwDtDFy3/jIvFAW9ytvX/defOhXfUt/fJYp1IyFS6ho7UcNRgqEMevOh/KB6/evmRBpQsVJps9qULiqGnKY7SqhHOhTM4pgCQcIFsbeWyZKE20eUQSZUf4AeHoKYZyBXnymMOUNtZnKzE7OdCXVEAT08I4KKT1rufUa3SzQzzv8N6Zi8//3HrDAms2RBld2ij8MHGqYKlDiUvD2LleVxUstPgowxgPCk2z1Ut5nK2lY7JJcXnx3i/ul5tw3UrRjIM/HoHoYlF+/2317kuXLVjuGrnvemTQRUBly6P+QbgjuOrCshjRMHpSGhFP380ltOTSPhl6KoZ5Gp8FxEfBem5jBlSDt2WaoJAAUI10vC84re0e1n63v/7ojpZpCCvRk6hT/2SX/vjhRF8ZBSPE4ZCncGN/p2Ccz2M3oLtD7ty9+vGp1XNv/2i4XXKFT5gR2Nk2LIbY3aaUszG4/5DKSpSvLXz65MYD9tbtD5QhrTCeHSAHvZvYsVvmvfPH8JcMst+6m9avZ7771IIy0tRjyidBbfMx2+9/bClIkciWNeTmVNQT20TSnemIaxmQz0w1zLOFRA3GSEERR9YowbuoNZrTXhH8851YtHmMizksKqhkeRsnqabO5WsK4TjdaT/t3b/8hQWLr8NiH/tGoph6YsjdBGf8vCk6VtvJWWEWZ8SVhMRPcTXXOmSYParD7Xkmc2qaKvoizCJQ7lDWZ/prMXv+nwZa3AuZ6YU4do5SVqNgc1SSjdZpKnJr/3fw6PW1zCY2LMUIKHgi4V1w4sq4oBA2cuRgx92CLXvr4Sf7F7757CczZiq673C88fQnIyM/HeMJLXsmFv4zTGWLEtadaDL9C4zVXGfDCQAA"
}
}

trait ReadersDslSeq extends impl.ReadersSeq {self: MonadsDslSeq =>}
trait ReadersDslExp extends impl.ReadersExp {self: MonadsDslExp =>}
