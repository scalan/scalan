package scalan.monads

import scala.reflect.runtime.universe._
import scalan._
import scalan.monads._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait IOsAbs extends scalan.ScalanDsl with IOs {
  self: IOsDsl =>

  // single proxy for each type family
  implicit def proxyIO[A](p: Rep[IO[A]]): IO[A] = {
    proxyOps[IO[A]](p)(scala.reflect.classTag[IO[A]])
  }

  // familyElem
  class IOElem[A, To <: IO[A]](implicit _eA: Elem[A])
    extends EntityElem[To] {
    def eA = _eA
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[IO[A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[IO[A]] => convertIO(x) }
      tryConvert(element[IO[A]], this, x, conv)
    }

    def convertIO(x: Rep[IO[A]]): Rep[To] = {
      x.selfType1 match {
        case _: IOElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have IOElem[_, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def iOElement[A](implicit eA: Elem[A]): Elem[IO[A]] =
    cachedElem[IOElem[A, IO[A]]](eA)

  implicit case object IOCompanionElem extends CompanionElem[IOCompanionAbs] {
    lazy val tag = weakTypeTag[IOCompanionAbs]
    protected def getDefaultRep = IO
  }

  abstract class IOCompanionAbs extends CompanionDef[IOCompanionAbs] with IOCompanion {
    def selfType = IOCompanionElem
    override def toString = "IO"
  }
  def IO: Rep[IOCompanionAbs]
  implicit def proxyIOCompanionAbs(p: Rep[IOCompanionAbs]): IOCompanionAbs =
    proxyOps[IOCompanionAbs](p)

  abstract class AbsReadFile
      (fileName: Rep[String])
    extends ReadFile(fileName) with Def[ReadFile] {
    lazy val selfType = element[ReadFile]
  }
  // elem for concrete class
  class ReadFileElem(val iso: Iso[ReadFileData, ReadFile])
    extends IOElem[List[String], ReadFile]
    with ConcreteElem[ReadFileData, ReadFile] {
    override lazy val parent: Option[Elem[_]] = Some(iOElement(listElement(StringElement)))
    override lazy val typeArgs = TypeArgs()

    override def convertIO(x: Rep[IO[List[String]]]) = // Converter is not generated by meta
!!!("Cannot convert from IO to ReadFile: missing fields List(fileName)")
    override def getDefaultRep = ReadFile("")
    override lazy val tag = {
      weakTypeTag[ReadFile]
    }
  }

  // state representation type
  type ReadFileData = String

  // 3) Iso for concrete class
  class ReadFileIso
    extends EntityIso[ReadFileData, ReadFile] with Def[ReadFileIso] {
    override def from(p: Rep[ReadFile]) =
      p.fileName
    override def to(p: Rep[String]) = {
      val fileName = p
      ReadFile(fileName)
    }
    lazy val eFrom = element[String]
    lazy val eTo = new ReadFileElem(self)
    lazy val selfType = new ReadFileIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class ReadFileIsoElem() extends Elem[ReadFileIso] {
    def getDefaultRep = reifyObject(new ReadFileIso())
    lazy val tag = {
      weakTypeTag[ReadFileIso]
    }
    lazy val typeArgs = TypeArgs()
  }
  // 4) constructor and deconstructor
  class ReadFileCompanionAbs extends CompanionDef[ReadFileCompanionAbs] with ReadFileCompanion {
    def selfType = ReadFileCompanionElem
    override def toString = "ReadFile"

    @scalan.OverloadId("fromFields")
    def apply(fileName: Rep[String]): Rep[ReadFile] =
      mkReadFile(fileName)

    def unapply(p: Rep[IO[List[String]]]) = unmkReadFile(p)
  }
  lazy val ReadFileRep: Rep[ReadFileCompanionAbs] = new ReadFileCompanionAbs
  lazy val ReadFile: ReadFileCompanionAbs = proxyReadFileCompanion(ReadFileRep)
  implicit def proxyReadFileCompanion(p: Rep[ReadFileCompanionAbs]): ReadFileCompanionAbs = {
    proxyOps[ReadFileCompanionAbs](p)
  }

  implicit case object ReadFileCompanionElem extends CompanionElem[ReadFileCompanionAbs] {
    lazy val tag = weakTypeTag[ReadFileCompanionAbs]
    protected def getDefaultRep = ReadFile
  }

  implicit def proxyReadFile(p: Rep[ReadFile]): ReadFile =
    proxyOps[ReadFile](p)

  implicit class ExtendedReadFile(p: Rep[ReadFile]) {
    def toData: Rep[ReadFileData] = isoReadFile.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoReadFile: Iso[ReadFileData, ReadFile] =
    reifyObject(new ReadFileIso())

  // 6) smart constructor and deconstructor
  def mkReadFile(fileName: Rep[String]): Rep[ReadFile]
  def unmkReadFile(p: Rep[IO[List[String]]]): Option[(Rep[String])]

  abstract class AbsWriteFile
      (fileName: Rep[String], lines: Rep[List[String]])
    extends WriteFile(fileName, lines) with Def[WriteFile] {
    lazy val selfType = element[WriteFile]
  }
  // elem for concrete class
  class WriteFileElem(val iso: Iso[WriteFileData, WriteFile])
    extends IOElem[Unit, WriteFile]
    with ConcreteElem[WriteFileData, WriteFile] {
    override lazy val parent: Option[Elem[_]] = Some(iOElement(UnitElement))
    override lazy val typeArgs = TypeArgs()

    override def convertIO(x: Rep[IO[Unit]]) = // Converter is not generated by meta
!!!("Cannot convert from IO to WriteFile: missing fields List(fileName, lines)")
    override def getDefaultRep = WriteFile("", element[List[String]].defaultRepValue)
    override lazy val tag = {
      weakTypeTag[WriteFile]
    }
  }

  // state representation type
  type WriteFileData = (String, List[String])

  // 3) Iso for concrete class
  class WriteFileIso
    extends EntityIso[WriteFileData, WriteFile] with Def[WriteFileIso] {
    override def from(p: Rep[WriteFile]) =
      (p.fileName, p.lines)
    override def to(p: Rep[(String, List[String])]) = {
      val Pair(fileName, lines) = p
      WriteFile(fileName, lines)
    }
    lazy val eFrom = pairElement(element[String], element[List[String]])
    lazy val eTo = new WriteFileElem(self)
    lazy val selfType = new WriteFileIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class WriteFileIsoElem() extends Elem[WriteFileIso] {
    def getDefaultRep = reifyObject(new WriteFileIso())
    lazy val tag = {
      weakTypeTag[WriteFileIso]
    }
    lazy val typeArgs = TypeArgs()
  }
  // 4) constructor and deconstructor
  class WriteFileCompanionAbs extends CompanionDef[WriteFileCompanionAbs] with WriteFileCompanion {
    def selfType = WriteFileCompanionElem
    override def toString = "WriteFile"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[WriteFileData]): Rep[WriteFile] =
      isoWriteFile.to(p)
    @scalan.OverloadId("fromFields")
    def apply(fileName: Rep[String], lines: Rep[List[String]]): Rep[WriteFile] =
      mkWriteFile(fileName, lines)

    def unapply(p: Rep[IO[Unit]]) = unmkWriteFile(p)
  }
  lazy val WriteFileRep: Rep[WriteFileCompanionAbs] = new WriteFileCompanionAbs
  lazy val WriteFile: WriteFileCompanionAbs = proxyWriteFileCompanion(WriteFileRep)
  implicit def proxyWriteFileCompanion(p: Rep[WriteFileCompanionAbs]): WriteFileCompanionAbs = {
    proxyOps[WriteFileCompanionAbs](p)
  }

  implicit case object WriteFileCompanionElem extends CompanionElem[WriteFileCompanionAbs] {
    lazy val tag = weakTypeTag[WriteFileCompanionAbs]
    protected def getDefaultRep = WriteFile
  }

  implicit def proxyWriteFile(p: Rep[WriteFile]): WriteFile =
    proxyOps[WriteFile](p)

  implicit class ExtendedWriteFile(p: Rep[WriteFile]) {
    def toData: Rep[WriteFileData] = isoWriteFile.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoWriteFile: Iso[WriteFileData, WriteFile] =
    reifyObject(new WriteFileIso())

  // 6) smart constructor and deconstructor
  def mkWriteFile(fileName: Rep[String], lines: Rep[List[String]]): Rep[WriteFile]
  def unmkWriteFile(p: Rep[IO[Unit]]): Option[(Rep[String], Rep[List[String]])]

  registerModule(IOs_Module)
}

// Std -----------------------------------
trait IOsStd extends scalan.ScalanDslStd with IOsDsl {
  self: IOsDslStd =>

  lazy val IO: Rep[IOCompanionAbs] = new IOCompanionAbs {
  }

  case class StdReadFile
      (override val fileName: Rep[String])
    extends AbsReadFile(fileName) {
  }

  def mkReadFile
    (fileName: Rep[String]): Rep[ReadFile] =
    new StdReadFile(fileName)
  def unmkReadFile(p: Rep[IO[List[String]]]) = p match {
    case p: ReadFile @unchecked =>
      Some((p.fileName))
    case _ => None
  }

  case class StdWriteFile
      (override val fileName: Rep[String], override val lines: Rep[List[String]])
    extends AbsWriteFile(fileName, lines) {
  }

  def mkWriteFile
    (fileName: Rep[String], lines: Rep[List[String]]): Rep[WriteFile] =
    new StdWriteFile(fileName, lines)
  def unmkWriteFile(p: Rep[IO[Unit]]) = p match {
    case p: WriteFile @unchecked =>
      Some((p.fileName, p.lines))
    case _ => None
  }
}

// Exp -----------------------------------
trait IOsExp extends scalan.ScalanDslExp with IOsDsl {
  self: IOsDslExp =>

  lazy val IO: Rep[IOCompanionAbs] = new IOCompanionAbs {
  }

  case class ExpReadFile
      (override val fileName: Rep[String])
    extends AbsReadFile(fileName)

  object ReadFileMethods {
    object toOper {
      def unapply(d: Def[_]): Option[Rep[ReadFile]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ReadFileElem] && method.getName == "toOper" =>
          Some(receiver).asInstanceOf[Option[Rep[ReadFile]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ReadFile]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ReadFileCompanionMethods {
  }

  def mkReadFile
    (fileName: Rep[String]): Rep[ReadFile] =
    new ExpReadFile(fileName)
  def unmkReadFile(p: Rep[IO[List[String]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ReadFileElem @unchecked =>
      Some((p.asRep[ReadFile].fileName))
    case _ =>
      None
  }

  case class ExpWriteFile
      (override val fileName: Rep[String], override val lines: Rep[List[String]])
    extends AbsWriteFile(fileName, lines)

  object WriteFileMethods {
    object toOper {
      def unapply(d: Def[_]): Option[Rep[WriteFile]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WriteFileElem] && method.getName == "toOper" =>
          Some(receiver).asInstanceOf[Option[Rep[WriteFile]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[WriteFile]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object WriteFileCompanionMethods {
  }

  def mkWriteFile
    (fileName: Rep[String], lines: Rep[List[String]]): Rep[WriteFile] =
    new ExpWriteFile(fileName, lines)
  def unmkWriteFile(p: Rep[IO[Unit]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: WriteFileElem @unchecked =>
      Some((p.asRep[WriteFile].fileName, p.asRep[WriteFile].lines))
    case _ =>
      None
  }

  object IOMethods {
    object toOper {
      def unapply(d: Def[_]): Option[Rep[IO[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IOElem[_, _]] && method.getName == "toOper" =>
          Some(receiver).asInstanceOf[Option[Rep[IO[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IO[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object IOCompanionMethods {
  }
}

object IOs_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWTWwbRRQe24l/4lDSINKCFJEal7QF7IoKFSlCKCQOpHJtK9s04FZF492xO2F2dtkduzaHcqsE3BDigMShCMQlQkJcEEhcAAkh1ANXzpxaqqoHKiGBeDP747VrB3PAh9Hu7Jv3873vfZ6939G066AnXB0zzAsmEbigqedVV+S1EhdU9M5aRpuRddK89PLHf14w3z4UR3N1lLyM3XWX1VHGeyh17fBZE0YZZTDXiSssxxXoSFlFKOoWY0QX1OJFapptgRuMFMvUFStlNNWwjN4b6CqKldGcbnHdIYJoawy7LnH9/TSRGdHwPaPee1W7H4MXZRXFSBXnHEwFpA8x5jz7LWJrPW7xninQAT+1qi3TApss6dpQw6ZpMxUmUUYpatqWI4KoKYhw2TKC1ymOYQPNl3dxBxchaquoCYfylnRmY/113CIVMJHmU1CDS1jzXM8mvvOsK4yBeF0bIQRdeUYlVuhjVggxK0jM8hpxKGb0TSw/1hyr20PeL5ZAqGuDi6f+xUXggZS4kX/non7hnpY14/JwV6aSUgklwdFjYxii2gPY/rj1nnv3peun42imjmaou9pwhYN1EaWBD1cWc24JlXOIIHZa0MHcuA6qKKtgM0STjG6ZNubgycdyFhrFqE6FNJZ7s357xmCfEjYJTGNdOxbWuzSmXsWlNcxY7eYjTx+9VXoljuKDITLgUoNhcAKnAsU3q75juT4owAywD2M9Pi6WTWoONYHvHfLsd99s3/m2Mq3CzRukidtMnMesTTyq+cH7iai4uZxAyb5Bpju8pvYpNQR9+eZt44eT6GI8bJVf2WTsABfzz3349VFS+zyO0nU1TBsMtxRPZC/WiavXUdrqEMfbT3Uwk08juZLyi/c7GIU+AdALtDRWB2wi+7Ki5isWlJ/1RqRicZLfqOX/0H56f09OgINmvS+eMPxNT//164GmUMMhULpJmRrqoKUJ0BQPD7kcGgX0jOdPs0xyMHeXXrr+rlCQxrqD4lFt7MKwrqhzR/ZBN9C1L65de/jOJ689pGYv3aDCxHb+5H+YvGBQ/sfJQgqEvqY82n+XSw4gPbhFsLEBsK5FQ+eGzwD2geHQ92xscMoGx24BNo4dh9nb5lTc3x4VIGK+GHJERZm046PPTjPKiXv/wRF6MJI90SSX1XpiAkDndxwqyCSIZkLLvkGknqQfcjDpBFBvP8BhI7YacTUG86E6Ji8vu1kdUZcnt+o9lKfF8QILFH31llE4fHvxShwlz6DpJuiOW0bTDavNjYD7cBsRpCteDPZig9wHrmMHm+ElpYPhXxVmT6CFQIvagrLieX/fUyD4LaF+4iFakPCCn7A8Vdjknj+Rf/KrvSv0xvENpUHDcFb6NS+Di8KYmteJzrBDDHndICZchzw5OPXBCztnDu9sK0GaNZSR9yUU1NGXt7PYXlFXjWP7XDXAKF8ybbhKwsOp75//5a2fP/s0rCLl158A+gj0QJC4xbHhhvXkxtSj+bIDZLx676PKiRtf/qb+I2ekgIGm8/DWFv1vHKRpEgLDDSzCVRhJqWcRRjblsvsP0WndQzALAAA="
}
}

