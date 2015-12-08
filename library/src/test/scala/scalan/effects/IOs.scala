package scalan.examples

import scala.reflect.runtime.universe._
import scalan._
import scalan.monads._

trait IOs { self: IOsDsl =>

  type RepIO[A] = Rep[IO[A]]
  trait IO[A] extends Def[IO[A]] {
    implicit def eA: Elem[A]
    def toOper: Rep[Oper[A]]
  }
  trait IOCompanion

  abstract class ReadFile(val fileName: Rep[String]) extends IO[List[String]] {
    def eA: Elem[List[String]] = element[List[String]]
    def toOper = readFileIO(fileName) 
  }
  trait ReadFileCompanion

  abstract class WriteFile(val fileName: Rep[String], val lines: Rep[List[String]]) 
    extends IO[Unit] {
    def eA: Elem[Unit] = element[Unit]
    def toOper = writeFileIO(fileName, lines)
  }
  trait WriteFileCompanion
}

trait IOsDsl extends ScalanDsl with impl.IOsAbs with IOs
    with MonadsDsl {

  implicit def ioCont: Cont[IO] = new Cont[IO] {
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[IO[T]]
    def lift[T](implicit eT: Elem[T]) = element[IO[T]]
  }

  class IOs[F[_]:Cont](implicit I: Inject[IO,F]) {
    def readFile(fileName: Rep[String]): Rep[Free[F,List[String]]] = lift(ReadFile(fileName))
    def writeFile(fileName: Rep[String], lines: Rep[List[String]]): Rep[Free[F,Unit]] = lift(WriteFile(fileName, lines))
  }
  object IOs {
    implicit def instance[F[_]:Cont](implicit I: Inject[IO,F]): IOs[F] = new IOs[F]
  }

  object IOOper extends (IO ~> Oper) {
    def cIn = container[IO]
    def cOut = container[Oper]
    def apply[A:Elem](i: Rep[IO[A]]): Rep[Oper[A]] = i.toOper
  }

  def readFileIO(s: Rep[String]): Rep[Oper[List[String]]]
  def writeFileIO(s: Rep[String], lines: Rep[List[String]]): Rep[Oper[Unit]]
}

trait IOsDslSeq extends ScalanCtxSeq with impl.IOsSeq with MonadsDslSeq {
  def readFileIO(s: Rep[String]): Rep[Oper[List[String]]] = ???
  def writeFileIO(s: Rep[String], lines: Rep[List[String]]): Rep[Oper[Unit]] = ???
}

trait IOsDslExp extends ScalanCtxExp with impl.IOsExp with MonadsDslExp {
  def readFileIO(s: Rep[String]): Rep[Oper[List[String]]] =
    fun { i => ReadF(i, s) }
  def writeFileIO(s: Rep[String], lines: Rep[List[String]]): Rep[Oper[Unit]] =
    fun { (i: Rep[Int]) => WriteF(i, s, lines) }

  case class ReadF(i: Rep[Int], fileName: Rep[String]) extends BaseDef[(Int, List[String])]

  case class WriteF(i: Rep[Int], file: Rep[String], lines: Rep[List[String]]) extends BaseDef[(Int, Unit)]
}