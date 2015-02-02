package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.virtualization.lms.common._


trait SystemOps extends Base {

  def parallel_execute[T:Manifest](nJobs: Rep[Int], f: Rep[Int] => Rep[T]): Rep[Array[T]]
  def hash_code[T](obj: Rep[T]): Rep[Int]

  def arraySortBy[A: Manifest, B: Manifest](a: Rep[Array[A]], by: Rep[A] => Rep[B]): Rep[Array[A]] 

  def string_matches(str: Rep[String], pattern: Rep[String]): Rep[Boolean]
}


trait SystemOpsExp extends BaseExp with Functions with EffectExp {

  case class ParallelExecute[T:Manifest](val nJobs: Exp[Int], val f: Exp[Int => T]) extends Def[Array[T]] {
    val m = manifest[T]
  }

  case class HashCode[T](val obj: Exp[T]) extends Def[Int] {
  }

  case class ArraySortBy[A: Manifest, B: Manifest](a: Exp[Array[A]], by: Exp[A => B]) extends Def[Array[A]] {
    val mA = manifest[A]
    val mB = manifest[B]
  }

  case class StringMatches(val str: Exp[String], val pattern: Exp[String]) extends Def[Boolean] {
  }

 def parallel_execute[T:Manifest](nJobs: Rep[Int], f: Rep[Int] => Rep[T]) = {
    ParallelExecute (nJobs, f)
  }

  def hash_code[T](obj: Exp[T]) = {
    HashCode(obj)
  }

  def arraySortBy[A: Manifest, B: Manifest](a: Exp[Array[A]], by: Rep[A] => Rep[B]): Exp[Array[A]] = ArraySortBy(a, by)

  def sortBy[A: Manifest, B: Manifest](a: Exp[Array[A]], by: Rep[A => B]): Exp[Array[A]] = ArraySortBy(a, by)

  def string_matches(str: Exp[String], pattern: Exp[String]) = {
    StringMatches(str, pattern)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = {
    (e match {
      case ParallelExecute(nJobs, job) => ParallelExecute(f(nJobs), f(job))(mtype(manifest[A]))
      case StringMatches(str, pattern) => StringMatches(f(str), f(pattern))
      case HashCode(obj) => HashCode(f(obj))
      case a@ArraySortBy(arr, by) => sortBy(f(arr), f(by))(a.mA, a.mB)
      case Reflect(a@ArraySortBy(arr, by), u, es) => reflectMirrored(Reflect(ArraySortBy(f(arr), f(by))(a.mA, a.mB), mapOver(f, u), f(es)))(mtype(manifest[A]), pos)
      case _ => super.mirror(e, f)
    }).asInstanceOf[Exp[A]] // why??
  }
}

trait ScalaGenSystemOps extends ScalaGenBase {
  val IR: SystemOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case pe@ParallelExecute(nJobs, f) =>
      val m = remap(pe.m)
      val n = quote(nJobs)
      stream.println("// generating parallel execute")
      stream.println("val " + quote(sym) + " ={")
      //stream.println("\tval tasks: Seq[scala.concurrent.Future[" + m + "]] = for (i <- 0 until " + n + ") yield future {" ote(f) + "(i) }")
      stream.println("\timport scala.concurrent.ExecutionContext.Implicits.global")
      stream.println("\tval tasks = for (i <- 0 until " + n + ") yield scala.concurrent.future {" + quote(f) + "(i) }")
      stream.println("\tscala.concurrent.Await.result(scala.concurrent.Future.sequence(tasks), scala.concurrent.duration.Duration.Inf).toArray")
      stream.println("}")
    case StringMatches(str, pattern) =>
      stream.println("val " + quote(sym) + " = " + quote(str) + ".matches(" + quote(pattern) + ")")
    case HashCode(obj) =>
      stream.println("val " + quote(sym) + " = " + quote(obj) + ".hashCode")
    case a@ArraySortBy(arr, by) =>
      stream.println("val " + quote(sym) + " = " + quote(arr) + ".sortBy(" + quote(by)+ ")")
    case _ => super.emitNode(sym, rhs)
  }

}
