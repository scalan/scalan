package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.lms.common._


trait SystemOps extends Base {

  def parallel_execute[T:Manifest](nJobs: Rep[Int], f: Rep[Int] => Rep[T]): Rep[Array[T]]
  def hash_code[T](obj: Rep[T]): Rep[Int]

  def string_matches(str: Rep[String], pattern: Rep[String]): Rep[Boolean]
}


trait SystemOpsExp extends BaseExp with Functions with EffectExp {

  case class ParallelExecute[T:Manifest](val nJobs: Exp[Int], val f: Exp[Int => T]) extends Def[Array[T]] {
    val m = manifest[T]
  }

  case class HashCode[T](val obj: Exp[T]) extends Def[Int] {
  }

  case class StringMatches(val str: Exp[String], val pattern: Exp[String]) extends Def[Boolean] {
  }

 def parallel_execute[T:Manifest](nJobs: Rep[Int], f: Rep[Int] => Rep[T]): Exp[Array[T]] =
    ParallelExecute(nJobs, f)

  def hash_code[T](obj: Exp[T]): Exp[Int] =
    HashCode(obj)

  def string_matches(str: Exp[String], pattern: Exp[String]): Exp[Boolean] = {
    StringMatches(str, pattern)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = {
    (e match {
      case ParallelExecute(nJobs, job) => ParallelExecute(f(nJobs), f(job))(mtype(manifest[A]))
      case StringMatches(str, pattern) => StringMatches(f(str), f(pattern))
      case HashCode(obj) => HashCode(f(obj))
      case _ => super.mirror(e, f)
    }).asInstanceOf[Exp[A]] // why??
  }
}

trait ScalaGenSystemOps extends ScalaGenBase {
  val IR: SystemOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case pe@ParallelExecute(nJobs, f) =>
      stream.println("// generating parallel execute")
      stream.println(src"val $sym ={")
      //stream.println("\tval tasks: Seq[scala.concurrent.Future[" + m + "]] = for (i <- 0 until " + n + ") yield future {" ote(f) + "(i) }")
      stream.println("import scala.concurrent.ExecutionContext.Implicits.global")
      stream.println(src"val tasks = for (i <- 0 until $nJobs) yield scala.concurrent.future {$f(i) }")
      stream.println("scala.concurrent.Await.result(scala.concurrent.Future.sequence(tasks), scala.concurrent.duration.Duration.Inf).toArray")
      stream.println("}")
    case StringMatches(str, pattern) =>
      stream.println(src"val $sym = $str.matches($pattern)")
    case HashCode(obj) =>
      stream.println(src"val $sym = $obj.hashCode")
    case _ => super.emitNode(sym, rhs)
  }

}
