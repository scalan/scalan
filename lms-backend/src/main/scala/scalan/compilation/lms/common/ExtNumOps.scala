package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.lms.common._
import scala.lms.internal.ScalaNestedCodegen
import scalan.compilation.lms.cxx.sharedptr.CxxShptrCodegen

trait ExtNumOps extends Base {

  def numeric_Random(bound: Rep[Double]): Rep[Double]
}


trait ExtNumOpsExp extends ExtNumOps with BaseExp with EffectExp {

  case class NumericRand(bound: Exp[Double]) extends Def[Double] {
    val m = manifest[Double]
  }

  //case class NumericRand(a: Exp[Double]) extends Def[Unit]

  def numeric_Random(bound: Rep[Double]): Rep[Double] =
  {
    //NumericRand(bound, i)
    val x = fresh[Double]
    val b = reifyEffects(bound)
    reflectEffect(NumericRand(bound), summarizeEffects(b).star)
  }

  override def mirror[A: Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case NumericRand(bound) => numeric_Random(f(bound)).asInstanceOf[Exp[A]] // TODO: is this hack valid?
    case _ => super.mirror(e,f)
  }

//  override def syms(e: Any): List[Sym[Any]] = e match {
//    case NumericRand(bound, i) => syms(bound)
//    case _ => super.syms(e)
//  }
//
//  override def boundSyms(e: Any): List[Sym[Any]] = e match {
//    case NumericRand(bound, i) => bound
//    case _ => super.boundSyms(e)
//  }
//
//  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
//    case NumericRand(bound, i) => freqNormal(bound)
//    case _ => super.symsFreq(e)
//  }
}

trait ScalaGenExtNumOps extends ScalaGenBase {
  val IR: ExtNumOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ds @ NumericRand(n) =>
      stream.println(src"val $sym = util.Random.nextDouble() * $n.asInstanceOf[Double]")
    case _ => super.emitNode(sym, rhs)
  }

}

trait CxxShptrGenExtNumOps extends CxxShptrCodegen {
  val IR: ExtNumOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }

}
