package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.lms.common._
import scala.lms.internal.ScalaNestedCodegen

trait ExtNumOps extends Base {
  def numeric_rand[A: Manifest](bound: Rep[A], id: Int): Rep[A]
}

trait ExtNumOpsExp extends ExtNumOps with BaseExp with EffectExp {

  case class NumericRand[A: Manifest](bound: Exp[A], id: Int) extends Def[A]

  //case class NumericRand(a: Exp[Double]) extends Def[Unit]

  def numeric_rand[A: Manifest](bound: Exp[A], id: Int): Exp[A] = {
    //NumericRand(bound, i)
    val x = fresh[A]
    val b = reifyEffects(bound)
    reflectEffect(NumericRand(bound, id), summarizeEffects(b).star)
  }

  override def mirror[A: Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case NumericRand(bound, id) => numeric_rand(f(bound), id).asInstanceOf[Exp[A]] // TODO: is this hack valid?
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
    case NumericRand(bound, _) =>
      bound.tp.asInstanceOf[Manifest[_]] match {
        case Manifest.Int => stream.println(src"val $sym = util.Random.nextInt($bound.asInstanceOf[Int])")
        case Manifest.Double => stream.println(src"val $sym = util.Random.nextDouble() * $bound.asInstanceOf[Double]")
        case _ => throw new Exception(s"random not implemented for ${bound.tp}")
      }
    case _ => super.emitNode(sym, rhs)
  }

}

