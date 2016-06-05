package scalan.compilation

import scalan.ScalanDslExp
import scalan.compilation.KernelTypes._

trait Kernel[+ScalanCake <: ScalanDslExp, A, B] extends (A => B) {
  val scalan: ScalanCake
  import scalan._
  lazy val eA = kernelFunc.elem.eDom
  lazy val eB = kernelFunc.elem.eRange
  def kernelFunc: this.scalan.Exp[A => B]
}

trait KernelStore[+ScalanCake <: ScalanDslExp] {
  val scalan: ScalanCake
  import scalan._
  def createKernel[A,B](kernelId: String, f: Exp[A => B]): Kernel[ScalanCake, A, B]
}

object KernelStore {
  def open[SCake <: ScalanDslExp]
          (scalan: SCake, kernelType: KernelType): KernelStore[SCake] = {
               ???
  }
}
