package scalan.meta.scalanizer

import scala.annotation.StaticAnnotation
import scalan.meta.ScalanAst.KernelType

class HotSpot(kernel: KernelType = KernelType.Scala) extends StaticAnnotation
