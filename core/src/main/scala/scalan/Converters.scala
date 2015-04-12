package scalan

trait Converters extends Views { self: Scalan =>

  type Conv[T,R] = Rep[Converter[T,R]]
  trait Converter[T,R] extends Reifiable[Converter[T,R]] {
    implicit def eDom: Elem[T]
    implicit def eRange: Elem[R]
    def apply(x: Rep[T]): Rep[R]
//    override def toString = s"${eDom.name} --> ${eRange.name}"
//    override def equals(other: Any) = other match {
//      case c: Converter[_, _] => eDom == c.eDom && eRange == c.eRange
//      case _ => false
//    }
  }
  trait ConverterCompanion

  abstract class ConverterImpl[T,R]()(implicit val eDom: Elem[T], val eRange: Elem[R])
    extends Converter[T,R] {
    def apply(x: Rep[T]): Rep[R] = !!!  // dummy implementation to make scalan-meta happy
  }
  trait ConverterImplCompanion
}

trait ConvertersDsl extends impl.ConvertersAbs { self: Scalan =>
}

trait ConvertersDslSeq extends impl.ConvertersSeq { self: ScalanSeq =>
}

trait ConvertersDslExp extends impl.ConvertersExp { self: ScalanExp =>
}