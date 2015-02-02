package scalan.primitives

import scalan.{ScalanExp, ScalanSeq, Scalan}

trait Exceptions { self: Scalan =>
  def THROW(msg: Rep[String]): Rep[Unit]
}

trait ExceptionsSeq extends Exceptions { self: ScalanSeq =>
  def THROW(msg: Rep[String]): Rep[Unit] = throw new Exception(msg)
}

trait ExceptionsExp extends Exceptions { self: ScalanExp =>
  case class ThrowException(msg: Rep[String]) extends BaseDef[Unit] {
    def uniqueOpId = name(selfType)
    override def mirror(t: Transformer) = ThrowException(t(msg))
  }
  def THROW(msg: Rep[String]): Rep[Unit] = ThrowException(msg)    
}
