package scalan.primitives

import scalan.{ScalanExp, Scalan}

trait Exceptions { self: Scalan =>
  def THROW(msg: Rep[String]): Rep[Unit]
}

trait ExceptionsExp extends Exceptions { self: ScalanExp =>
  case class ThrowException(msg: Rep[String]) extends BaseDef[Unit]
  def THROW(msg: Rep[String]): Rep[Unit] = ThrowException(msg)    
}
