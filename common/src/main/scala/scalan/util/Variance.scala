package scalan.util

sealed trait Variance extends Product with Serializable
case object Invariant extends Variance
case object Covariant extends Variance
case object Contravariant extends Variance

