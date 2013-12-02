package scalan.rx

import scalan._
import scalan.common.Common
import scala.language.implicitConversions

trait Trees extends ScalanDsl {

  type T[A] = Rep[Tree[A]]
  trait Tree[A] {
    implicit def eA: Elem[A]
    def value: Rep[A]
    def left: Rep[Tree[A]]
    def right: Rep[Tree[A]]
  }

  class TreeImpl[A]
      (val value: Rep[A], val left: Rep[Tree[A]], val right: Rep[Tree[A]])(implicit val eA: Elem[A]) extends Tree[A] {
  }

}








