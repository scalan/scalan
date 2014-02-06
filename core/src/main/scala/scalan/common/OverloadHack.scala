package scalan.common

// hack to appease erasure

trait OverloadHack {
  class Overloaded1 { override def toString = "O1"}
  class Overloaded2 { override def toString = "O2"}
  class Overloaded3 { override def toString = "O3"}
  class Overloaded4 { override def toString = "O4"}
  class Overloaded5 { override def toString = "O5"}
  implicit val overloaded1 = new Overloaded1
  implicit val overloaded2 = new Overloaded2
  implicit val overloaded3 = new Overloaded3
  implicit val overloaded4 = new Overloaded4
  implicit val overloaded5 = new Overloaded5
}