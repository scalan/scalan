package scalan.frontend

trait R {
  type Rep[T]
}

trait WrapRep extends R {

  trait Rep[T] { def x: T }

  object Rep {
    implicit def apply[A](a: A): Rep[A] =
      new Rep[A] { val x = a }
  }
}

trait IdRep extends R{
  type Rep[+T] = T
  object Rep {
    implicit def apply[A](a: A): Rep[A] =
      a.asInstanceOf[Rep[A]]
  }
}
