package scalan.common

object Common extends Zeros
  with Semigroups
  with Options {

  implicit class AnyExtensions[A](x: A) {
    def |>[B] (f: A => B): B = f(x)
  }
  //implicit def addAnyExtensions[A](x: A) = new AnyExtensions(x)

}
