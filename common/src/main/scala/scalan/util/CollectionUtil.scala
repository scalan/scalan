package scalan.util

object CollectionUtil {

  def foldRight[A,B](xs: Seq[A])(proj: A => B)(f: (A,B) => B): B =
    xs.foldRight[B](null.asInstanceOf[B]) { case (a, b) =>
      b match {
        case null => proj(a)
        case _ => f(a, b)
      }
    }

}
