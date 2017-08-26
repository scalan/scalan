package scalan.meta

object PrintExtensions {
  implicit class IterableExtensions[A](val it: Iterable[A]) extends AnyVal
  {
    def opt(show: Iterable[A] => String = _.mkString, default: String = ""): String =
      if (it.isEmpty) default else show(it)

    def rep(show: A => String = _.toString, sep: String = ", "): String = it.map(show).mkString(sep)
    def asTypeParams(show: A => String = _.toString) =
      if (it.nonEmpty) it.map(show).mkString("[", ", ", "]") else ""

    def optList(start: String, end: String, sep: String = ", ", show: A => String = _.toString) =
      if (it.nonEmpty) it.map(show).mkString(start, sep, end) else ""

    def enumTypes(show: Int => String) = (1 to it.size).map(show)
  }

  implicit class OptionExtensions[A](val opt: Option[A]) extends AnyVal {
    def opt(show: A => String = _.toString, default: String = ""): String = opt match {
      case None => default
      case Some(a) => show(a)
    }
    def ifDefined(value: String): String = if (opt.isDefined) value else ""
  }

  implicit class BooleanExtensions(val opt: Boolean) extends AnyVal {
    def opt(show: => String, default: => String = ""): String = if(opt) show else default
  }

  implicit class StringExtensions(val str: String) extends AnyVal {
    def stripAndTrim = str.stripMargin.stripPrefix("\n").stripPrefix("\r\n").stripLineEnd
    def opt(show: String => String = _.toString, default: String = ""): String =
      (!str.isEmpty).opt(show(str), default)
  }

  def join(xs: Any*) = xs.map {
    case x: Iterable[_] => x.rep()
    case x => x.toString
  }.filter(_.nonEmpty).rep()
}
