package scalan.codegen.emit

trait Printer[T] {
  def print(v: T)(implicit f: Formatter): Formatter
}
object Printer {
  implicit object IntPrinter extends Printer[Int] {
    def print(v: Int)(implicit f: Formatter) = f << v.toString
  }
  implicit object StringPrinter extends Printer[String] {
    def print(v: String)(implicit f: Formatter) = f << v
  }
  implicit def OptionPrinter[A:Printer] = new Printer[Option[A]] {
    def print(v: Option[A])(implicit f: Formatter) = f << v
  }
}

class Formatter {
  var tabSize = 4
  val buffer: StringBuilder = new StringBuilder()
  var spaces: String = ""
  var _indentLevel: Int = 0

  def indentLevel: Int = _indentLevel
  def indentLevel_=(level: Int) = {
    _indentLevel = level
    val size = level * tabSize
    val buf = new StringBuffer(size)
    (1 to size) foreach{ _ => buf.append(" ") }
    spaces = buf.toString
  }

  def << (msg: String): this.type = {
    val indented = msg.replaceAll("\n", "\n"+ spaces)
    buffer.append(indented)
    this
  }
  def << (block: => Unit): this.type = {
    block
    this
  }
  def <<[T](v: Option[T]): this.type = {
    v match {
      case Some(s) =>
        val indented = s.toString.replaceAll("\n", "\n"+ spaces)
        buffer.append(indented)
      case None =>
    }
    this
  }

  def <<[T:Printer](xs: List[T], sep: String = ","): this.type = {
    val p = implicitly[Printer[T]]
    xs match {
      case x :: y :: xs => p.print(x)(this) << sep << (y :: xs)
      case x :: Nil => p.print(x)(this)
      case Nil =>
    }
    this
  }

  def -->(block: Formatter => Unit) : Formatter = {
    indentLevel += 1
    block(this)
    indentLevel -= 1
    this
  }

  def |->(block: Formatter => Unit) : Formatter = {
    indentLevel += 1
    this << "\n"
    block(this)
    indentLevel -= 1
    this
  }

  override def toString = buffer.toString
}
