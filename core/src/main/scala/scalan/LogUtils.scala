package scalan

// TODO: add logging, etc.
object LogUtils {
  val verbosity = Base.config.getInt("verbosity")

  def __ = throw new RuntimeException("unsupported embedded dsl operation")

  def printdbg(x: =>Any) { if (verbosity >= 2) System.err.println(x) }
  def printlog(x: =>Any) { if (verbosity >= 1) System.err.println(x) }
  def printerr(x: =>Any) { System.err.println(x); hadErrors = true }

  var hadErrors = false
}