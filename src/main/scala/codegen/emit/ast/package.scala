package scalan.codegen.emit

package object ast {
  /** `???` can be used for marking methods that remain to be implemented.
   *  @throws  An `Error`
   */
  def ??? : Nothing = throw new Error("an implementation is missing")
  def !!!(msg: String, obj:Any) : Nothing = throw new Error("%s: %s".format(msg, obj))

  type ??? = Nothing
  type *** = Any
}
