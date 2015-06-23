package scalan.compilation.lms.uni

/**
 * Created by adel on 6/1/15.
 */
class NativeMethodsConfig(val rootIsNative: Boolean = true, nativeMethods: List[String] = List()) {

  def checkMethod[Exp](method: Exp): Boolean = false
}

