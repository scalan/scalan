package scalan.compilation.lms

import scala.reflect.AnyValManifest

trait ManifestUtil {
  implicit class ManifestOps[T](manifest: Manifest[T]) {
    /**
     * Non-deprecated version of <:<
     */
    def <::<(other: Manifest[_]) = {
      @deprecated("", "") val isSubtype = manifest <:< other

      isSubtype
    }

    def isPrimitive = manifest.isInstanceOf[AnyValManifest[_]]

    def isClass = <::<(Manifest.AnyRef)
  }
}

object ManifestUtil extends ManifestUtil