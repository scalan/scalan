package scalan.compilation.lms

import scala.reflect.AnyValManifest

trait ManifestUtil {
  implicit class ManifestOps[T](manifest: Manifest[T]) {
    /**
     * Non-deprecated version of <:<
     */
    def <::<(other: Manifest[_]) = {
      ManifestUtil.SubTyper.<:<(manifest, other)
    }

    def isPrimitive = manifest.isInstanceOf[AnyValManifest[_]]

    def isClass = <::<(Manifest.AnyRef)
  }
}

object ManifestUtil extends ManifestUtil {
  @deprecated("trick to avoid deprecation warnings", "") private class SubTyper {
    @inline
    def <:<(m1: Manifest[_], m2: Manifest[_]) = m1 <:< m2
  }
  private object SubTyper extends SubTyper
}