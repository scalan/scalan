package scalan.compilation.lms.collections

import scalan.compilation.lms.ScalaCoreLmsBackend
import scalan.compilation.lms.scalac.{ScalaCoreCodegen, LmsCompilerScala}
import scalan.compilation.lms.cxx.sharedptr.CxxCoreCodegen
import scalan.compilation.lms.scalac.ScalaCoreCodegen
import scalan.compilation.lms.uni.{LmsBackendUni, LmsCompilerUni}
import scalan.{JNIExtractorOpsExp, ScalanDslExp}
import scalan.compilation.lms.ScalaCoreLmsBackend
import scalan.compilation.lms.uni._
import scalan.compilation.lms.scalac.{ScalaCoreCodegen, LmsCompilerScala}
import scalan.collections.CollectionsDslExp
import scalan.arrays.ArrayOpsExp

class CollectionsLmsBackendUni extends LmsBackendUni

class CollectionsLmsCompilerUni[+ScalanCake <: CollectionsDslExp with JNIExtractorOpsExp](_scalan: ScalanCake)
  extends LmsCompilerUni[ScalanCake](_scalan) {
  override val lms = new CollectionsLmsBackendUni
}

class ScalaCollectionsLmsBackend extends ScalaCoreLmsBackend

class CollectionsLmsCompilerScala[+ScalanCake <: CollectionsDslExp](_scalan: ScalanCake)
  extends LmsCompilerScala(_scalan) {
  override val lms = new ScalaCollectionsLmsBackend
}
