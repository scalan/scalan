package scalan.it

import scalan.{BaseTests, TestContexts, ScalanExp}

/**
 * Base class for integration testing
 *
 * @param mkProgStd By-name parameter to construct the sequential version of [[Prog]]. Called only once. `???` may be passed
 *                  if the class doesn't use [[ItTestsUtil.compareOutputWithStd]].
 * @tparam Prog Program type
 */
abstract class BaseItTests[Prog <: ScalanExp](mkProgStd: => Prog) extends BaseTests with ItTestsUtil[Prog] {
  lazy val progStd = mkProgStd
}

abstract class BaseCtxItTests[Prog <: ScalanExp](mkProgStd: => Prog) extends BaseItTests[Prog](mkProgStd) with TestContexts