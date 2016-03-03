package scalan.it

import scalan.{ScalanStd, Scalan, BaseTests, TestContexts}

/**
 * Base class for integration testing
 *
 * @param mkProgStd By-name parameter to construct the sequential version of [[Prog]]. Called only once. `???` may be passed
 *                  if the class doesn't use [[ItTestsUtil.compareOutputWithStd]].
 * @tparam Prog Program type
 */
abstract class BaseItTests[Prog <: Scalan](mkProgStd: => Prog with ScalanStd) extends BaseTests with ItTestsUtil[Prog] {
  lazy val progStd = mkProgStd
}

abstract class BaseCtxItTests[Prog <: Scalan](mkProgStd: => Prog with ScalanStd) extends BaseItTests[Prog](mkProgStd) with TestContexts