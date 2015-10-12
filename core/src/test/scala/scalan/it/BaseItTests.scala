package scalan.it

import scalan.{ScalanSeq, Scalan, BaseTests, TestContexts}

/**
 * Base class for integration testing
 * @param mkProgSeq By-name parameter to construct the sequential version of [[Prog]]. Called only once. `???` may be passed
 *                  if the class doesn't use `compareOutputWithSequential`.
 * @tparam Prog Program type
 */
abstract class BaseItTests[Prog <: Scalan](mkProgSeq: => Prog with ScalanSeq) extends BaseTests with ItTestsUtil[Prog] {
  lazy val progSeq = mkProgSeq
}

abstract class BaseCtxItTests[Prog <: Scalan](mkProgSeq: => Prog with ScalanSeq) extends BaseItTests[Prog](mkProgSeq) with TestContexts