package scalan.it

import scalan.{BaseTests, TestContexts}

abstract class BaseItTests extends BaseTests with ItTestsUtil

abstract class BaseCtxItTests extends BaseItTests with TestContexts