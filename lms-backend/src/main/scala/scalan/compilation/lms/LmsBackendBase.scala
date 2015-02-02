package scalan.compilation.lms

import scalan.compilation.lms.common.{SystemOpsExp, VectorOpsExp}

/**
 * Created by zotov on 2/2/15.
 */
trait CoreLmsBackendBase extends LmsBackend with LmsBackendFacade
trait CommunityLmsBackendBase extends CoreLmsBackendBase with VectorOpsExp with SystemOpsExp
