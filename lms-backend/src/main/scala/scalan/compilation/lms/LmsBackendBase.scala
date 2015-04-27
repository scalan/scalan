package scalan.compilation.lms

import scalan.compilation.lms.common.{ExtNumOpsExp, SystemOpsExp, VectorOpsExp}

trait CoreLmsBackendBase extends LmsBackend with LmsBackendFacade

trait CommunityLmsBackendBase extends CoreLmsBackendBase with VectorOpsExp with ExtNumOpsExp with SystemOpsExp
