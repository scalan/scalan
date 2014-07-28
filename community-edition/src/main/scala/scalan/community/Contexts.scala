package scalan.community

import scalan.{ScalanCtxStaged, ScalanCtxSeq, ScalanDsl}
import scalan.arrays._

trait ScalanCommunity extends ScalanDsl
  with ArrayOps
  with ArrayViews

trait ScalanCommunitySeq extends ScalanCtxSeq
  with ScalanCommunity
  with ArrayOpsSeq
  with ArrayViewsSeq

trait ScalanCommunityStaged extends ScalanCtxStaged
  with ScalanCommunity
  with ArrayOpsExp
  with ArrayViewsExp