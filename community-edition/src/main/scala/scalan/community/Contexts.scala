package scalan.community

import scalan.{ScalanCtxStaged, ScalanCtxSeq, ScalanDsl}
import scalan.arrays._

trait ScalanCommunity extends ScalanDsl
  with ArrayOps
  with ArrayViews

trait ScalanCommunitySeq extends ScalanCtxSeq
  with ArrayOpsSeq
  with ArrayViewsSeq

trait ScalanCommunityStaged extends ScalanCtxStaged
  with ArrayOpsExp
  with ArrayViewsExp