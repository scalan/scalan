package scalan.community

import scalan.linalgebra.{MatricesDslExp, MatricesDslSeq, MatricesDsl}
import scalan.{Scalan, ScalanCtxStaged, ScalanCtxSeq}
import scalan.arrays._

trait ScalanCommunity extends Scalan
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

trait ScalanCommunityDsl extends ScalanCommunity with PArraysDsl with MatricesDsl

trait ScalanCommunityDslSeq extends ScalanCommunityDsl with PArraysDslSeq with MatricesDslSeq

trait ScalanCommunityDslStaged extends ScalanCommunityDsl with PArraysDslExp with MatricesDslExp