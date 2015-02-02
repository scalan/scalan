package scalan.community

import scalan.linalgebra.{MatricesDslExp, MatricesDslSeq, MatricesDsl}
import scalan._
import scalan.parrays._

trait ScalanCommunity extends Scalan

trait ScalanCommunitySeq extends ScalanCtxSeq
  with ScalanCommunity

trait ScalanCommunityExp extends ScalanCtxExp
  with ScalanCommunity

trait ScalanCommunityDsl extends ScalanCommunity with PArraysDsl with MatricesDsl

trait ScalanCommunityDslSeq extends ScalanCommunityDsl with PArraysDslSeq with MatricesDslSeq

trait ScalanCommunityDslExp extends ScalanCommunityDsl with PArraysDslExp with MatricesDslExp