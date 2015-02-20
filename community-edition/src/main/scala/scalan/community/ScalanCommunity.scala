package scalan.community

import scalan.collection.CollectionsDsl
import scalan.linalgebra.{MatricesDslExp, MatricesDslSeq, MatricesDsl}
import scalan.{Scalan, ScalanCtxExp, ScalanCtxSeq}
import scalan.parrays._
import scalan.collections._

trait ScalanCommunity extends Scalan

trait ScalanCommunitySeq extends ScalanCtxSeq
  with ScalanCommunity

trait ScalanCommunityExp extends ScalanCtxExp
  with ScalanCommunity

trait ScalanCommunityDsl extends ScalanCommunity with CollectionsDsl with BitSets with PArraysDsl with MatricesDsl with MultiMapsDsl

trait ScalanCommunityDslSeq extends ScalanCommunityDsl with ScalanCommunitySeq with BitSetsSeq with PArraysDslSeq with MatricesDslSeq with MultiMapsDslSeq

trait ScalanCommunityDslExp extends ScalanCommunityDsl with ScalanCommunityExp with BitSetsExp with PArraysDslExp with MatricesDslExp with MultiMapsDslExp