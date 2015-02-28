package scalan

import scalan.collection._
import scalan.collections._
import scalan.linalgebra.{MatricesDsl, MatricesDslExp, MatricesDslSeq}
import scalan.parrays._

trait ScalanCommunity extends Scalan

trait ScalanCommunitySeq extends ScalanCtxSeq
  with ScalanCommunity

trait ScalanCommunityExp extends ScalanCtxExp
  with ScalanCommunity

trait ScalanCommunityDsl extends ScalanCommunity
  with CollectionsDsl
  with BitSets
  with PArraysDsl
  with MatricesDsl
  with MultiMapsDsl
  with HashSetsDsl
  with SeqsDsl

trait ScalanCommunityDslSeq extends ScalanCommunitySeq with ScalanCommunityDsl
  with BitSetsSeq
  with PArraysDslSeq
  with MatricesDslSeq
  with MultiMapsDslSeq
  with HashSetsDslSeq
  with SeqsDslSeq

trait ScalanCommunityDslExp extends ScalanCommunityExp with ScalanCommunityDsl
  with BitSetsExp
  with PArraysDslExp
  with MatricesDslExp
  with MultiMapsDslExp
  with HashSetsDslExp
  with SeqsDslExp
