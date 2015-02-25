package scalan

import scalan.collections._
import scalan.linalgebra.{MatricesDsl, MatricesDslExp, MatricesDslSeq}
import scalan.parrays._

trait ScalanCommunity extends Scalan

trait ScalanCommunitySeq extends ScalanCtxSeq
  with ScalanCommunity

trait ScalanCommunityExp extends ScalanCtxExp
  with ScalanCommunity

trait ScalanCommunityDsl extends ScalanCommunity
  with PArraysDsl
  with MatricesDsl
  with MultiMapsDsl
  with HashSetsDsl
  with SeqsDsl

trait ScalanCommunityDslSeq extends ScalanCommunitySeq with ScalanCommunityDsl
  with PArraysDslSeq
  with MatricesDslSeq
  with MultiMapsDslSeq
  with HashSetsDslSeq
  with SeqsDslSeq

trait ScalanCommunityDslExp extends ScalanCommunityExp with ScalanCommunityDsl
  with PArraysDslExp
  with MatricesDslExp
  with MultiMapsDslExp
  with HashSetsDslExp
  with SeqsDslExp
