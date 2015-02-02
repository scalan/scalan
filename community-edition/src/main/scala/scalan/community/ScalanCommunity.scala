package scalan.community

import scalan.linalgebra.{MatricesDslExp, MatricesDslSeq, MatricesDsl}
import scalan._
import scalan.parrays._
import scalan.collections._
import scalan.primitives.{AbstractStringsDsl, AbstractStringsDslExp, AbstractStringsDslSeq}

trait ScalanCommunity extends Scalan

trait ScalanCommunitySeq extends ScalanCtxSeq
  with ScalanCommunity

trait ScalanCommunityExp extends ScalanCtxExp
  with ScalanCommunity

trait ScalanCommunityDsl extends ScalanCommunity with PArraysDsl with MatricesDsl with MultiMapsDsl with JNIExtractorOps with AbstractStringsDsl

trait ScalanCommunityDslSeq extends ScalanCommunityDsl with ScalanCommunitySeq with PArraysDslSeq with MatricesDslSeq with MultiMapsDslSeq with JNIExtractorOpsSeq with AbstractStringsDslSeq

trait ScalanCommunityDslExp extends ScalanCommunityDsl with ScalanCommunityExp with PArraysDslExp with MatricesDslExp with MultiMapsDslExp with JNIExtractorOpsExp with AbstractStringsDslExp
