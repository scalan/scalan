package scalan

import scalan.collections._
import scalan.linalgebra.{MatricesDslExp, MatricesDslSeq, MatricesDsl}

trait ScalanCommunity extends Scalan

trait ScalanCommunitySeq extends ScalanCtxSeq
  with ScalanCommunity

trait ScalanCommunityExp extends ScalanCtxExp
  with ScalanCommunity

trait ScalanCommunityDsl extends ScalanCommunity
  with CollectionsDsl
  with BitSetsDsl
  with MatricesDsl
  with MultiMapsDsl
  with HashSetsDsl
  with SeqsDsl

trait ScalanCommunityDslSeq extends ScalanCommunitySeq with ScalanCommunityDsl
  with CollectionsDslSeq
  with BitSetsDslSeq
  with MatricesDslSeq
  with MultiMapsDslSeq
  with HashSetsDslSeq
  with SeqsDslSeq

trait ScalanCommunityDslExp extends ScalanCommunityExp with ScalanCommunityDsl
  with CollectionsDslExp
  with BitSetsDslExp
  with MatricesDslExp
  with MultiMapsDslExp
  with HashSetsDslExp
  with SeqsDslExp


trait ScalanCommunityScalaCompiler extends ScalanCommunityDslExp
  with CollectionsScalaCompiler
