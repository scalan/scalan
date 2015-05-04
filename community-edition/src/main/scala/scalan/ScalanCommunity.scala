package scalan

import scalan.collections._
import scalan.io.{ConsoleExp, ConsoleSeq, Console}
import scalan.linalgebra.{MatricesDslExp, MatricesDslSeq, MatricesDsl}

trait ScalanCommunity extends Scalan
  with Console

trait ScalanCommunitySeq extends ScalanCtxSeq
  with ScalanCommunity
  with ConsoleSeq

trait ScalanCommunityExp extends ScalanCtxExp
  with ScalanCommunity
  with ConsoleExp

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
