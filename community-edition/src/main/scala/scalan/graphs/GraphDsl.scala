/**
 * Created by afilippov on 2/16/15.
 */

package scalan.graphs

import scalan.collection.{CollectionsDsl, CollectionsDslExp, CollectionsDslSeq}
import scalan.{ScalanCommunityDsl, ScalanCommunityDslExp, ScalanCommunityDslSeq}
//import scalan.{ScalanEnterpriseDslSeq, ScalanEnterpriseDslExp}
//import scalan.collections.{BitSetsExp, BitSetsSeq, BitSets}

trait GraphsDsl extends impl.GraphsAbs with impl.EdgesAbs with impl.VerticesAbs /*with BitSets*/ with CollectionsDsl with ScalanCommunityDsl {

}

trait GraphsDslSeq
  extends GraphsDsl
  with impl.GraphsSeq
  with impl.EdgesSeq
  with impl.VerticesSeq
//  with BitSetsSeq
  with CollectionsDslSeq
  with ScalanCommunityDslSeq

trait GraphsDslExp
  extends GraphsDsl
  with impl.GraphsExp
  with impl.EdgesExp
  with impl.VerticesExp
//  with BitSetsExp
  with CollectionsDslExp
  with ScalanCommunityDslExp