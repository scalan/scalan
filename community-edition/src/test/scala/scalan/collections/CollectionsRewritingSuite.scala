package scalan.collections

import scalan.{ScalanCommunityDslExp, RewriteRuleSuite, TestCtx}

class CollectionsRewritingSuite extends RewriteRuleSuite[(Array[Int],Array[Int]), Array[(Int,Int)], Array[(Int,Int)], Array[(Int,Int)]] {
  def getCtx = new TestCtx[(Array[Int],Array[Int]), Array[(Int,Int)], Array[(Int,Int)], Array[(Int,Int)]] with CollectionExamples with ScalanCommunityDslExp {
    val eA = element[LemmaArg]
    val eC = element[TestArg]
    val eD = element[TestRes]
    
    lazy val test = (xs: Arr[(Int,Int)]) => Collection.fromArray(xs).arr

    lazy val testLemma = postulate((xs: Arr[Int], ys: Arr[Int]) => (Collection.fromArray(xs) zip Collection.fromArray(ys)).arr <=> (xs zip ys))
    
    lazy val expected = {(ps: Arr[(Int,Int)]) => 
      val as = ps.map {_._1}
      val bs = ps.map {_._2}
      as zip bs 
    }
  }
}
