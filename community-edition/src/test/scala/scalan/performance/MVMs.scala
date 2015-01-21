package scalan.performance

/**
 * Created by zotov on 12/8/14.
 */
class MVMs {
//  @native def extractorTest( p: (Array[(Array[Int],(Array[Double], Int))], Array[Double]) ): Unit
//  @native def extractorTest( p: (Array[Double], Double) ): Array[Double]
  @native def extractorTest( p: (Array[Array[Double]], Array[Double]) ): Array[Double]
  @native def sdmvm( p: (Array[(Array[Int],(Array[Double], Int))], Array[Double]) ): Array[Double]
//  @native def extractorTest( on: Array[Array[Double]]): Unit

}

object MVMs {
//  System.loadLibrary("sdmvm")
//  System.loadLibrary("jniExtractor")

  def apply(): MVMs = new MVMs

  def sparseVectorData(arr: Array[Double]) = ((0.until(arr.length)).toArray, (arr, arr.length))

  def main( args: Array[String] ): Unit = {
//    val in = ( Array((Array(1,2,3),(Array(11.0,22.0,3.0),3))
//                    ,(Array(1,2,3),(Array(1.0,2.0,3.0),3))
//                    ,(Array(1,2,3),(Array(1.0,2.0,3.0),3)))
//             , Array(1.0,222.0,3.0))
//
//    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
//    val inV = Array(2.0, 3.0)
//    val in = (inM, inV)
//    val out = Array(5.0, 3.0)

//    val inM = (0 until 10000).map({i => (0 until 10000).map({i => Math.random()}).filter({a => a > 0.9}).toArray}).toArray.map(sparseVectorData)
//    val inV = (0 until 10000).map({i => Math.random()}).toArray
//    val in = (inM, inV)

//      val in = (Array(3.142), 0.618)
//    val res = new MVMs().extractorTest(in)

//    println( res.mkString("[", ",", "]") )
  }
}
