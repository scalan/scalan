package benchmark
package jni

/**
 * Created by zotov on 1/30/15.
 */
class NativeMethods {
  System.loadLibrary("jniddmvm")
  type TT = Array[Int]
//  type TT = ( Array[( Array[Int], (Array[Double],Int))], Array[Double])
  @native def extractAndPack( in: TT ): TT
  @native def MSTadjlist( in: (Array[Int], (Array[Double], (Array[Int], Array[Int]))) ): Array[Int]
  @native def MSTadjmatrix( in: (Array[Double], Int) ): Array[Int]

  @native def ddmvm( in: (Array[Array[Double]], Array[Double]) ): Array[Double]
  @native def sdmvm( p: (Array[(Array[Int],(Array[Double], Int))], Array[Double]) ): Array[Double]
  @native def sdmvmScalanJNI( p: (Array[(Array[Int],(Array[Double], Int))], Array[Double]) ): Array[Double]
}
