package benchmark
package jni

class NativeMethods {
  type TT = Array[Int]
//  type TT = ( Array[( Array[Int], (Array[Double],Int))], Array[Double])
  @native def extractAndPack( in: TT ): TT

  @native def ddmvm( in: (Array[Array[Double]], Array[Double]) ): Array[Double]
  @native def dsmvm( p: (Array[Array[Double]], (Array[Int], (Array[Double], Int))) ): Array[Double]
  @native def sdmvm( p: (Array[(Array[Int],(Array[Double], Int))], Array[Double]) ): Array[Double]
  @native def ssmvm( p: (Array[(Array[Int], (Array[Double], Int))], (Array[Int], (Array[Double], Int))) ): Array[Double]
  @native def fdmvm( p: ((Array[Double], Int), Array[Double]) ): Array[Double]
  @native def fsmvm( p: ((Array[Double], Int), (Array[Int], (Array[Double], Int))) ): Array[Double]

  @native def sdmvmScalanJNI( p: (Array[(Array[Int],(Array[Double], Int))], Array[Double]) ): Array[Double]
}
