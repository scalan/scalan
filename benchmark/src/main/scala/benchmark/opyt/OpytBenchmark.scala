package benchmark
package opyt

import java.util.concurrent.TimeUnit

import benchmark.jni.NativeMethods
import org.openjdk.jmh.annotations._

/**
 * Created by zotov on 1/30/15.
 */

object OpytBenchmark {
  @State(Scope.Benchmark)
  class BenchmarkState {
    def sparseVectorData(arr: Array[Double]) = ((0.until(arr.length)).toArray, (arr, arr.length))

//    @volatile
//    var inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
//    @volatile
//    var inV = Array(2.0, 3.0)

    @volatile
    var inM = (0 until 10000).map({i => (0 until 10000).map({i => Math.random()}).filter({a => a > 0.9}).toArray}).toArray.map(sparseVectorData)
    @volatile
    var inV = (0 until 10000).map({i => Math.random()}).toArray
    @volatile
    var in = (inM, inV)

    @volatile
    var nativeMethods = new NativeMethods
  }
}

class OpytBenchmark {
  System.loadLibrary("sdmvm")
  System.loadLibrary("jniExtractor")

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def sdmvm( state: OpytBenchmark.BenchmarkState ): Array[Double] = {
    state.nativeMethods.sdmvm( state.in )
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def sdmvmScalanJNI( state: OpytBenchmark.BenchmarkState ): Array[Double] = {
    state.nativeMethods.sdmvmScalanJNI( state.in )
  }
}
