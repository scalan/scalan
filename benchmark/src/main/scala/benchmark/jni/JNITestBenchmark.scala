package benchmark
package jni

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

object JNITestBenchmark {
  @State(Scope.Benchmark)
  @volatile
  class ExtractAndPackState {
//    val in: NativeMethods#TT = ( Array((Array(1,2,3),(Array(11.0,22.0,3.0),3))
//      ,(Array(1,2,3),(Array(1.0,2.0,3.0),3))
//      ,(Array(1,2,3),(Array(1.0,2.0,3.0),3)))
//      , Array(1.0,222.0,3.0))
    val in: NativeMethods#TT = Array(1,2)
    var nativeMethods = new NativeMethods
    var res: NativeMethods#TT = null;

    @TearDown
    def check(): Unit = {
//      require(res == in, s"error!")
      require(res.deep == in.deep, s"error!")
    }
  }
}

class JNITestBenchmark {
  System.loadLibrary("jniTest")

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def extractAndPack( state: JNITestBenchmark.ExtractAndPackState ): NativeMethods#TT = {
    val res = state.nativeMethods.extractAndPack( state.in )
    state.res = res
    res
  }
}
