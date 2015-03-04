package benchmark.common

import java.nio.file.{Files, Path, Paths}

import scodec.{Attempt, Codec, DecodeResult, Decoder}
import scodec.bits.BitVector
import scodec.codecs._

import scala.collection.mutable

object GraphUtilsSSCA {

  case class SSCA( n: Int = 0,
                   m: Int = 0,
                   dir: Boolean = false,
                   rowInd: Vector[Int] = Vector.empty,
                   endV: Vector[Int] = Vector.empty,
                   weights: Vector[Double] = Vector.empty)

  def apply(inSSCA: Path) = {
    val byteArray = Files.readAllBytes(inSSCA)


    val c = (uint32L :: int64L :: byte :: byte).as[(Long,Long,Byte,Byte)] map {r => val dir = if(r._3 > 0) true else false; SSCA(r._1.toInt, r._2.toInt, dir)}

    val c1 = c flatMap {res0 => new Decoder[SSCA] {
      def decode(b: BitVector): Attempt[DecodeResult[SSCA]] = {
        val sz = Codec.encode(res0.n + 1)(int32L).require
        val b1 = sz ++ b
        val vr = vectorOfN(int32L, int64L).decode( b1 ).require;
        val v = vr.value.map{a => a.toInt}

        Attempt.successful(DecodeResult.apply(res0.copy(rowInd = v), vr.remainder))
      }
    }
    }

    val c2 = c1 flatMap {res1 => new Decoder[SSCA] {
      def decode(b: BitVector): Attempt[DecodeResult[SSCA]] = {
        val sz = Codec.encode(res1.m)(int32L).require
        val b1 = sz ++ b
        val vr = vectorOfN(int32L, uint32L).decode( b1 ).require;
        val v = vr.value.map{a => a.toInt}

        Attempt.successful(DecodeResult.apply(res1.copy(endV = v), vr.remainder))
      }
    }
    }

    val c3 = c2 flatMap {res2 => new Decoder[SSCA] {
      def decode(b: BitVector): Attempt[DecodeResult[SSCA]] = {
        val sz = Codec.encode(res2.m)(int32L).require
        val b1 = sz ++ b
        val vr = vectorOfN(int32L, doubleL).decode( b1 ).require;
        val v = vr.value

        Attempt.successful(DecodeResult.apply(res2.copy(weights = v), vr.remainder))
      }
    }
    }

    c3.decode(BitVector(byteArray)).require.value
  }

  def toDenseWeights(g: SSCA)(implicit defWeight: Double = 0.0): Vector[Double] = {
    val resA = Array.fill(g.n*g.n)(defWeight)

    for(i <- 0 until g.n) {
      for (ij <- g.rowInd(i) until g.rowInd(i + 1) ) {
        val j = g.endV(ij.toInt)
        resA((g.n*i + j).toInt) = g.weights(ij.toInt)
      }
    }

    resA.toVector
  }


  def calcSegLens(offs: Array[Int]): Array[Int] = {
    val bu = mutable.ArrayBuilder.make[Int]
    for( i <- 0 until offs.length - 1) {
      bu += offs(i+1) - offs(i)
    }
    bu.result()
  }

  def main(args: Array[String]): Unit = {
    val g = GraphUtilsSSCA(Paths.get("ssca2-3"))
    println( g )
    println(toDenseWeights(g))
    println(calcSegLens(g.rowInd.toArray).mkString(","))
  }

}
