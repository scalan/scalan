package scalan.collections

import scalan.Scalan
import scalan.primitives.Functions

trait SimpleMapTests {
  trait SimpleMapProg extends Maps with Functions { self: Scalan =>
    lazy val mapEmpty = fun {_:Rep[Int] =>
      MMap.empty[Int,Int]
    }

    lazy val mapPutContains = fun { p:Rep[(Int,Double)] =>
      val m = MMap.empty[Int,Double]
      val m1 = m.update(p._1, p._2)|m
      m1.contains(p._1)
    }

    lazy val mapAsSet = fun {in:Rep[Int] =>
      val m = MMap.empty[Int,Unit]
      val m1 = m.update(in,())|m
      m1.contains(in)
    }
  }
}
