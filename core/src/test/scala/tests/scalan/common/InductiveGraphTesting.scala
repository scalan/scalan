package tests.scalan.common

import scalan.common.InductiveGraphs

trait InductiveGraphTesting {
  val graphs: InductiveGraphs[Int]
  import graphs._

  def node(n: Int, ins: Seq[Int]) = Context(ins map (in => Edge((), in)), n, n, Seq())
}
