package isos

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._

import org.scalatest.exceptions.TestFailedException


@RunWith(classOf[JUnitRunner])
class CheckIsosSuite extends FunSuite with Checkers {
  def checkBogus(p: Prop) {
    var ok = false
    try {
      check(p)
    } catch {
      case e: TestFailedException =>
        ok = true
    }
    assert(ok, "A bogus heap should NOT satisfy all properties. Try to find the bug!")
  }

  test("Isos properties.") {
    val props = new CheckIsos {}
    props.check
  }

  test("NArray validness") {
    val na = NArray(Array(1,2), Array(0,1), Array(1,2,3))
    val okLen = na.hasValidLengths
    assert(okLen, "valid lengths")
    val okOfs = na.hasValidOffsets
    assert(okOfs, "valid offsets")

    val na2 = NArray(Array(1,2), Array(0,1), Array(1,2,3,4))
    val ok = na2.hasValidLengths
    assert(!ok, "invalid lengths")

    val na3 = NArray(Array(1,2), Array(1,3), Array(1,2,3))
    val ok2 = na3.hasValidOffsets
    assert(!ok2, "invalid offsets")
  }

  test("TreeGraph 1") {
    val p = Ptr[Node[Int]]()
    val t = Tree(Node(p, List(Node(p, Nil, 2), Node(p, Nil, 3)), 1))
    val g = TreeGraph.fromTree(t, 0)
    assert(g.children.hasValidLengths, "lengths")
    assert(g.children.hasValidOffsets, "offsets")
  }
}
