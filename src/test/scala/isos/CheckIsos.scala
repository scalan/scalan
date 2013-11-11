package isos

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class CheckIsos extends Properties("Isos") {

  def genChildren[A:Arbitrary]: Gen[List[Node[A]]] = Gen.frequency(
    (2, List()),
    (1, (for { n <- genNode[A]; cs <- genChildren[A] } yield n :: cs))
  )

  def genNode[A:Arbitrary]: Gen[Node[A]] =
    for {
      a <- arbitrary[A]
      cs <- genChildren[A]
      n = Node(Ptr[Node[A]](), cs, a)
    } yield {
      for { c <- cs } c.parent.set(n)
      n
    }

  implicit def arbNode[A](implicit a: Arbitrary[A]): Arbitrary[Node[A]] = Arbitrary(genNode[A])


  def genTree[A:Arbitrary]: Gen[Tree[A]] = for { n <- arbitrary[Node[A]] } yield Tree(n)

  implicit def arbTree[A](implicit a: Arbitrary[A]): Arbitrary[Tree[A]] = Arbitrary(genTree[A])

  val arbType = Arbitrary(choose(0,3))

  property("p1") = forAll(genTree[Int](arbType)) { t: Tree[Int] =>
    t.size > 0
  }

  def validParents(n: Node[Int]): Boolean = {
    if (n.parent.get == null) true
    else n.children.forall { c =>
      c.parent.get == n && validParents(c)
    }
  }

  property("p2") = forAll(genNode[Int](arbType)) { n: Node[Int] =>
    collect(n.size) {
      validParents(n)
    }
  }

  def validParents(g: TreeGraph[Int], node: Int): Boolean = {
    import g._
    import children._

    if (parents(node) == -1) true
    else {
      val ofs = offsets(node)
      val len = lens(node)
      val children = for {
        i <- ofs until (ofs + len)
        child = values(i)
      } yield child

      children forall { c =>  parents(c) == node && validParents(g, c) }
    }
  }

  property("p3") = forAll(genNode[Int](arbType)) { n: Node[Int] =>
    val g = TreeGraph.fromTree(Tree(n), 0)
    validParents(g, 0)
  }

  property("children") = forAll(genNode[Int](arbType)) { n: Node[Int] =>
    val g = TreeGraph.fromTree(Tree(n), 0)
    ("has valid length " + g |: g.children.hasValidLengths) &&
    ("has valid offsets" + g |: g.children.hasValidOffsets)
  }

}
