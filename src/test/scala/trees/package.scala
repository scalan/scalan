import org.scalacheck.Arbitrary._
import org.scalacheck.{Gen, Arbitrary}

/**
 * User: Alexander Slesarenko   
 * Date: 11/17/13
 */
package object trees extends RoseTrees with TreeGraphs {

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


  def genTree[A:Arbitrary]: Gen[RoseTree[A]] = for { n <- arbitrary[Node[A]] } yield RoseTree(n)

  implicit def arbTree[A](implicit a: Arbitrary[A]): Arbitrary[RoseTree[A]] = Arbitrary(genTree[A])

}

