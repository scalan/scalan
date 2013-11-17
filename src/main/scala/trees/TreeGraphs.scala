/**
 * User: Alexander Slesarenko
 * Date: 11/17/13
 */
package trees

trait TreeGraphs  {

  case class NArray[A](lens: Array[Int], offsets: Array[Int], values: Array[A]) {
    def hasValidLengths = lens.sum == values.length
    def hasValidOffsets = (lens.scan(0)(_ + _).take(lens.length), offsets).zipped.forall { _ == _ }
    override def toString = "NArray(%s,%s,%s)".format(lens.mkString("(",", ",")"), offsets.mkString("(",", ",")"), values.mkString("(",", ",")"))
  }

  case class TreeGraph[A](parents: Array[Int], children: NArray[Int], nodeProps: Array[A]) {
    val root: Int = 0
    override def toString = "TreeGraph(%s,%s,%s)".format(parents.mkString("(",", ",")"), children, nodeProps.mkString("(",", ",")"))
  }

  object TreeGraph {
    def fromTree[A:Manifest](t: RoseTree[A], zero: A) = {
      val sz = t.size

      val parents = Array.fill(sz)(0)
      val lens = Array.fill(sz)(0)
      val offsets = Array.fill(sz)(0)
      val values = Array.fill(sz)(0)
      val props = Array.fill(sz)(zero)

      var nextNodeId = 0
      var currOffset = 0
      def visitNode(node: Node[A], iParent: Int, iChild: Int): Unit = { // side effect on arrays above
      val i = nextNodeId
        nextNodeId += 1

        props(i) = node.info                    // (***)
        parents(i) = iParent                    // (***)
        if (iParent >= 0)
          values(offsets(iParent) + iChild) = i   // (***)

        // calculate number of children
        val nChildren = node.children.size
        lens(i) = nChildren      // (***) after loop j is the number of children
        offsets(i) = currOffset  // (***)
        currOffset += nChildren  //  reserve space in "values"

        // visit children recursively
        var child = node.children
        var j = 0             // index of child
        while (!child.isEmpty) {
          visitNode(child.head, i, j)

          child = child.tail
          j += 1
        }
      }

      visitNode(t.root, -1, 0)
      val actualValues = values.take(currOffset)
      new TreeGraph(parents, NArray(lens, offsets, actualValues), props)
    }
  }

}
