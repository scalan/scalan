package trees

/**
 * User: Alexander Slesarenko   
 * Date: 11/17/13
 */
trait RoseTrees {

  case class Ptr[T](private var obj: T = null) {
    def get: T = obj
    def set(t: T): Unit = { obj = t }
    override def toString = if (obj != null) "Ptr" else "null"
  }
  case class Node[A](parent: Ptr[Node[A]], children: List[Node[A]], info: A) {
    def size: Int = {
      1 + (0 /: children)(_ + _.size)
    }
  }

  case class RoseTree[A](root: Node[A]) {
    def size = root.size
  }

}
