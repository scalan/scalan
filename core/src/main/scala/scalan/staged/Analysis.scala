package scalan.staged

import scala.collection.{Seq, mutable}
import scalan.ScalanExp
import scalan.util.CollectionUtil

trait Analysis { self: ScalanExp =>
  abstract class Analyzer {
    def name: String
    override def toString = s"Analysis($name)"
  }

  trait JoinSemiLattice[M] {
    def bottom: M
    def join(a: M, b: M): M
  }

  abstract class BackwardAnalyzer[M](implicit val lattice: JoinSemiLattice[M]) extends Analyzer {
    type MarkedSym = (Exp[_], M)

    implicit class ExpMarking(s: Exp[_]) {
      def marked(m: M) = (s, m)
    }

    def keyPrefix: String = name

    def beforeAnalyze[A, B](l: Lambda[A, B]): Unit = {}

    def getInboundMarkings[T](te: TableEntry[T], outMark: M): Seq[MarkedSym]

    def getLambdaMarking[A, B](lam: Lambda[A, B], mDom: M, mRange: M): M

    // Note: MetaKey[Marking] is used as a cheat to avoid requiring Elem[M]
    // This is currently safe
    val markingKey: MetaKey[M] =
      markingKeys.getOrElseUpdate(keyPrefix, MetaKey[Marking](s"marking_${keyPrefix}")).asInstanceOf[MetaKey[M]]

    def clearMark[T](s: Exp[T]): Unit = {
      implicit val eT = s.elem
      s.removeMetadata(markingKey)
    }

    def defaultMark[T](s: Exp[T]): M = lattice.bottom

    def getMark[T](s: Exp[T]): M = {
      val mark = s.getMetadata(markingKey).getOrElse(defaultMark(s))
      mark
    }

    def hasMark[T](s: Exp[T]): Boolean = {
      implicit val eT = s.elem
      s.getMetadata(markingKey).isDefined
    }

    def updateOutboundMarking[T](s: Exp[T], mark: M): Unit = {
      implicit val eT = s.elem
      val current = getMark(s)
      val updated = lattice.join(current, mark)
      s.setMetadata(markingKey)(updated, Some(true))
    }

    def backwardAnalyzeRec(g: AstGraph): Unit = {
      val revSchedule = g.schedule.reverseIterator
      for (te <- revSchedule) {
        val s = te.sym
        val d = te.rhs
        // back-propagate analysis information (including from Lambda to Lambda.y, see LevelAnalyzer)
        val outMark = getMark(s)
        val inMarks = getInboundMarkings(te, outMark)
        for ((s, mark) <- inMarks) {
          updateOutboundMarking(s, mark)
        }
        d match {
          // additionally if it is Lambda
          case l: Lambda[a, b] =>
            // analyze lambda after the markings were assigned to the l.y during previous propagation step
            backwardAnalyzeRec(l)
            // markings were propagated up to the lambda variable
            val mDom = getMark(l.x)
            val mRange = getMark(l.y)

            // update markings attached to l
            val lMark = getLambdaMarking(l, mDom, mRange)
            updateOutboundMarking(l.self, lMark)
          case _ =>
        }
      }
    }
  }

  private val markingKeys = mutable.Map.empty[String, MetaKey[_]]

  // marker trait
  trait Marking {}

  private val defaultMarking = new Marking {
    override def toString = "Default Marking"
  }
  implicit val MarkingElem: Elem[Marking] = new BaseElem[Marking](defaultMarking)

  case class LevelCount(level: Int) extends Marking {
    def +(i: Int) = LevelCount(level + i)
  }

  implicit object LevelSemiLattice extends JoinSemiLattice[LevelCount] {
    val bottom = LevelCount(0)
    def join(a: LevelCount, b: LevelCount) = LevelCount(a.level max b.level)
  }

  class LevelAnalyzer extends BackwardAnalyzer[LevelCount] {
    val name = "levelCounter"

    def level[T](s: Exp[T]) = getMark(s).level

    def getLambdaMarking[A, B](lam: Lambda[A, B], mDom: LevelCount, mRange: LevelCount) = LevelCount(0)

    def getInboundMarkings[T](te: TableEntry[T], outMark: LevelCount) = {
      te.rhs match {
        case ArrayMap(xs, f) =>
          Seq(xs.marked(outMark), f.marked(outMark + 1))
        case ArraySortBy(xs, f, _) =>
          Seq(xs.marked(outMark), f.marked(outMark + 1))
        case ArrayFold(xs, init, f) =>
          Seq(xs.marked(outMark), init.marked(outMark), f.marked(outMark + 1))
        case ArrayFilter(xs, p) =>
          Seq(xs.marked(outMark), p.marked(outMark + 1))
        case lam: Lambda[a,b] =>
          Seq(lam.y.marked(outMark))
        case _ =>
          te.rhs.getDeps.map(_.marked(outMark))
      }
    }
  }

  /**
    * Represents usage summary of the symbol it is attached
    *
    * @param usagesByLevel Key - usage level, Value - symbols that use this symbol from the corresponding level
    */
  case class Usages(usagesByLevel: Map[Int, Seq[Exp[_]]]) extends Marking

  implicit object UsagesSemiLattice extends JoinSemiLattice[Usages] {
    def bottom = Usages(Map())
    def join(a: Usages, b: Usages) = {
      val allUsages =
        CollectionUtil.outerJoin(a.usagesByLevel, b.usagesByLevel)((_, l) => l, (_, r) => r, (k, l, r) => (l ++ r).distinct)
      Usages(allUsages)
    }
  }

  class UsageAnalyzer extends BackwardAnalyzer[Usages] {
    val name = "usageCounter"
    val levelAnalyzer = new LevelAnalyzer

    def usagesByLevel[T](s: Exp[T]) = getMark(s).usagesByLevel
    def allUsages[T](s: Exp[T]) = usagesByLevel(s).valuesIterator.flatten
    def isLinear[T](s: Exp[T]) = {
      val iter = allUsages(s)
      !(iter.hasNext && iter.hasNext)
    }

    def getLambdaMarking[A, B](lam: Lambda[A, B], mDom: Usages, mRange: Usages) =
      lattice.bottom

    def getInboundMarkings[T](te: TableEntry[T], outMark: Usages) = {
      te.rhs match {
        case _: Lambda[_, _] =>
          Seq()
        case rhs =>
          val lhs = te.sym
          val level = levelAnalyzer.level(lhs)
          val usedByLhs = Usages(Map(level -> Seq(lhs)))
          rhs.getDeps.map { _ -> usedByLhs }
      }
    }
  }

}
