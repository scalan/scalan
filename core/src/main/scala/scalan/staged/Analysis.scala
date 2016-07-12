package scalan.staged

import scala.collection.{Seq, mutable}
import scalan.ScalanExp
import scalan.util.CollectionUtil

trait Analysis { self: ScalanExp =>
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
