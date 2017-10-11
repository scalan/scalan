package scalan

import scala.collection.Seq
import scalan.util.CollectionUtil

trait Analyzing { self: Scalan =>

  case class LevelCount[T](level: Int)(implicit val elem: Elem[T]) extends Marking[T] {
    def nonEmpty = true
  }

  implicit object LevelCountLattice extends Lattice[LevelCount] {
    def maximal[T:Elem] = Some(LevelCount(Int.MaxValue))
    def minimal[T:Elem] = Some(LevelCount(0))
    def join[T](a: LevelCount[T], b: LevelCount[T]) = {
      implicit val eT = a.elem
      LevelCount(a.level max b.level)
    }
  }

  class LevelAnalyzer extends BackwardAnalyzer[LevelCount] {
    val name = "levelCounter"
    def lattice = LevelCountLattice
    def defaultMarking[T:Elem] = LevelCount[T](0)
    def mkLevelMark[T](level: Int)(eT: Elem[T]) = LevelCount(level)(eT)

    def updateMark[T](s: Exp[T], level: Int): (Exp[T], LevelCount[T]) = {
      updateMark(s, mkLevelMark(level)(s.elem))
    }

    def getLambdaMarking[A, B](lam: Lambda[A, B], mDom: LevelCount[A], mRange: LevelCount[B]): LevelCount[(A) => B] =
      mkLevelMark(0)(lam.elem)

    def getInboundMarkings[T](te: TableEntry[T], outMark: LevelCount[T]): MarkedSyms = {
      val l = outMark.level
      te.rhs match {
        case lam: Lambda[a,b] =>
          Seq[MarkedSym](updateMark(lam.y, l))
        case _ =>
          te.rhs.getDeps.toSeq.map(s => updateMark(s, l))
      }
    }
  }

  /**
    * Represents usage summary of the symbol it is attached
    * @param counters Key - usage level, Value - symbols that use this symbol from the corresponding level
    */
  case class UsageCount[T](counters: Map[Int, Seq[Sym]])(implicit val elem: Elem[T]) extends Marking[T] {
    def nonEmpty = true
  }

  implicit object UsageCountLattice extends Lattice[UsageCount] {
    def maximal[T:Elem] = None
    def minimal[T:Elem] = Some(UsageCount(Map()))
    def join[T](a: UsageCount[T], b: UsageCount[T]) = {
      implicit val eT = a.elem
      UsageCount[T](
        CollectionUtil.outerJoin(a.counters, b.counters)
                                ((_,l) => l, (_,r) => r, (k,l,r) => (l ++ r).distinct))
    }
  }

  class UsageAnalyzer extends BackwardAnalyzer[UsageCount] {
    val name = "usageCounter"
    val levelAnalyzer = new LevelAnalyzer  // use this to access markings
    def lattice = UsageCountLattice
    def defaultMarking[T:Elem] = UsageCount[T](Map())
    def mkUsageMark[T](counters: Map[Int,Seq[Sym]])(eT: Elem[T]) = UsageCount(counters)(eT)

    def promoteMark[T](s: Exp[T], counters: Map[Int,Seq[Sym]]): (Exp[T], UsageCount[T]) = {
      s -> mkUsageMark(counters)(s.elem)
    }

    def getLevel[T](s: Exp[T]): Int = levelAnalyzer.getMark(s).level

    def getLambdaMarking[A, B](lam: Lambda[A, B], mDom: UsageCount[A], mRange: UsageCount[B]): UsageCount[(A) => B] =
      mkUsageMark(Map())(lam.elem)

    def getInboundMarkings[T](te: TableEntry[T], outMark: UsageCount[T]): MarkedSyms = {
      te.rhs match {
        case l: Lambda[a,b] => Seq()
        case _ =>
          val l = getLevel(te.sym)
          te.rhs.getDeps.toSeq.map(s => {
            promoteMark(s, Map(l -> Seq(te.sym)))
          })
      }
    }
  }

}
