package scalan.staged

import scala.collection.mutable
import scalan.ScalanExp
import scalan.common.Lazy

trait Slicing extends ScalanExp {

  class SliceAnalyzer extends BackwardAnalyzer[SliceMarking] {
    val name = SliceMarking.KeyPrefix
    def lattice = SliceLattice
    def defaultMarking[T:Elem]: SliceMarking[T] = EmptyMarking[T](element[T])

    def getLambdaMarking[A, B](lam: Lambda[A, B], mDom: SliceMarking[A], mRange: SliceMarking[B]): SliceMarking[(A) => B] = {
      implicit val eA = lam.eA
      implicit val eB = lam.eB
      FuncMarking(mDom, mRange)
    }

    override def backwardAnalyzeRec(g: AstGraph): Unit = {
      val revSchedule = g.schedule.reverseIterator
      for (te <- revSchedule) te match { case te: TableEntry[t] =>
        val s = te.sym
        val d = te.rhs
        d match {
          case _: AstGraph =>
            // skip non root subgraphs as they are traversed recursively from rules in getInboundMarkings
          case _ =>
            // back-propagate analysis information
            val outMark = getMark(s)
            val inMarks = getInboundMarkings[t](te, outMark)
            for ((s, mark) <- inMarks) {
              updateOutboundMarking(s, mark)
            }
        }
      }
    }

    def analyzeFunc[A,B](f: Rep[A => B], mRes: SliceMarking[B]): FuncMarking[A,B] = {
      val mX = f match {
        case Def(l: Lambda[A,B]@unchecked) =>
          implicit val eA = l.eA
          implicit val eB = l.eB
          updateOutboundMarking(l.y, mRes)
          backwardAnalyzeRec(l)
          getMark(l.x)
        case _ =>
          AllMarking(f.elem.eDom)
      }
      val mF = FuncMarking(mX, mRes)
      updateOutboundMarking(f, mF)
      mF
    }

    def analyzeThunk[A](thunk: Th[A], mRes: SliceMarking[A]): ThunkMarking[A] = {
      val Def(th: ThunkDef[A @unchecked]) = thunk
      implicit val eA = th.selfType.eItem
      updateOutboundMarking(th.root, mRes)
      backwardAnalyzeRec(th)
      val thunkMarking = ThunkMarking(mRes)
      updateOutboundMarking(th.self, thunkMarking)
      thunkMarking
    }

    implicit class ExpOpsForSlicing[T](s: Exp[T]) {
      def marked(m: SliceMarking[T]): MarkedSym = (s, m).asInstanceOf[MarkedSym]
    }

    def getInboundMarkings[T](te: TableEntry[T], outMark: SliceMarking[T]): MarkedSyms = {
      val thisSym = te.sym
      val d = te.rhs
      d match {
        case SimpleStruct(tag, fields) =>
          outMark match {
            case StructMarking(fMarks) =>
              val struct = thisSym.asRep[Struct]
              fMarks.map { case (name, mark) =>
                struct.getUntyped(name) match {
                  case field: Rep[a] => field.marked(mark.asMark[a])
                }
              }
          }

        case FieldApply(s, fn) =>
          Seq((s, StructMarking(Seq(fn -> outMark))(s.elem)))

        case p: First[a,b] =>
          implicit val eA = p.pair.elem.eFst
          implicit val eB = p.pair.elem.eSnd
          Seq(p.pair.marked(PairMarking(outMark.asMark[a], EmptyMarking(eB))))

        case p: Second[a,b] =>
          implicit val eA = p.pair.elem.eFst
          implicit val eB = p.pair.elem.eSnd
          Seq(p.pair.marked(PairMarking(EmptyMarking(eA), outMark.asMark[b])))

        case Tup(a: Rep[a], b: Rep[b]) =>
          outMark match {
            case PairMarking(ma, mb) =>
              Seq[MarkedSym](a.marked(ma.asMark[a]), b.marked(mb.asMark[b]))
          }

        case Apply(f: RFunc[a, b], x) =>
          val mB = outMark.asMark[b]
          val FuncMarking(mA, _) = analyzeFunc(f, mB)
          Seq[MarkedSym](x.marked(mA))

        case _ if outMark.isEmpty =>
          Seq()
        case _ =>
          val deps = te.rhs.getDeps
          val res = deps.map { case s: Exp[a] => (s, AllMarking(s.elem)) }
          res
      }
    }
  }

  val sliceAnalyzer: SliceAnalyzer = createSliceAnalyzer
  protected def createSliceAnalyzer: SliceAnalyzer

  implicit object SliceLattice extends Lattice[SliceMarking] {
    def maximal[T:Elem]: Option[SliceMarking[T]] = Some(AllMarking(element[T]))
    def minimal[T:Elem]: Option[SliceMarking[T]] = Some(EmptyMarking(element[T]))
    def join[T](a: SliceMarking[T], b: SliceMarking[T]) = a.join(b)
  }

  /**
    * Defines a subset of type `T`
    *
    * If extended by a non-case-class, make sure to implement `canEqual`, `equals`, `hashCode`.
    *
    * @tparam T
    */
  trait SliceMarking[T] extends Marking[T] { _: Product =>
    def children: Seq[SliceMarking[_]]

    /**
      * The marking lattice 'meet' operation (greatest lower bound)
      *
      * @param other
      * @return
      */
    def meet(other: SliceMarking[T]): SliceMarking[T]

    /**
      * The marking lattice 'join' operation (least upper bound)
      *
      * @param other
      * @return
      */
    def join(other: SliceMarking[T]): SliceMarking[T]

    // SliceMarking[T] { type Projected = m2.Projected } would be more precise, but doesn't work well
    def >>(m2: SliceMarking[Projected]): SliceMarking[T]

    /**
      * Whether marking represent identity projection
      */
    def isIdentity: Boolean

    /**
      * Whether the resulting type of the projection is inhabited
      */
    def nonEmpty: Boolean
    def isEmpty = !nonEmpty

    /**
      * Add child making for the component specified by `key`.
      *
      * @param key path to the component
      * @param inner marking which describes slicing inside the component
      */
    def |/|[R](key: KeyPath, inner: SliceMarking[R]): SliceMarking[T]
    type Projected
    def project(x: Exp[T]): Exp[Projected]
    def projectedElem: Elem[Projected]
    def makeSlot: Exp[T]
    def set(slot: Exp[T], value: Exp[Projected]): Exp[T]

    protected def setInvalid(slot: Exp[_], value: Exp[_]) =
      !!!(s"$this.set(${slot.toStringWithDefinition}, ${value.toStringWithDefinition}")

    override def toString = s"$productPrefix[${elem.name} -> ${projectedElem.name}]"
  }

  object SliceMarking {
    val KeyPrefix = "slicing"
    type Aux[A, B] = SliceMarking[A] { type Projected = B }
  }

  case class EmptyBaseMarking[T](override val elem: Elem[T])
        extends EmptyMarking[T](elem) with SliceMarking[T] {
    def children: Seq[SliceMarking[_]] = Seq()
    def meet(other: SliceMarking[T]) = this
    def join(other: SliceMarking[T]) = other
    def >>(m2: SliceMarking[Projected]) = this
    def isIdentity = false
    def |/|[R](key: KeyPath, inner: SliceMarking[R]) = !!!(s"Inner marking is not possible for base type ${elem}")

    type Projected = Unit
    def project(x: Exp[T]) = toRep(())
    def projectedElem = element[Unit]
    def makeSlot = SlicedBase((), this)
    def set(slot: Exp[T], value: Exp[Unit]) = slot
  }

  case class AllBaseMarking[T](override val elem: Elem[T])
        extends SliceMarking[T] {
    def nonEmpty = true
    def children: Seq[SliceMarking[_]] = Seq()
    def meet(other: SliceMarking[T]) = other
    def join(other: SliceMarking[T]) = this
    def >>(m2: SliceMarking[Projected]) = {
      assert(elem == m2.elem)
      m2 match {
        case _: AllBaseMarking[_] => this
        case _: EmptyBaseMarking[_] => m2.asMark[T]
        case _ => !!!(s"Cannot compose ${this} >> ${m2}")
      }
    }
    def isIdentity = true
    def |/|[R](key: KeyPath, inner: SliceMarking[R]) = !!!(s"Inner marking is not possible for base type ${elem}")

    type Projected = T
    def project(x: Exp[T]): Exp[T] = x
    def projectedElem: Elem[T] = elem
    def makeSlot = fresh[T](Lazy(elem))
    def set(slot: Exp[T], value: Exp[T]) = {
      assert(value.elem == elem)
      value.asRep[T]
    }
  }

  object EmptyMarking {
    def apply[T](eT: Elem[T]): SliceMarking[T] = createEmptyMarking(eT)
  }

  def createEmptyMarking[T](eT: Elem[T]): SliceMarking[T] = eT match {
    case pe: PairElem[a,b] =>
      implicit val eA = pe.eFst
      implicit val eB = pe.eSnd
      PairMarking(EmptyMarking(eA), EmptyMarking(eB))
    case pe: FuncElem[a,b] =>
      implicit val eA = pe.eDom
      implicit val eB = pe.eRange
      FuncMarking(EmptyMarking(eA), EmptyMarking(eB))
    case se: StructElem[Struct]@unchecked =>
      StructMarking[Struct](Seq())(se)
    case be: BaseElem[a] =>
      EmptyBaseMarking(be)
    case te: ThunkElem[a] =>
      val eA = te.eItem
      ThunkMarking(EmptyMarking(eA)).asMark[T]
    case ae: ArrayElem[a] =>
      val eA = ae.eItem
      ArrayMarking(KeyPath.None, EmptyMarking(eA)).asMark[T]
    case _ =>
      !!!(s"Cannot create empty marking for element ${eT}")
  }

  object AllMarking {
    def apply[T](eT: Elem[T]): SliceMarking[T] = createAllMarking(eT)
  }

  def createAllMarking[T](e: Elem[T]): SliceMarking[T] = e match {
    case ae: ArrayElem[a] =>
      implicit val eA = ae.eItem
      ArrayMarking[a](KeyPath.All, AllMarking(eA)).asMark[T]
    case pe: PairElem[a,b] =>
      implicit val eA = pe.eFst
      implicit val eB = pe.eSnd
      PairMarking[a,b](AllMarking(eA), AllMarking(eB)).asMark[T]
    case fe: FuncElem[a,b] =>
      FuncMarking[a,b](AllMarking(fe.eDom), AllMarking(fe.eRange)).asMark[T]
    case se: StructElem[s] =>
      val fields = se.fields.map { case (fn, e) => (fn, AllMarking(e)) }
      StructMarking[Struct](fields)(se.asElem[Struct]).asMark[T]
    case be: BaseElem[a] =>
      AllBaseMarking[a](be).asMark[T]
    case te: ThunkElem[a] =>
      implicit val eA = te.eItem
      ThunkMarking(AllMarking(eA)).asMark[T]
    case _ =>
      ???(s"Elem $e cannnot be root of SliceMarking")
  }

  // Not a case class because non-empty fields have to be filtered out on construction
  class StructMarking[T <: Struct] private (val fields: Seq[(String, SliceMarking[_])], val elem: Elem[T]) extends SliceMarking[T] with Product {
    val fieldNames = fields.map(_._1)
    def get(fn: String): Option[SliceMarking[_]] = fields.find(_._1 == fn).map(_._2)
    def children = fields.map(_._2)
    def nonEmpty = fields.exists(_._2.nonEmpty)
    def isIdentity = {
      fieldNames == elem.fieldNames && fields.forall(_._2.isIdentity)
    }
    def meet(other: SliceMarking[T]) = other match { case other: StructMarking[T]@unchecked =>
      ???
    }
    override def join(other: SliceMarking[T]) = {
      assert(elem == other.elem, s"${elem} != ${other.elem}")
      other match {
        case other: StructMarking[T]@unchecked =>
          val res = mutable.ArrayBuffer.empty[(String, SliceMarking[_])]
          for ((fn, e) <- elem.fields) {
            val optThis = this.get(fn)
            val optOther = other.get(fn)
            (optThis, optOther) match {
              case (Some(m1: SliceMarking[a]), Some(m2)) =>
                res += ((fn -> m1.join(m2.asMark[a])))
              case (Some(m1), None) =>
                res += ((fn -> m1))
              case (None, Some(m2)) =>
                res += ((fn -> m2))
              case _ => // skip this field
            }
          }
          StructMarking(res)(elem)
      }
    }

    def >>(other: SliceMarking[Projected]) = {
      assert(this.projectedElem == other.elem)
      other match {
        case other: StructMarking[Projected] =>
          val newFields = other.fields.map { case (fn, mOther) =>
            this.get(fn) match {
              case Some(mThis) =>
                fn -> (mThis >> mOther.asMark[mThis.Projected])
              case _ =>
                !!!(s"Inconsistent marking ${this} >> ${other}")
            }
          }
          StructMarking(newFields)(elem)
      }
    }

    def |/|[R](key: KeyPath, inner: SliceMarking[R]) = key match {
      case KeyPath.Field(keyName) =>
        StructMarking[T](fields.map { case (fn, m) => if (fn == keyName) (fn, inner) else (fn, m) })(elem)
      case _ =>
        !!!(s"StructMarking |/| ($key, $inner)")
    }

    type Projected <: Struct
    def project(x: Exp[T]): Exp[Projected] = {
      val structFields = fields.map {
        case (name, marking: SliceMarking[a]) =>
          val projectedField = marking.project(x.getUnchecked[a](name))
          (name, projectedField)
      }
      struct(structFields).asRep[Projected]
    }
    val projectedElem: Elem[Projected] = {
      val projectedFields = fields.map {
        case (name, marking) => name -> marking.projectedElem
      }
      structElement(elem.structTag, projectedFields).asElem[Projected]
    }
    def makeSlot = {
      val slotFields = fields.map {
        case (name, marking) => name -> marking.makeSlot
      }
      val source = struct(elem.structTag, slotFields)
      SlicedStruct(source, this)
    }

    def set(slot: Exp[T], value: Exp[Projected]) = slot match {
      case Def(ss: SlicedStruct[_, _]) =>
        assert(value.elem == ss.mark.projectedElem, s"${value.elem} != ${ss.mark.projectedElem}")
        SlicedStruct(value.asRep[Struct], ss.mark)
      case _ =>
        setInvalid(slot, value)
    }

    def canEqual(other: Any) = other.isInstanceOf[StructMarking[_]]
    override def equals(other: Any) = other match {
      case other: StructMarking[_] =>
        (this eq other) || (other.canEqual(this) && fields.toList == other.fields.toList && elem == other.elem)
      case _ => false
    }
    override def hashCode = fields.hashCode * 21 + elem.hashCode

    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => fields
      case 1 => elem
      case _ => throw new NoSuchElementException(s"StructMarking.productElement($n)")
    }
  }
  object StructMarking {
    def apply[T <: Struct](fields: Seq[(String, SliceMarking[_])])(elem: Elem[T]) =
      new StructMarking(fields.filter(_._2.nonEmpty), elem)
    def unapply(sm: SliceMarking[_]): Option[Seq[(String, SliceMarking[_])]] = sm match {
      case sm: StructMarking[_] => Some(sm.fields)
      case _ => None
    }
  }

  case class PairMarking[A,B](markA: SliceMarking[A], markB: SliceMarking[B]) extends SliceMarking[(A,B)] {
    implicit val eA = markA.elem
    implicit val eB = markB.elem
    val elem = element[(A,B)]
    def children = Seq(markA, markB)
    def nonEmpty = markA.nonEmpty || markB.nonEmpty
    def isIdentity = markA.isIdentity && markB.isIdentity
    def meet(other: SliceMarking[(A,B)]) = other match {
      case other: PairMarking[A,B]@unchecked =>
        PairMarking(markA.meet(other.markA), markB.meet(other.markB))
    }
    def join(other: SliceMarking[(A,B)]) = other match {
      case other: PairMarking[A,B]@unchecked =>
        PairMarking(markA.join(other.markA), markB.join(other.markB))
    }
    def >>(m2: SliceMarking[Projected]) = {
      assert(this.projectedElem == m2.elem)
      m2 match {
        case m2: PairMarking[markA.Projected, markB.Projected] @unchecked =>
          PairMarking(markA >> m2.markA, markB >> m2.markB)
      }
    }
    def |/|[R](key: KeyPath, inner: SliceMarking[R]) = key match {
      case KeyPath.First if inner.elem == eA =>
        PairMarking(inner.asMark[A], markB)
      case KeyPath.Second if inner.elem == eB =>
        PairMarking(markA, inner.asMark[B])
    }

    type Projected = (markA.Projected, markB.Projected)
    def project(x: Exp[(A,B)]) =
      Pair(markA.project(x._1), markB.project(x._2))
    val projectedElem =
      pairElement(markA.projectedElem, markB.projectedElem)
    def makeSlot = {
      SlicedPair(Pair(markA.makeSlot, markB.makeSlot), this)
    }
    def set(slot: Exp[(A, B)], value: Exp[Projected]) = slot match {
      case Def(sp: SlicedPair[A,B,a,b]@unchecked) =>
        assert(value.elem == sp.mark.projectedElem, s"${value.elem} != ${sp.mark.projectedElem}")
        SlicedPair(value.asRep[(a,b)], sp.mark)
      case _ =>
        setInvalid(slot, value)
    }
  }
  object FirstMarking {
    def unapply[T](m: SliceMarking[T]): Option[SliceMarking[_]] = m match {
      case PairMarking(ma,mb) if ma.nonEmpty && mb.isEmpty => Some(ma)
      case _ => None
    }
  }
  object SecondMarking {
    def unapply[T](m: SliceMarking[T]): Option[SliceMarking[_]] = m match {
      case PairMarking(ma,mb) if ma.isEmpty && mb.nonEmpty => Some(mb)
      case _ => None
    }
  }

  case class FuncMarking[A,B](mDom: SliceMarking[A], mRange: SliceMarking[B]) extends SliceMarking[A => B] {
    implicit val eA = mDom.elem
    implicit val eB = mRange.elem
    val elem = element[A => B]
    def children = Seq(mDom, mRange)
    def nonEmpty = mDom.nonEmpty || mRange.nonEmpty
    def isIdentity = mDom.isIdentity && mRange.isIdentity
    def meet(other: SliceMarking[A => B]) = other match {
      case other: FuncMarking[A,B]@unchecked =>
        FuncMarking(mDom.meet(other.mDom), mRange.meet(other.mRange))
    }
    def join(other: SliceMarking[A => B]) = other match {
      case other: FuncMarking[A,B]@unchecked =>
        FuncMarking(mDom.join(other.mDom), mRange.join(other.mRange))
    }
    def >>(other: SliceMarking[Projected]) = ???
    def |/|[R](key: KeyPath, inner: SliceMarking[R]) = ???

    type Projected = mDom.Projected => mRange.Projected
    def project(f: Exp[A => B]) =
      sliceFunc(f, this)

    val projectedElem =
      funcElement(mDom.projectedElem, mRange.projectedElem)
    def makeSlot: Exp[A => B] =
      SlicedFunc(fresh(Lazy(funcElement(mDom.projectedElem, mRange.projectedElem))), this)
    def set(slot: Exp[A => B], value: Exp[Projected]) = slot match {
      case Def(sf: SlicedFunc[a, b, a1, b1]) =>
        SlicedFunc(value.asRep[a1 => b1], this)
      case _ =>
        setInvalid(slot, value)
    }
  }

  abstract class SliceMarking1[T, F[_]](implicit val cF: Cont[F]) extends SliceMarking[F[T]] { _: Product =>
    val innerMark: SliceMarking[T]
    implicit val eItem = innerMark.elem
    val elem = cF.lift(eItem)
    def children: Seq[SliceMarking[_]] = Seq(innerMark)
    type Projected = F[innerMark.Projected]
    val projectedElem = cF.lift(innerMark.projectedElem)
  }

  case class TraversableMarking[T, F[_]](itemsPath: KeyPath, innerMark: SliceMarking[T], override val cF: Cont[F]) extends SliceMarking1[T, F]()(cF) {
    def >>(other: SliceMarking[Projected]) = other match {
      case TraversableMarking(KeyPath.None, innerMark1, _) =>
        copy(itemsPath = KeyPath.None, innerMark = innerMark >> innerMark1)
      case TraversableMarking(`itemsPath`, innerMark1, _) =>
        copy(innerMark = innerMark >> innerMark1)
    }

    def meet(other: SliceMarking[F[T]]) = ???

    def join(other: SliceMarking[F[T]]) = other match {
      case _ if this.itemsPath == KeyPath.None =>
        other
      case TraversableMarking(KeyPath.None, _, _) =>
        this
      case TraversableMarking(`itemsPath`, innerMark1, _) =>
        copy(innerMark = innerMark.join(innerMark1))
    }

    def nonEmpty = !itemsPath.isNone && innerMark.nonEmpty
    def isIdentity = itemsPath.isAll && innerMark.isIdentity

    def |/|[R](key: KeyPath, inner: SliceMarking[R]) = key match {
      case KeyPath.All if inner.elem == eItem =>
        copy(key, inner.asMark[T])
    }

    def project(xs: Exp[F[T]]) = itemsPath match {
      case KeyPath.All =>
        assert(xs.elem == this.elem)
        reifyObject(UnpackSliced(xs, this))
      case KeyPath.None =>
        projectedElem.defaultRepValue
      case _ =>
        !!!(s"Expect itemsPath to be All or None, but got $itemsPath")
    }

    def makeSlot =
      SlicedTraversable(fresh(Lazy(projectedElem)), innerMark, cF)

    def set(slot: Exp[F[T]], value: Exp[Projected]) = slot match {
      case Def(sliced: SlicedTraversable[T, a, F] @unchecked) =>
        sliced.copy(value.asRep[F[a]])
      case _ =>
        setInvalid(slot, value)
    }
  }

  class TraversableMarkingFor[F[_]](implicit cF: Cont[F]) {
    def apply[A](innerMark: SliceMarking[A]): TraversableMarking[A, F] =
      TraversableMarking(KeyPath.All, innerMark, cF)
    def apply[A](keyPath: KeyPath, innerMark: SliceMarking[A]): TraversableMarking[A, F] =
      TraversableMarking(keyPath, innerMark, cF)
    // Could take SliceMarking[F[A]], but this leads to a lot of unchecked warnings
    def unapply(mark: SliceMarking[_]) = mark match {
      case TraversableMarking(keyPath, innerMark, cF1) if cF == cF1 => Some((keyPath, innerMark))
      case _ => None
    }

    override def toString = s"TraversableMarkingFor[${cF.name}]"
  }

  val ArrayMarking = new TraversableMarkingFor[Array]
  val ListMarking = new TraversableMarkingFor[List]

  case class ThunkMarking[A](innerMark: SliceMarking[A]) extends SliceMarking1[A, Thunk] {
    def meet(other: SliceMarking[Thunk[A]]) = other match {
      case am: ThunkMarking[A] @unchecked =>
        ThunkMarking(innerMark.meet(am.innerMark))
    }
    def >>(other: SliceMarking[Projected]) = ???
    def join(other: SliceMarking[Thunk[A]]) = other match {
      case am: ThunkMarking[A] @unchecked =>
        ThunkMarking(innerMark.join(am.innerMark))
    }
    def nonEmpty = innerMark.nonEmpty
    def isIdentity = innerMark.isIdentity
    def |/|[R](key: KeyPath, inner: SliceMarking[R]) = key match {
      case KeyPath.All if inner.elem == eItem =>
        ThunkMarking[A](inner.asMark[A])
    }
    def project(thunk: Exp[Thunk[A]]) = thunk match {
      case Def(SlicedThunk(thunk1, m1)) if this == m1 =>
        thunk1.asRep[Projected]
      case _ if innerMark.isIdentity =>
        thunk.asRep[Projected]
      case _ =>
        implicit val eB = innerMark.projectedElem
        Thunk {
          val forced = thunk.force()
          val projected = innerMark.project(forced)
          projected
        }
    }

    def makeSlot = SlicedThunk(fresh(Lazy(projectedElem)), this)
    def set(slot: Exp[Thunk[A]], value: Exp[Projected]) = slot match {
      case Def(sf: SlicedThunk[a, a1]) =>
        SlicedThunk(value.asRep[Thunk[a1]], this)
      case _ =>
        setInvalid(slot, value)
    }
  }

  implicit class SliceMarkingOps[T](m: SliceMarking[T]) {
    def asMark[R]: SliceMarking[R] = m.asInstanceOf[SliceMarking[R]]
  }

  implicit class ElemOpsForSlicing[T](e: Elem[T]) {
    def toMarking: SliceMarking[T] = AllMarking(e)
  }

  implicit class StructElemOpsForSlicing[T <: Struct](e: Elem[T]) {
    def toStructMarking: StructMarking[T] = e.toMarking.asInstanceOf[StructMarking[T]]
  }

  implicit class ExpOpsForSlicing(e: Exp[_]) {
    def marked(m: SliceMarking[_]): MarkedSym = (e, m).asInstanceOf[MarkedSym]
  }

  def sliceIn[A,B](f: Exp[A => B], m: SliceMarking[A]): Exp[m.Projected => B] = f match {
    case Def(d) => d match {
      case SlicedFunc(fs, fm) =>
        if (m == fm.mDom && fm.mRange.isIdentity)
          fs.asRep[m.Projected => B] // m.Projected == A
        else
          !!!(s"sliceIn(${f.toStringWithDefinition}, $m)")
      case lam: Lambda[A, B] @unchecked =>
        val elem = f.elem
        implicit val eA = elem.eDom
        implicit val eB = elem.eRange
        implicit val eC = m.projectedElem
        val res = fun { x: Exp[m.Projected] =>
          val slot = m.makeSlot
          val init = m.set(slot, x)
          mirrorApply(lam, init)
        }
        res
    }
  }

  def sliceOut[A,B](f: Exp[A => B], m: SliceMarking[B]): Exp[A => m.Projected] = {
    val elem = f.elem
    implicit val eA = elem.eDom
    implicit val eB = elem.eRange
    implicit val eC = m.projectedElem
    val res = fun { x: Exp[A] =>
      val y = f(x)
      m.project(y)
    }
    res
  }

  def sliceFunc[A,B](f: Exp[A => B], m: FuncMarking[A,B]): Exp[m.Projected] = (f match {
    case Def(SlicedFunc(f1, m1)) if m == m1 =>
      f1
    case _ =>
      (m.mDom, m.mRange) match {
        case (_, _) if m.isIdentity =>
          f
        case (mD, mR) if mD.isIdentity =>
          sliceOut(f, mR)
        case (mD, mR) if mR.isIdentity =>
          sliceIn(f, mD)
        case (mD, mR) =>
          val f_out = sliceOut(f, mR)
          sliceIn(f_out, mD)
      }
  }).asRep[m.Projected]

  def sliceFunc[A,B](f: Exp[A => B], m: SliceMarking[A => B]): Exp[m.Projected] = m match {
    case fm: FuncMarking[A,B]@unchecked => sliceFunc(f, fm).asRep[m.Projected]
    case _ => !!!(s"FuncMarking expected but found ${m}")
  }

  class SlicingMirror(sliceAnalyzer: SliceAnalyzer, graph: PGraph) extends Mirror[MapTransformer] {

//    override def mirrorNode[A](t: MapTransformer, rewriter: Rewriter, g: AstGraph, node: Exp[A]): (MapTransformer, Exp[_]) = {
//      val (t1, mirrored) = super.mirrorNode(t, rewriter, g, node)
//      implicit val eA = mirrored.elem.asElem[A]
//      val mark = mirrored.getMetadata(markingKey[A](SliceMarking.KeyPrefix))
//      if (mark.isDefined) {
//        println(s"Marking: ${mark.get}")
//      }
//      (t1,mirrored)
//    }
//    protected override def mirrorMetadata[A, B](
//                                                 t: MapTransformer, old: Exp[A], mirrored: Exp[B]): (MapTransformer, MetaNode) = {
//      val (t1, metaNode) = super.mirrorMetadata(t, old, mirrored)
//
//      //      implicit val eA = old.elem
//      //      val mark = old.getMetadata(sliceMarkingKey[A])
//      //      if (mark.isDefined) {
//      ////        println(mark.get)
//      //        println(s"MetaNode: $metaNode")
//      //      }
//
//      (t1, metaNode)
//    }
    override protected def mirrorLambda[A, B](
        t: MapTransformer, rewriter: Rewriter, node: Exp[(A) => B], lam: Lambda[A, B]): (MapTransformer, Exp[_]) = {
      if (graph.roots.contains(node)) {
        val fm = sliceAnalyzer.getMark(node)
        if (!fm.isIdentity) {
          val fs = sliceFunc(node, fm)
          val res = Sliced(fm)(fs)
          (t + (node -> res), res)
        }
        else
          super.mirrorLambda(t, rewriter, node, lam)
      }
      else
        super.mirrorLambda(t, rewriter, node, lam)
    }
  }

  abstract class Sliced[From, To] extends Def[From] {
    def source: Rep[To]
    def mark: SliceMarking[From]
    implicit lazy val selfType = mark.elem
  }

  case class SlicedFunc[AFrom, BFrom, ATo, BTo](source: Rep[ATo => BTo], mark: FuncMarking[AFrom, BFrom])
    extends Sliced[AFrom => BFrom, ATo => BTo] {
  }

  case class SlicedThunk[AFrom, ATo](source: Rep[Thunk[ATo]], mark: ThunkMarking[AFrom]) extends
    Sliced[Thunk[AFrom], Thunk[ATo]]

  case class UnpackSliced[From, To](sliced: Rep[From], mark: SliceMarking[From]) extends Def[To] {
    implicit def selfType = mark.projectedElem.asElem[To]
  }

  case class SlicedTraversable[A, B, F[_]](source: Rep[F[B]], innerMark: SliceMarking[A], cF: Cont[F]) extends Sliced[F[A], F[B]] {
    val mark = TraversableMarking(KeyPath.All, innerMark, cF)
    override def toString = s"SlicedTraversable[${cF.name}][${innerMark.elem.name}]($source)"
  }

  case class SlicedStruct[From <: Struct, To <: Struct](source: Rep[To], mark: StructMarking[From])
    extends Sliced[From, To] {
  }

  case class SlicedBase[A](source: Rep[Unit], mark: EmptyBaseMarking[A])
    extends Sliced[A, Unit] {
  }

  case class SlicedPair[A,B,A1,B1](source: Rep[(A1,B1)], mark: PairMarking[A,B])
    extends Sliced[(A,B), (A1,B1)] {
  }

  def getAllSliced[A,B](g: AstGraph): Seq[TableEntry[_]] = {
    g.scheduleAll.collect { case te @ TableEntry(s, _: Sliced[_,_]) => te }
  }

  type SliceInfo[A,B] = (Exp[B], SliceMarking[A])
  object IsSliced {
    def unapply(s: Def[_]): Option[SliceInfo[T,_] forSome {type T}] = s match {
      case sliced: Sliced[a,b] =>
        Some((sliced.source, sliced.mark))
      case _ =>
        None
    }
    def unapply(s: Exp[_]): Option[SliceInfo[T,_] forSome {type T}] = s match {
      case Def(IsSliced(s, m)) => Some((s,m))
      case _ => None
    }
  }

  object IsSlicedFunc {
    def unapply[T](s: Exp[T]): Option[(SlicedFunc[_,_,_,_], Exp[_], SliceMarking[T])] = s match {
      case Def(sliced: SlicedFunc[a,b,c,d]) =>
        Some((sliced, sliced.source, sliced.mark.asMark[T]))
      case Def(l: Lambda[a,b]) if sliceAnalyzer.hasMark(s) =>
        val f = s.asRep[a => b]
        val fm = sliceAnalyzer.getMark(f).asInstanceOf[FuncMarking[a,b]]
        if (fm.isIdentity)
          None
        else {
          val fs = sliceFunc(f, fm)
          Some((SlicedFunc(fs, fm), fs, fm.asMark[T]))
        }
      case _ =>
        None
    }
  }

  class SlicingRewriter(sliceAnalyzer: SliceAnalyzer, graph: PGraph) extends Rewriter {
    def apply[T](x: Exp[T]): Exp[T] = x
  }

  object Sliced {
    def apply[A](m: SliceMarking[A])(s: Exp[m.Projected]): Exp[A] =
      if (m.isIdentity)
        s.asRep[A]
      else {
        val slot = m.makeSlot
        m.set(slot, s)
      }
  }

  override def rewriteDef[T](d: Def[T]): Exp[_] = d match {
    case First(IsSliced(p, m: PairMarking[a,b])) =>
      Sliced(m.markA)(p.asRep[m.Projected]._1)
    case Second(IsSliced(p, m: PairMarking[a,b])) =>
      Sliced(m.markB)(p.asRep[m.Projected]._2)
    case FieldApply(IsSliced(p, m: StructMarking[_]), name) =>
      m.get(name) match {
        case Some(m1) =>
          val field = p.asRep[m.Projected].getUntyped(name).asRep[m1.Projected]
          Sliced(m1)(field)
        case None =>
          assert(false, s"Field $name accessed in a sliced struct with source ${p.toStringWithDefinition}, mark $m")
      }
    case Apply(IsSliced(f: RFunc[a, b] @unchecked, m: FuncMarking[c, _]), x) =>
      val x1 = m.mDom.project(x.asRep[c]).asRep[a]
      assert(x1.elem == f.elem.eDom)
      val y1 = f(x1).asRep[m.mRange.Projected]
      Sliced(m.mRange)(y1)

    case Apply(f: RFunc[a, b] @unchecked, IsSliced(x: Rep[c], m)) =>
      // is this correct?
      val m1 = m.asMark[a]
      val f1 = sliceIn(f, m1)
      f1(x.asRep[m1.Projected])

    case IsSliced(IsSliced(s, m1), m2) =>
      assert(m2.projectedElem == m1.elem,
        s"Nested Sliced with non-composing markings: m2.projectedElem = ${m2.projectedElem}, m1.elem = ${m1.elem}")
      val m = m2 >> m1.asMark[m2.Projected]
      Sliced(m)(s.asRep[m.Projected])

    case UnpackSliced(IsSliced(x, m1), m2) if m1 == m2 =>
      x

    case _ => super.rewriteDef(d)
  }
}
