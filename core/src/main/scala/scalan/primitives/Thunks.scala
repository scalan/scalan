package scalan.primitives

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scalan.compilation.GraphVizExport
import scalan.{ScalanExp, ScalanSeq, Scalan}
import scalan.common.{Default, Lazy}
import scala.reflect.runtime.universe._

trait Thunks { self: Scalan =>
  type Th[+T] = Rep[Thunk[T]]
  trait Thunk[+A] { def value: A }
  class ThunkCompanion {
    def apply[T:Elem](block: => Rep[T]) = thunk_create(block)
  }
  val Thunk: ThunkCompanion = new ThunkCompanion

  implicit class RepThunkOps[T: Elem](t: Th[T]) {
    def force() = thunk_force(t)
  }
  case class ThunkElem[A](val eItem: Elem[A]) extends Element[Thunk[A]] {
    override def isEntityType = eItem.isEntityType
    lazy val tag = {
      implicit val rt = eItem.tag
      weakTypeTag[Thunk[A]]
    }
    lazy val defaultRep = Default.defaultVal(Thunk(eItem.defaultRepValue)(eItem))
  }

  implicit def thunkElement[T](implicit eItem: Elem[T]): Elem[Thunk[T]] = new ThunkElem[T](eItem)
  implicit def extendThunkElement[T](elem: Elem[Thunk[T]]): ThunkElem[T] = elem.asInstanceOf[ThunkElem[T]]

  implicit def DefaultOfThunk[A](implicit e: Elem[A]): Default[Rep[Thunk[A]]] = {
    Default.defaultVal[Rep[Thunk[A]]](Thunk(e.defaultRepValue))
  }
  def thunk_create[A:Elem](block: => Rep[A]): Rep[Thunk[A]]
  def thunk_force[A](t: Th[A]): Rep[A]
}

trait ThunksSeq extends Thunks { self: ScalanSeq =>
  def thunk_create[A:Elem](block: => Rep[A]): Rep[Thunk[A]] = new Thunk[A] { def value = block }
  def thunk_force[A](t: Th[A]): Rep[A] = t.value
}

trait ThunksExp extends Thunks with GraphVizExport { self: ScalanExp =>

  case class DefBlock[A](val root: Exp[A], override val schedule: Schedule)
                        (implicit val eA: Elem[A] = root.elem)
    extends BaseDef[Thunk[A]] with AstGraph with Product {
    lazy val uniqueOpId = s"Thunk[${eA.name}]"

    override def mirror(t: Transformer) = {
      val newSym = fresh[Thunk[A]]
      val newSchedule = for {
        tp <- schedule
        res <- t(tp.sym) match {
          case te@TableEntry(_, _) => List(te)
          case _ => Nil
        }
      } yield res
      val newThunk = DefBlock(t(root), newSchedule)
      toExp(newThunk, newSym)
    }

    // structural equality pattern implementation
    override def hashCode: Int = (41 * (41 + root.hashCode) + schedule.hashCode)
    override def equals(other: Any) =
      other match {
        case that: DefBlock[_] =>
          (that canEqual this) &&
            (this.root equals that.root) &&
            (this.schedule equals that.schedule)
        case _ => false
      }
    override def toString = s"Th($root, [${schedule.map(_.sym).mkString(",")}])"
    def canEqual(other: Any) = other.isInstanceOf[DefBlock[_]]

    // Product implementation
    val productElements = scala.Array[Any](root)
    def productElement(n: Int): Any = productElements(n)
    def productArity: Int = 1

    override def boundVars = Nil
    override lazy val freeVars = super.freeVars
    val roots = List(root)
  }

  class ThunkScope(val thunkSym: Exp[Any], val body: ListBuffer[TableEntry[Any]] = ListBuffer.empty) {
    def +=(te: TableEntry[_]) =
      body += te

    def scheduleForResult(root: Exp[Any]): Schedule = {
      val bodySet = body.map(_.sym).toSet
      buildScheduleForResult(Seq(root), _.getDeps.filter(bodySet.contains(_)))
    }

    def findDef[T](d: Def[T]): Option[TableEntry[T]] = {
      body.find(te => te.rhs == d).asInstanceOf[Option[TableEntry[T]]]
    }
  }

  class ThunkStack {
    var stack = new mutable.Stack[ThunkScope]()
    def top: Option[ThunkScope] = stack.headOption
    def push(e: ThunkScope): this.type = { stack.push(e); this }
    def pop: ThunkScope = stack.pop
  }
  protected val thunkStack = new ThunkStack

  def thunk_create[A:Elem](block: => Rep[A]): Rep[Thunk[A]] = {
    val newThunkSym = fresh[Thunk[A]]
    val newScope = new ThunkScope(newThunkSym)

    thunkStack.push(newScope)
    val res = block  // execute block and add all new definitions to the top scope (see createDefinition)
    thunkStack.pop

    val scheduled = newScope.scheduleForResult(res)
    val scheduledSyms = scheduled.map(_.sym).toSet
    val remaining = newScope.body.filterNot(te => scheduledSyms.contains(te.sym))

//    thunkStack.top match {
//      case Some(parentScope) =>
//        parentScope.mergeFromChild(remaining)
//      case None =>
//    }

    val newThunk = DefBlock(res, scheduled)
    toExp(newThunk, newThunkSym)
  }

  var isInlineThunksOnForce = false

  def mirrorThunk[A](thunk: Th[A], subst: MapTransformer = MapTransformer.Empty): Exp[A] = {
    val Def(th: DefBlock[A]) = thunk
    val body = th.scheduleSyms
    val (t, _) = DefaultMirror.mirrorSymbols(subst, NoRewriting, body)
    t(th.root).asRep[A]
  }

  def thunk_force[A](t: Th[A]): Rep[A] =
    if (isInlineThunksOnForce)
      t match {
        case Def(th@DefBlock(_, _)) =>
          mirrorThunk(t)
        case _ => ThunkForce(t)
      }
    else
      ThunkForce(t)

  case class ThunkForce[A](thunk: Exp[Thunk[A]]) extends Def[A]
  {
    implicit def selfType = thunk.elem.eItem
    lazy val uniqueOpId = name(selfType)
    override def mirror(t: Transformer) = ThunkForce(t(thunk))
  }

  override protected def formatDef(d: Def[_]): String = d match {
    case DefBlock(r, sch) => s"Thunk($r, [${sch.map(_.sym).mkString(",")}])"
    case _ => super.formatDef(d)
  }
}
