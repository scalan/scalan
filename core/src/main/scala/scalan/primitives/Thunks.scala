package scalan.primitives

import scala.collection.mutable
import scalan.compilation.GraphVizExport
import scalan.{ScalanExp, ScalanSeq, Scalan}
import scalan.common.{Default, Lazy}
import scala.reflect.runtime.universe._

trait Thunks { self: Scalan =>
  type Th[+T] = Rep[Thunk[T]]
  trait Thunk[+A] { def value: A }
  class ThunkCompanion {
    def apply[T](block: => Rep[T]) = thunk_create(block)
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
    lazy val defaultRep = Default.defaultVal(Thunk(eItem.defaultRepValue))
  }

  implicit def thunkElement[T](implicit eItem: Elem[T]): Elem[Thunk[T]] = new ThunkElem[T](eItem)
  implicit def extendThunkElement[T](elem: Elem[Thunk[T]]): ThunkElem[T] = elem.asInstanceOf[ThunkElem[T]]

  implicit def DefaultOfThunk[A](implicit e: Elem[A]): Default[Rep[Thunk[A]]] = {
    Default.defaultVal[Rep[Thunk[A]]](Thunk(e.defaultRepValue))
  }
  def thunk_create[A](block: => Rep[A]): Rep[Thunk[A]]
  def thunk_force[A](t: Th[A]): Rep[A]
}

trait ThunksSeq extends Thunks { self: ScalanSeq =>
  def thunk_create[A](block: => Rep[A]): Rep[Thunk[A]] = new Thunk[A] { def value = block }
  def thunk_force[A](t: Th[A]): Rep[A] = t.value
}

trait ThunksExp extends Thunks with GraphVizExport { self: ScalanExp =>

  case class DefBlock[A](val root: Exp[A], override val schedule: Schedule)
                        (implicit val eA: Elem[A] = root.elem)
    extends BaseDef[Thunk[A]] with AstGraph {
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

    override def boundVars = Nil
    override lazy val freeVars = super.freeVars
    val roots = List(root)
  }

  class ThunkScope(val body: mutable.Set[Exp[Any]] = mutable.Set()) {
    def +=(s: Exp[Any]) =
      body += s

    def scheduleForResult(root: Exp[Any]): Schedule = {
      buildScheduleForResult(Seq(root), _.getDeps.filter(body.contains(_)))
    }
  }

  class ThunkStack {
    var stack = new mutable.Stack[ThunkScope]()
    def top: Option[ThunkScope] = stack.headOption
    def push(e: ThunkScope): this.type = { stack.push(e); this }
    def pop: ThunkScope = stack.pop
  }
  protected val thunkStack = new ThunkStack

  def thunk_create[A](block: => Rep[A]): Rep[Thunk[A]] = {
    val newScope = new ThunkScope

    thunkStack.push(newScope)
    val res = block  // execute block and add all new definitions to the top scope (see createDefinition)
    thunkStack.pop

    val schedule = newScope.scheduleForResult(res)
    val scheduleSyms = schedule.map(_.sym)
    val remaining = newScope.body -- scheduleSyms

    thunkStack.top match {
      case Some(parentScope) =>
        parentScope.body ++= remaining
      case None =>
    }

    implicit val eA = res.elem
    DefBlock(res, schedule)
  }

  def thunk_force[A](t: Th[A]): Rep[A] = ThunkForce(t)

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
