package scalan.primitives

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
    def apply() = thunk_force(t)
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

trait ThunksExp extends Thunks { self: ScalanExp =>

  case class DefBlock[A](val root: Exp[A], override val schedule: Schedule)
                        (implicit val eA: Elem[A] = root.elem)
    extends BaseDef[Thunk[A]] with AstGraph {
    lazy val uniqueOpId = s"Thunk[${eA.name}]"

    override def mirror(t: Transformer) = {
      val newSym = fresh[Thunk[A]]
      val newSchedule = schedule.collect(tp => t(tp.sym) match {
        case te@ TableEntry(_, _) => te
      })
      val newThunk = DefBlock(t(root), newSchedule)
      toExp(newThunk, newSym)
    }

    override def boundVars = Nil
    override lazy val freeVars = super.freeVars
    val roots = List(root)
  }

  def thunk_create[A](block: => Rep[A]): Rep[Thunk[A]] = {
    ???
  }
  def thunk_force[A](t: Th[A]): Rep[A] = ???
}
