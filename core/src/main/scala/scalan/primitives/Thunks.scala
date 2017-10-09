package scalan.primitives

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.{ViewsModule, Scalan}
import scala.reflect.runtime.universe._
import scalan.common.Lazy
import scalan.util.Covariant

trait Thunks extends Functions with ViewsModule with GraphVizExport with Effects { self: Scalan =>
  type Th[+T] = Rep[Thunk[T]]
  trait Thunk[+A] { def value: A }
  class ThunkCompanion {
    def apply[T](block: => Rep[T]) = thunk_create(block)
  }
  val Thunk: ThunkCompanion = new ThunkCompanion

  implicit class RepThunkOps[T](t: Th[T]) {
    def force() = thunk_force(t)
    def map[R](f: Rep[T => R]): Th[R] = thunk_map(t, f)
    def map[R](f: Rep[T] => Rep[R]): Th[R] = thunk_map1(t, f)
  }

  implicit val thunkCont: Cont[Thunk] = new Cont[Thunk] {
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[Thunk[T]]
    def lift[T](implicit eT: Elem[T]) = element[Thunk[T]]
    def unlift[T](implicit eFT: Elem[Thunk[T]]) = eFT.eItem
    def getElem[T](fa: Rep[Thunk[T]]) = !!!("Operation is not supported by Thunk container " + fa)
    def unapply[T](e: Elem[_]) = e match {
      case e: ThunkElem[_] => Some(e.asElem[Thunk[T]])
      case _ => None
    }
  }

  case class ThunkElem[A](override val eItem: Elem[A])
    extends EntityElem1[A, Thunk[A], Thunk](eItem, container[Thunk]) {
    def parent: Option[Elem[_]] = None
    override lazy val typeArgs = TypeArgs("A" -> (eItem -> Covariant))
    lazy val tag = {
      implicit val rt = eItem.tag
      weakTypeTag[Thunk[A]]
    }
    protected def getDefaultRep = Thunk(eItem.defaultRepValue)
  }

  implicit def thunkElement[T](implicit eItem: Elem[T]): Elem[Thunk[T]] =
    cachedElem[ThunkElem[T]](eItem)
  implicit def extendThunkElement[T](elem: Elem[Thunk[T]]): ThunkElem[T] = elem.asInstanceOf[ThunkElem[T]]

  case class ThunkDef[A](val root: Exp[A], override val schedule: Schedule)
    extends BaseDef[Thunk[A]]()(thunkElement(root.elem)) with AstGraph with Product {
    implicit val eA: Elem[A] = root.elem
    // structural equality pattern implementation
    override lazy val hashCode: Int = 41 * (41 + root.hashCode) + schedule.hashCode
    override def equals(other: Any) =
      other match {
        case that: ThunkDef[_] =>
          (that canEqual this) &&
            (this.root equals that.root) &&
            (this.schedule equals that.schedule)
        case _ => false
      }
    override def toString = s"Th($root, [${scheduleSyms.mkString(",")}])"
    def canEqual(other: Any) = other.isInstanceOf[ThunkDef[_]]

    // Product implementation
    def productElement(n: Int): Any = n match {
      case 0 => root
      case _ => throw new NoSuchElementException(s"ThunkDef.productElement($n) is undefined")
    }
    def productArity: Int = 1

    override def boundVars = Nil
    override lazy val freeVars = super.freeVars
    val roots = List(root)
  }

  override def transformDef[A](d: Def[A], t: Transformer) = d match {
    case thunk: ThunkDef[a] =>
      implicit lazy val eA = thunk.eA
      val newSym = fresh[Thunk[a]]
      val newSchedule = for {
        tp <- thunk.schedule
        res <- t(tp.sym) match {
          case te@TableEntry(_, _) => List(te)
          case _ => Nil
        }
      } yield res
      val newThunk = ThunkDef(t(thunk.root), newSchedule)
      toExp(newThunk, newSym)
    case _ => super.transformDef(d, t)
  }

  case class ThunkView[A, B](source: Rep[Thunk[A]])(innerIso: Iso[A, B])
    extends View1[A, B, Thunk](thunkIso(innerIso)) {
  }

  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ThunkView[_,_]) =>
      Some((view.source, view.iso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

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

  protected def currentThunkSym = thunkStack.top match {
    case Some(scope) => scope.thunkSym
    case None => globalThunkSym
  }

  def thunk_create[A](block: => Rep[A]): Rep[Thunk[A]] = {
    var eA: Elem[A] = null // will be known after block is evaluated
    val newThunkSym = fresh[Thunk[A]](Lazy{ thunkElement(eA) })
    val newScope = new ThunkScope(newThunkSym)

    thunkStack.push(newScope)
    // execute block and add all new definitions to the top scope (see createDefinition)
    // reify all the effects during block execution
    val b @ Block(res) = reifyEffects(block)
    eA = res.elem
    val eTh = newThunkSym.elem  // force lazy value in newThunkSym (see Lazy above)
    thunkStack.pop

    val scheduled = newScope.scheduleForResult(res)
    val scheduledSyms = scheduled.map(_.sym).toSet
    val remaining = newScope.body.filterNot(te => scheduledSyms.contains(te.sym))

    val newThunk = ThunkDef(res, scheduled)
    val u = summarizeEffects(b)
    reflectEffect(newThunk, u, newThunkSym)
  }

  def thunk_map[A, B](t: Th[A], f: Rep[A => B]): Th[B] = {
    Thunk {
      f(thunk_force(t))
    }
  }
  def thunk_map1[A, B](t: Th[A], f: Rep[A] => Rep[B]): Th[B] = {
    Thunk {
      f(thunk_force(t))
    }
  }

  var isInlineThunksOnForce = false

  def forceThunkByMirror[A](thunk: Th[A], subst: MapTransformer = MapTransformer.Empty): Exp[A] = {
    val Def(th: ThunkDef[A]) = thunk
    forceThunkDefByMirror(th, subst)
  }
  def forceThunkDefByMirror[A](th: ThunkDef[A], subst: MapTransformer = MapTransformer.Empty): Exp[A] = {
    val body = th.scheduleSyms
    val (t, _) = DefaultMirror.mirrorSymbols(subst, NoRewriting, th, body)
    t(th.root).asRep[A]
  }

  def thunk_force[A](t: Th[A]): Rep[A] =
    if (isInlineThunksOnForce)
      t match {
        case Def(th@ThunkDef(_, _)) =>
          forceThunkByMirror(t)
        case _ => ThunkForce(t)
      }
    else
      ThunkForce(t)

  case class ThunkForce[A](thunk: Exp[Thunk[A]]) extends Def[A] {
    implicit def selfType = thunk.elem.eItem
  }

  override def effectSyms(x: Any): List[Exp[Any]] = x match {
//    case ThunkDef(_, sch) =>
//      flatMapIterable(sch.map(_.sym), effectSyms)
    case _ => super.effectSyms(x)
  }

  override protected def matchDefs(d1: Def[_], d2: Def[_], allowInexactMatch: Boolean, subst: Subst): Option[Subst] = d1 match {
    case ThunkDef(root1, sch1) => d2 match {
      case ThunkDef(root2, sch2) =>
        matchExps(root1, root2, allowInexactMatch, subst).
          flatMap(matchIterators(sch1.iterator, sch2.iterator, allowInexactMatch, _))
      case _ => None
    }
    case _ =>
      super.matchDefs(d1, d2, allowInexactMatch, subst)
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    case th @ ThunkDef(HasViews(srcRes, iso: Iso[a,b]), _) => {
      implicit val eA = iso.eFrom
      implicit val eB = iso.eTo
      val newTh = Thunk { iso.from(forceThunkDefByMirror(th)) }   // execute original th as part of new thunk
      ThunkView(newTh)(iso)
    }
    case ThunkForce(HasViews(srcTh, Def(iso: ThunkIso[a, b]))) => {
      val innerIso = iso.innerIso
      implicit val eA = innerIso.eFrom
      innerIso.to(srcTh.asRep[Thunk[a]].force)
    }
    case _ => super.rewriteDef(d)
  }

  override protected def formatDef(d: Def[_])(implicit config: GraphVizConfig): String = d match {
    case ThunkDef(r, sch) => s"Thunk($r, [${sch.map(_.sym).mkString(",")}])"
    case _ => super.formatDef(d)
  }

  override protected def nodeColor(td: TypeDesc, d: Def[_])(implicit config: GraphVizConfig) = td match {
    case _: ThunkElem[_] => "red"
    case _ => super.nodeColor(td, d)
  }
}
