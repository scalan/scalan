package scalan.staged

import annotation.unchecked.uncheckedVariance
import scalan.{Base, ScalanExp}
import scala.language.{implicitConversions}
import scalan.common.Lazy
import scala.collection.immutable.ListMap

trait BaseExp extends Base { self: ScalanExp =>
  type Rep[+A] = Exp[A]

  /**
   * constants/symbols (atomic)
   */
  abstract class Exp[+T] {
    def elem: Elem[T @uncheckedVariance]

    private[scalan] var isRec = false
    def isRecursive: Boolean = isRec
    private[scalan] def isRecursive_=(b: Boolean) = { isRec = b }

    def isVar: Boolean = this match {
      case Def(_) => false
      case _ => true
    }

    def isConst: Boolean = this match {
      case Def(Const(_)) => true
      case _ => false
    }
    def varName: String
    def toStringWithType = varName + ":" + elem.name
    def toStringWithDefinition: String
  }
  type ExpAny = Exp[_]

  // this trait is mixed in Def[A]
  trait ReifiableExp[+T, +TImpl <: T] extends Reifiable[T] {
    lazy val self: Rep[T] = reifyObject(this)
    def name: String = getClass.getSimpleName
    def name[A](eA: Elem[A]): String = s"$name[${eA.name}]"
    def name[A,B](eA: Elem[A], eB: Elem[B]): String = s"$name[${eA.name},${eB.name}]"
    def uniqueOpId: String
    def mirror(f: Transformer): Rep[T]
    def decompose: Option[Rep[T]] = None
  }

  type Def[+A] = ReifiableExp[A,A]
  
  abstract class BaseDef[T](implicit val selfType: Elem[T]) extends Def[T]

  case class Const[T: Elem](x: T) extends BaseDef[T] {
    def uniqueOpId = toString
    override def mirror(t: Transformer) = self
  }

  abstract class Transformer {
    def apply[A](x: Rep[A]): Rep[A]
    def isDefinedAt(x: Rep[_]): Boolean
    def domain: Set[Rep[_]]
    def apply[A](xs: Seq[Rep[A]]): Seq[Rep[A]] = xs map (e => apply(e))
    def apply[X,A](f: X=>Rep[A]): X=>Rep[A] = (z:X) => apply(f(z))
    def apply[X,Y,A](f: (X,Y)=>Rep[A]): (X,Y)=>Rep[A] = (z1:X,z2:Y) => apply(f(z1,z2))
  }
  def IdTransformer = MapTransformer.Empty

  trait TransformerOps[Ctx <: Transformer] {
    def empty: Ctx
    def add[A](ctx: Ctx, kv: (Rep[A], Rep[A])): Ctx
    def merge(ctx1: Ctx, ctx2: Ctx): Ctx = ctx2.domain.foldLeft(ctx1) {
      case (t, s: Rep[a]) => add(t, (s, ctx2(s)))
    }
  }

  implicit class TransformerEx[Ctx <: Transformer](self: Ctx)(implicit ops: TransformerOps[Ctx]) {
    def +[A](kv: (Rep[A], Rep[A])) = ops.add(self, kv)
    def ++(kvs: Map[Rep[A], Rep[A]] forSome {type A}) = kvs.foldLeft(self)((ctx, kv) => ops.add(ctx, kv))
    def merge(other: Ctx): Ctx = ops.merge(self, other)
  }

  def fresh[T](implicit leT: LElem[T]): Exp[T]
  def findDefinition[T](s: Exp[T]): Option[TableEntry[T]]
  def findDefinition[T](d: Def[T]): Option[TableEntry[T]]
  def createDefinition[T](s: Exp[T], d: Def[T]): TableEntry[T]

  /**
   * Updates the universe of symbols and definitions, then rewrites until fix-point
   * @param d A new graph node to add to the universe
   * @param newSym A symbol that will be used if d doesn't exist in the universe
   * @tparam T Type of the result
   * @return The symbol of the graph which is semantically(up to rewrites) equivalent to d
   */
  protected[scalan] def toExp[T](d: Def[T], newSym: => Exp[T]): Exp[T]
  implicit def reifyObject[T](obj: ReifiableExp[_,T]): Rep[T] = {
    // TODO bad cast
    val obj1 = obj.asInstanceOf[Def[T]]
    toExp(obj1, fresh[T](Lazy(obj1.selfType)))
  }

  override def toRep[A](x: A)(implicit eA: Elem[A]) = eA match {
    case _: BaseElem[_] => Const(x)
    case _: FuncElem[_, _] => Const(x)
    case _: ArrayElem[_] => Const(x)
    case pe: PairElem[a, b] =>
      val x1 = x.asInstanceOf[(a, b)]
      implicit val eA = pe.eFst
      implicit val eB = pe.eSnd
      Pair(toRep(x1._1), toRep(x1._2))
    case se: SumElem[a, b] =>
      val x1 = x.asInstanceOf[a | b]
      implicit val eA = se.eLeft
      implicit val eB = se.eRight
      x1.fold(l => Left[a, b](l), r => Right[a, b](r))
    case _ =>
      x match {
        // this may be called instead of reifyObject implicit in some cases
        case d: ReifiableExp[_, A @unchecked] => reifyObject(d)
        case _ => super.toRep(x)(eA)
      }
  }

  def def_unapply[T](e: Exp[T]): Option[Def[T]] = findDefinition(e).map(_.rhs)

  override def repReifiable_getElem[T <: Reifiable[T]](x: Rep[T]): Elem[T] = x.elem

  object Var {
    def unapply[T](e: Exp[T]): Option[Exp[T]] = e match {
      case Def(_) => None
      case _ => Some(e)
    }
  }

  object ExpWithElem {
    def unapply[T](s: Exp[T]): Option[(Exp[T],Elem[T])] = Some((s, s.elem))
  }

  trait TableEntry[+T] {
    def sym: Exp[T]
    def lambda: Option[Exp[_]]
    def rhs: Def[T]
    def isLambda = rhs.isInstanceOf[Lambda[_, _]]
  }

  trait TableEntryCompanion {
    def apply[T](sym: Exp[T], rhs: Def[T]): TableEntry[T]
    def apply[T](sym: Exp[T], rhs: Def[T], lam: Exp[_]): TableEntry[T]
    def unapply[T](tp: TableEntry[T]): Option[(Exp[T], Def[T])]
  }

  val TableEntry: TableEntryCompanion = null

  object DefTableEntry {
    def unapply[T](e: Exp[T]): Option[TableEntry[T]] = findDefinition(e)
  }

  def decompose[T](d: Def[T]): Exp[T] = d.decompose match {
    case None => null
    case Some(sym) => sym
  }

  // dependencies
  def syms(e: Any): List[Exp[_]] = e match {
    case s: Exp[_] => List(s)
    case s: Seq[_] => s.toList.flatMap(syms(_))
    case p: Product => p.productIterator.toList.flatMap(syms(_))
    case _ => Nil
  }

  def dep(e: Exp[_]): List[Exp[_]] = e match {
    case Def(d: Product) => syms(d)
    case _ => Nil
  }
  def dep(e: Def[_]): List[Exp[_]] = e match {
    case d: Product => syms(d)
    case _ => Nil
  }

  implicit class ExpForSomeOps(symbol: Exp[_]) {
    def inputs: List[Exp[Any]] = dep(symbol)
    def getDeps: List[Exp[_]] = symbol match {
      case Def(lam: Lambda[_,_]) => lam.freeVars.toList
      case _ => this.inputs
    }

    /** Shallow dependencies don't look into branches of IfThenElse  */
    def getShallowDeps: List[ExpAny] = symbol match {
      case Def(IfThenElse(c, _, _)) => List(c)
      case _ => getDeps
    }

    def isLambda: Boolean = symbol match {
      case Def(_: Lambda[_, _]) => true
      case _ => false
    }
    def tp: TableEntry[_] = findDefinition(symbol).get
    def sameScopeAs(other: Exp[_]): Boolean = this.tp.lambda == other.tp.lambda

    def mirror(t: Transformer) = symbol match {
      case Def(d) => d.mirror(t)
      case _ => fresh(Lazy(symbol.elem))
    }
  }

  implicit class DefForSomeOps(d: Def[_]) {
    def getDeps: List[Exp[_]] = d match {
      case lam: Lambda[_,_] => lam.freeVars.toList
      case _ => syms(d)
    }

    def asDef[T] = d.asInstanceOf[Def[T]]
  }

  final def rewrite[T](s: Exp[T]): Exp[_] = s match {
    case Def(d) => rewriteDef(d)
    case _ => rewriteVar(s)
  }

  def rewriteDef[T](d: Def[T]): Exp[_] = null

  def rewriteVar[T](s: Exp[T]): Exp[_] = null
}

/**
 * The Expressions trait houses common AST nodes. It also manages a list of encountered Definitions which
 * allows for common sub-expression elimination (CSE).
 *
 * @since 0.1
 */
trait Expressions extends BaseExp { self: ScalanExp =>
  /**
   * A Sym is a symbolic reference used internally to refer to expressions.
   */
  object Sym { private var currId = 0 }
  case class Sym[+T](id: Int = {Sym.currId += 1; Sym.currId})
                    (implicit et: LElem[T]) extends Exp[T]
  {
    override def elem: Elem[T @uncheckedVariance] = this match {
      case Def(d) => d.selfType
      case _ => et.value
    }
    def varName = "s" + id
    override def toString = varName

    lazy val definition = findDefinition(this).map(_.rhs)
    def toStringWithDefinition = toStringWithType + definition.map(d => s" = $d").getOrElse("")
  }

  def fresh[T](implicit et: LElem[T]): Exp[T] = new Sym[T]()

  case class TableEntrySingle[T](sym: Exp[T], rhs: Def[T], lambda: Option[Exp[_]]) extends TableEntry[T]

  override val TableEntry: TableEntryCompanion = new TableEntryCompanion {
    def apply[T](sym: Exp[T], rhs: Def[T]) = new TableEntrySingle(sym, rhs, None)
    def apply[T](sym: Exp[T], rhs: Def[T], lam: Exp[_]) = new TableEntrySingle(sym, rhs, Some(lam))
    def unapply[T](tp: TableEntry[T]): Option[(Exp[T], Def[T])] = Some((tp.sym, tp.rhs))
    //def unapply[T](s: Exp[T]): Option[TableEntry[T]] = findDefinition(s)
  }

  private[this] var expToGlobalDefs: Map[Exp[_], TableEntry[_]] = ListMap.empty
  private[this] var defToGlobalDefs: Map[Def[_], TableEntry[_]] = ListMap.empty

  def findDefinition[T](s: Exp[T]): Option[TableEntry[T]] =
    expToGlobalDefs.get(s).asInstanceOf[Option[TableEntry[T]]]

  def findDefinition[T](d: Def[T]): Option[TableEntry[T]] =
    defToGlobalDefs.get(d).asInstanceOf[Option[TableEntry[T]]]

  def findOrCreateDefinition[T](d: Def[T], newSym: => Exp[T]): Exp[T] = {
    val res = findDefinition(d) match {
      case Some(TableEntry(s, _)) => s
      case None =>
        val TableEntry(s, _) = createDefinition(newSym, d)
        s
    }
    res
  }

  def createDefinition[T](s: Exp[T], d: Def[T]): TableEntry[T] = {
    val tp = lambdaStack.top match {
      case Some(fSym) => TableEntry(s, d, fSym)
      case _ => TableEntry(s, d)
    }
    expToGlobalDefs += tp.sym -> tp
    defToGlobalDefs += tp.rhs -> tp
    tp
  }

  /**
   * Updates the universe of symbols and definitions, then rewrites until fix-point
   * @param d A new graph node to add to the universe
   * @param newSym A symbol that will be used if d doesn't exist in the universe
   * @tparam T
   * @return The symbol of the graph which is semantically(up to rewrites) equivalent to d
   */
  protected[scalan] def toExp[T](d: Def[T], newSym: => Exp[T]): Exp[T] = {
    var res = findOrCreateDefinition(d, newSym)
    var currSym = res
    var currDef = d
    do {
      currSym = res
      val ns = rewrite(currSym).asRep[T]
      ns match {
        case null =>
          currDef = null
        case Def(someOtherD) =>
          res = ns
          currDef = someOtherD
        case _ =>
          res = ns
          currDef = null
      }
    } while (res != currSym && currDef != null)
    res
  }
}

