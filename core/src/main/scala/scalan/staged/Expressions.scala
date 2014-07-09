package scalan.staged

import annotation.unchecked.uncheckedVariance
import scalan.{Base, ScalanStaged}
import scala.language.{implicitConversions}
import scalan.common.Lazy

trait BaseExp extends Base { self: ScalanStaged =>

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
  type AnyExp = Exp[Any]
  type ExpSubst = AnyExp => AnyExp

  // this trait is mixed in Def[A]
  trait ReifiableObject[+T, +TImpl <: T] extends UserType[T @uncheckedVariance] {
    def name: String = getClass.getSimpleName
    def name[A](eA: Elem[A]): String = s"$name[${eA.name}]"
    def name[A,B](eA: Elem[A], eB: Elem[B]): String = s"$name[${eA.name},${eB.name}]"
    def uniqueOpId: String
    def mirror(f: Transformer): Rep[_]
    def decompose: Option[Rep[_]] = None
    def isScalarOp: Boolean = true
  }

  type Def[+A] = ReifiableObject[A,A]
  
  abstract class BaseDef[T](implicit val selfType: Elem[T]) extends Def[T] {
    lazy val self: Rep[T] = this
  }

  case class Const[T: Elem](x: T) extends BaseDef[T] {
    def uniqueOpId = toString
    override def mirror(t: Transformer): Rep[_] = self
    override def hashCode: Int = (41 + x.hashCode)

    override def equals(other: Any) =
      other match {
        case c @ Const(otherX) => selfType == c.selfType && (otherX match {
          case otherArr: Array[_] => x match {
            case arr: Array[_] =>
              arr.sameElements(otherArr)
            case _ => false
          }
          case _ => otherX == x
        })
        case _ => false
      }

    override def toString = "Const(" + (x match {
      case arr: Array[_] => arr.mkString("Array(", ", ", ")")
      case _ => x
    }) + ")"
  }

  trait UnOpBase[TArg,R] extends Def[R] {
    def arg: Rep[TArg]
    def copyWith(arg: Rep[TArg]): Rep[R]
    def opName: String
    override def toString = s"${this.getClass.getSimpleName}($arg)"
    lazy val uniqueOpId = name(arg.elem)
    lazy val self: Rep[R] = { 
      implicit val e = selfType
      this
    }
    override def mirror(t: Transformer) = {
      implicit val e = selfType
      copyWith(t(arg))
    }
  }

  trait BinOpBase[TArg,R] extends Def[R] {
    def lhs: Rep[TArg]
    def rhs: Rep[TArg]
    def copyWith(l: Rep[TArg], r: Rep[TArg]): Rep[R]
    def opName: String
    override def toString = s"${this.getClass.getSimpleName}($lhs, $rhs)"
    lazy val uniqueOpId = name(lhs.elem)
    override def mirror(t: Transformer) = {
      implicit val eT = selfType
      copyWith(t(lhs), t(rhs))
    }
    lazy val self: Rep[R] = {
      implicit val e = selfType
      this
    }
  }

  trait UnOp[T] extends UnOpBase[T,T]
  trait BinOp[T] extends BinOpBase[T,T]

  abstract class Transformer {
    def apply[A](x: Rep[A]): Rep[A]
    def isDefinedAt(x: Rep[_]): Boolean
    def domain: Set[Rep[_]]
    def apply[A](xs: Seq[Rep[A]]): Seq[Rep[A]] = xs map (e => apply(e))
    def apply[X,A](f: X=>Rep[A]): X=>Rep[A] = (z:X) => apply(f(z))
    def apply[X,Y,A](f: (X,Y)=>Rep[A]): (X,Y)=>Rep[A] = (z1:X,z2:Y) => apply(f(z1,z2))
  }

  trait TransformerOps[Ctx <: Transformer] {
    def empty: Ctx
    def add(ctx: Ctx, kv: (Rep[_], Rep[_])): Ctx
    def merge(ctx1: Ctx, ctx2: Ctx): Ctx = ctx2.domain.foldLeft(ctx1)((t,s) => add(t, (s, ctx2(s))))
  }

  implicit class TransformerEx[Ctx <: Transformer](self: Ctx)(implicit ops: TransformerOps[Ctx]) {
    def +(kv: (Rep[_], Rep[_])) = ops.add(self, kv)
    def ++(kvs: Map[Rep[_], Rep[_]]) = kvs.foldLeft(self)((ctx, kv) => ops.add(ctx,kv))
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
   * @param et Type descriptor of the resulting type of node d
   * @tparam T
   * @return The symbol of the graph which is semantically(up to rewrites) equivalent to d
   */
  protected[scalan] def toExp[T](d: Def[T], newSym: => Exp[T])(implicit et: LElem[T]): Exp[T]
  implicit def reifyObject[T:LElem](obj: ReifiableObject[_,T]): Rep[T] = toExp(obj.asInstanceOf[Def[T]], fresh[T])

  override def toRep[A](x: A)(implicit eA: Elem[A]) = eA match {
    case _: BaseElem[_] => Const(x)
    case _: ArrayElem[_] => Const(x)
    case _ => super.toRep(x)(eA)
  }

  object Def {
    def unapply[T](e: Exp[T]): Option[Def[T]] = findDefinition(e).map(_.rhs)
  }

  object Var {
    def unapply[T](e: Exp[T]): Option[Exp[T]] = e match {
      case Def(_) => None
      case _ => Some(e)
    }
  }
  object Exps {
    def unapply(xs: List[Any]): Option[List[Exp[_]]] = {
      val exps = xs map { a => a.asInstanceOf[Exp[_]] }
      Some(exps)
    }
  }
  object Elem {
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

  def decompose[T](d: Def[T]): Exp[_] = d.decompose match {
    case None => null
    case Some(sym) => sym.asInstanceOf[Exp[_]]
  }

  // dependencies
  def syms(e: Any): List[Exp[Any]] = e match {
    case s: Exp[_] => List(s)
    case p: Product => p.productIterator.toList.flatMap(syms(_))
    case _ => Nil
  }

  def dep(e: Exp[Any]): List[Exp[Any]] = e match {
    case Def(d: Product) => syms(d)
    case _ => Nil
  }
  def dep(e: Def[Any]): List[Exp[Any]] = e match {
    case d: Product => syms(d)
    case _ => Nil
  }

  implicit class ExpForSomeOps(symbol: Exp[_]) {
    def inputs: List[Exp[Any]] = dep(symbol)
    def getDeps: List[Exp[_]] = symbol match {
      case Def(lam: Lambda[_,_]) => lam.freeVars.toList
      case _ => this.inputs
    }
    def isLambda: Boolean = symbol match {
      case Def(_: Lambda[_, _]) => true
      case _ => false
    }
    def tp: TableEntry[_] = findDefinition(symbol).get
    def sameScopeAs(other: Exp[_]): Boolean = this.tp.lambda == other.tp.lambda

    def asPair[A,B,R](f: Elem[A] => Elem[B] => Rep[(A,B)] => R): R = {
      val elem = symbol.elem
      elem match {
        case _: PairElem[_,_] => {
          val pe = elem.asInstanceOf[PairElem[A,B]]
          f(pe.ea)(pe.eb)(symbol.asRep[(A,B)])
        }
        case _ => !!!(s"Symbol $symbol expected to have PairElem but it's ${elem.name}")
      }
    }

    def asFunc[A,B,R](f: Elem[A] => Elem[B] => Rep[A=>B] => R): R = {
      val elem = symbol.elem
      elem match {
        case _: FuncElem[_,_] => {
          val fe = elem.asInstanceOf[FuncElem[A,B]]
          f(fe.ea)(fe.eb)(symbol.asRep[A=>B])
        }
        case _ => !!!(s"Symbol $symbol expected to have FuncElem but it's ${elem.name}")
      }
    }

    def mirror(t: Transformer) = symbol match {
      case Def(d) => d.mirror(t)
      case _ => fresh(Lazy(symbol.elem.asInstanceOf[Elem[Any]]))
    }
  }

  implicit class DefForSomeOps(d: Def[_]) {
    def getDeps: List[Exp[_]] = d match {
      case lam: Lambda[_,_] => lam.freeVars.toList
      case _ => syms(d)
    }
  }

  def rewrite[T](s: Exp[T])(implicit eT: LElem[T]): Exp[_] = {
    null
  }
}

/**
 * The Expressions trait houses common AST nodes. It also manages a list of encountered Definitions which
 * allows for common sub-expression elimination (CSE).
 *
 * @since 0.1
 */
trait Expressions extends BaseExp { self: ScalanStaged =>
  /**
   * A Sym is a symbolic reference used internally to refer to expressions.
   */
  object Sym { private var currId = 0 }
  case class Sym[+T](id: Int = {Sym.currId += 1; Sym.currId})
                    (implicit et: LElem[T]) extends Exp[T]
  {
    override def elem: Elem[T @uncheckedVariance] = this match {
      case Def(d) => d.selfType.asInstanceOf[Elem[T]]
      case _ => et.value
    }
    def varName = "s" + id
    override def toString = {
      if (isDebug) toStringWithDefinition else varName
    }

    lazy val definition = findDefinition(this).map(_.rhs)
    def toStringWithDefinition = toStringWithType + definition.map(d => s" = $d").getOrElse("")
  }

  def fresh[T](implicit et: LElem[T]): Exp[T] = new Sym[T]()

  case class TableEntrySingle[T](sym: Exp[T], rhs: Def[T], lambda: Option[Exp[_]]) extends TableEntry[T]

  override val TableEntry = new TableEntryCompanion {
    def apply[T](sym: Exp[T], rhs: Def[T]) = new TableEntrySingle(sym, rhs, None)
    def apply[T](sym: Exp[T], rhs: Def[T], lam: Exp[_]) = new TableEntrySingle(sym, rhs, Some(lam))
    def unapply[T](tp: TableEntry[T]): Option[(Exp[T], Def[T])] = Some((tp.sym, tp.rhs))
    //def unapply[T](s: Exp[T]): Option[TableEntry[T]] = findDefinition(s)
  }

  private[this] var expToGlobalDefs: Map[Exp[_], TableEntry[_]] = Map.empty
  private[this] var defToGlobalDefs: Map[Def[_], TableEntry[_]] = Map.empty

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
   * @param et Type descriptor of the resulting type of node d
   * @tparam T
   * @return The symbol of the graph which is semantically(up to rewrites) equivalent to d
   */
  protected[scalan] def toExp[T](d: Def[T], newSym: => Exp[T])(implicit et: LElem[T]): Exp[T] = {
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

