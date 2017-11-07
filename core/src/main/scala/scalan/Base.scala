package scalan

import java.lang.reflect.{Constructor => Constr}
import java.util.{Objects, Arrays}

import com.github.kxbmap.configs.syntax._
import com.typesafe.config.{ConfigFactory, Config}
import com.typesafe.scalalogging.LazyLogging

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{mutable, TraversableOnce}
import scala.collection.mutable.ListBuffer
import scalan.compilation.GraphVizConfig
import scalan.util.{ParamMirror, ReflectionUtil}
import scala.reflect.runtime.universe._

/**
  * The Base trait houses common AST nodes. It also manages a list of encountered Definitions which
  * allows for common sub-expression elimination (CSE).
  */
trait Base extends LazyLogging { scalan: Scalan =>
  type Rep[+A] = Exp[A]
  type |[+A, +B] = Either[A, B]
  type IntRep = Rep[Int]
  type BoolRep = Rep[Boolean]
  type UnitRep = Rep[Unit]
  type NothingRep = Rep[Nothing]
  type ByteRep = Rep[Byte]
  type ShortRep = Rep[Short]
  type CharRep = Rep[Char]
  type LongRep = Rep[Long]
  type FloatRep = Rep[Float]
  type DoubleRep = Rep[Double]
  type :=>[-A, +B] = PartialFunction[A, B]
  type RFunc[-A,+B] = Rep[Function1[A,B]]
  type RPair[+A, +B] = Rep[(A,B)]

  // Consider if extra data should be Seq[Any] instead (change name in this case)
  class StagingException(message: String, cause: Throwable, val syms: Seq[Rep[_]]) extends
    RuntimeException(stagingExceptionMessage(message, syms), cause) {
    def this(message: String, syms: Seq[Rep[_]]) = this(message, null, syms)
  }

  class NotImplementedStagingException(message: String, syms: Seq[Rep[_]]) extends StagingException(message, null, syms)

  def ??? : Nothing = ???("Missing or incomplete implementation")
  def ???(value: Any, syms: Rep[_]*): Nothing = throw new NotImplementedStagingException(value.toString, syms)

  def !!! : Nothing = !!!("should not be called")
  def !!!(msg: String, syms: Rep[_]*): Nothing = throw new StagingException(msg, syms)
  def !!!(msg: String, e: Throwable, syms: Rep[_]*): Nothing = throw new StagingException(msg, e, syms)

  implicit class RepForSomeExtension(x: Rep[_]) {
    def asRep[T]: Rep[T] = x.asInstanceOf[Rep[T]]
  }
  implicit class RepExtension[A](x: Rep[A]) {
    def asValue: A = valueFromRep(x)
  }

  implicit def liftToRep[A:Elem](x: A): Rep[A] = toRep(x)

  trait Def[+T] extends Product {
    def selfType: Elem[T @uncheckedVariance]
    lazy val self: Rep[T] = reifyObject(this)

    override def equals(other: Any) = other match {
      // check that nodes correspond to same operation, have the same type, and the same arguments
      // alternative would be to include Elem fields into case class
      case other: Base#Def[_] =>
        (this eq other) || ({
          val cls1 = getClass
          val cls2 = other.getClass
          cls1 == cls2 || {
            def nameWithoutCGLib(clazz: Class[_]) = {
              val name = clazz.getName
              name.indexOf("$$EnhancerByCGLIB$$") match {
                case -1 => name
                case i => name.substring(0, i)
              }
            }

            cls1.getClassLoader == cls2.getClassLoader && nameWithoutCGLib(cls1) == nameWithoutCGLib(cls2)
          }
        } && productArity == other.productArity && {
          val len = productArity
          var i = 0
          var result = true
          while (result && i < len) {
            result = Objects.deepEquals(productElement(i), other.productElement(i))
            i += 1
          }
          result
        } && selfType.name == other.selfType.name)
      case _ => false
    }

    override lazy val hashCode = {
      val len = productArity
      var i = 0
      var result = 1
      while (i < len) {
        val element = productElement(i)
        val elementHashCode = element match {
          case null => 0
          case arr: Array[Object] => Arrays.deepHashCode(arr)
          case arr: Array[Int] => Arrays.hashCode(arr)
          case arr: Array[Long] => Arrays.hashCode(arr)
          case arr: Array[Float] => Arrays.hashCode(arr)
          case arr: Array[Double] => Arrays.hashCode(arr)
          case arr: Array[Boolean] => Arrays.hashCode(arr)
          case arr: Array[Byte] => Arrays.hashCode(arr)
          case arr: Array[Short] => Arrays.hashCode(arr)
          case arr: Array[Char] => Arrays.hashCode(arr)
          case _ => element.hashCode
        }
        result = 41 * result + elementHashCode
        i += 1
      }
      result
    }

    override def toString = {
      val sb = new StringBuilder
      sb.append(productPrefix)
      sb.append("(")
      val iterator = productIterator
      if (iterator.hasNext) {
        append(sb, iterator.next)
      }
      while (iterator.hasNext) {
        sb.append(", ")
        append(sb, iterator.next)
      }
      sb.append(")")
      sb.toString
    }

    private final def append(sb: StringBuilder, x: Any): Unit = {
      x match {
        case arr: Array[_] =>
          sb.append("Array(")
          if (arr.length > 0) {
            append(sb, arr(0))
            var i = 1
            while (i < arr.length) {
              sb.append(", ")
              append(sb, arr(i))
              i += 1
            }
          }
          sb.append(")")
        case s: String =>
          sb.append("\"")
          sb.append(s)
          sb.append("\"")
        case _ => sb.append(x)
      }
    }
  }

  object Def {
    def unapply[T](e: Rep[T]): Option[Def[T]] = def_unapply(e)
  }
  object && {
    def unapply[T](x: T): Option[(T,T)] = Some((x, x))
  }

  abstract class CompanionDef[T] extends Def[T] {
    override def productArity = 0
    override def productElement(n: Int) = !!!(s"productElement($n) called, but productArity = 0", self)
    override def canEqual(other: Any) = other.isInstanceOf[CompanionDef[_]]
  }

  // Allows using ConfigOps without importing com.github.kxbmap.configs.syntax._
  implicit def ConfigOps(x: Config) = new ConfigOps(x)
  def config = Base.config

  val cacheElems = true
  val cachePairs = true

  /**
    * constants/symbols (atomic)
    */
  abstract class Exp[+T] {
    def elem: Elem[T @uncheckedVariance]
    def varName: String

    private[scalan] var isRec = false
    def isRecursive: Boolean = isRec
    private[scalan] def isRecursive_=(b: Boolean) = { isRec = b }

    def isVar: Boolean = this match {
      case Def(_) => false
      case _ => true
    }

    def isConst: Boolean = this match {
      case Def(Const(_)) => true
      case _ => isCompanion
    }

    def isCompanion: Boolean = elem.isInstanceOf[CompanionElem[_]]

    def toStringWithDefinition: String
    def toStringWithType = varName + ":" + elem.name
    def show(): Unit = show(defaultGraphVizConfig)
    def show(emitMetadata: Boolean): Unit = show(defaultGraphVizConfig.copy(emitMetadata = emitMetadata))
    def show(config: GraphVizConfig): Unit = showGraphs(this)(config)
  }
  type Sym = Exp[_]

  abstract class BaseDef[+T](implicit val selfType: Elem[T @uncheckedVariance]) extends Def[T]

  case class Const[T](x: T)(implicit val eT: Elem[T]) extends BaseDef[T]

  abstract class Transformer {
    def apply[A](x: Rep[A]): Rep[A]
    def isDefinedAt(x: Rep[_]): Boolean
    def domain: Set[Rep[_]]
    def apply[A](xs: Seq[Rep[A]]): Seq[Rep[A]] = xs map (e => apply(e))
    def apply[X,A](f: X=>Rep[A]): X=>Rep[A] = (z:X) => apply(f(z))
    def apply[X,Y,A](f: (X,Y)=>Rep[A]): (X,Y)=>Rep[A] = (z1:X,z2:Y) => apply(f(z1,z2))
    def onlySyms[A](xs: List[Rep[A]]): List[Rep[A]] = xs map (e => apply(e)) collect { case e: Rep[A] => e }
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

  protected def stagingExceptionMessage(message: String, syms: Seq[Rep[_]]) = {
    // Skip syms already in the message, assume that's the only source for s<N>
    val symsNotInMessage = syms.map(_.toString).filterNot(message.contains)

    if (symsNotInMessage.isEmpty) {
      message
    } else {
      val between = if (message.isEmpty)
        ""
      else
        message.last match {
          // determine whether the message lacks ending punctuation
          case '.' | ';' | '!' | '?' => " "
          case _ => ". "
        }

      message + between + s"Sym${if (symsNotInMessage.length > 1) "s" else ""}: ${symsNotInMessage.mkString(", ")}"
    }
  }

  private[this] case class ReflectedProductClass(constructor: Constr[_], paramMirrors: List[ParamMirror], hasScalanParameter: Boolean)

  private[this] val baseType = typeOf[Base]

  private[this] def reflectProductClass(clazz: Class[_], d: Product) = {
    val constructors = clazz.getDeclaredConstructors
    assert(constructors.length == 1, s"Every class extending Def must have one constructor, $clazz has ${constructors.length}")
    val constructor = constructors(0)

    val paramMirrors = ReflectionUtil.paramMirrors(d)

    val hasScalanParam = constructor.getParameterTypes.headOption match {
      case None => false
      case Some(firstParamClazz) =>
        // note: classOf[Base].isAssignableFrom(firstParamClazz) can give wrong result due to the way
        // Scala compiles traits inheriting from classes
        val firstParamTpe = ReflectionUtil.classToSymbol(firstParamClazz).toType
        firstParamTpe <:< baseType
    }

    ReflectedProductClass(constructor, paramMirrors, hasScalanParam)
  }

  private[this] val defClasses = collection.mutable.Map.empty[Class[_], ReflectedProductClass]

  def transformDef[A](d: Def[A], t: Transformer): Rep[A] = d match {
    case c: Const[_] => c.self
    case comp: CompanionDef[_] => comp.self
    case _ =>
      val newD = transformProduct(d, t).asInstanceOf[Def[A]]
      reifyObject(newD)
  }

  protected def transformProductParam(x: Any, t: Transformer): Any = x match {
    case e: Rep[_] => t(e)
    case seq: Seq[_] => seq.map(transformProductParam(_, t))
    case arr: Array[_] => arr.map(transformProductParam(_, t))
    case opt: Option[_] => opt.map(transformProductParam(_, t))
    case p: Product if p.productArity != 0 => transformProduct(p, t)
    case x => x
  }

  def transformProduct(p: Product, t: Transformer): Product = {
    val clazz = p.getClass
    val ReflectedProductClass(constructor, paramMirrors, hasScalanParameter) =
      defClasses.getOrElseUpdate(clazz, reflectProductClass(clazz, p))

    val pParams = paramMirrors.map(_.bind(p).get)
    val transformedParams = pParams.map(transformProductParam(_, t))
    val finalParams =
      (if (hasScalanParameter)
        scalan :: transformedParams
      else
        transformedParams).asInstanceOf[List[AnyRef]]

    try {
      val transformedP = constructor.newInstance(finalParams: _*).asInstanceOf[Product]
      transformedP
    } catch {
      case e: Exception =>
        !!!(
          s"""
            |Failed to invoke constructor $clazz(${constructor.getParameterTypes.map(_.getSimpleName).mkString(", ")}) with parameters ${finalParams.mkString(", ")}
            |
             |Graph nodes have scalan cake as the first parameter ($$owner).
            |Check that the trait where class $clazz is defined extends Base.
            |""".stripMargin, e)
    }
  }

  implicit def reifyObject[A](obj: Def[A]): Rep[A] = {
    toExp(obj, fresh[A](Lazy(obj.selfType)))
  }

  def toRep[A](x: A)(implicit eA: Elem[A]):Rep[A] = eA match {
    case _: BaseElem[_] => Const(x)
    case _: FuncElem[_, _] => Const(x)
    case pe: PairElem[a, b] =>
      val x1 = x.asInstanceOf[(a, b)]
      implicit val eA = pe.eFst
      implicit val eB = pe.eSnd
      Pair(toRep(x1._1), toRep(x1._2))
    case se: SumElem[a, b] =>
      val x1 = x.asInstanceOf[a | b]
      implicit val eA = se.eLeft
      implicit val eB = se.eRight
      x1.fold(l => SLeft[a, b](l), r => SRight[a, b](r))
    case _ =>
      x match {
        // this may be called instead of reifyObject implicit in some cases
        case d: Base#Def[A @unchecked] => reifyObject(d.asInstanceOf[Def[A]])
        case _ => !!!(s"Don't know how to create Rep for $x with element $eA")
      }
  }

  def valueFromRep[A](x: Rep[A]): A = x match {
    case Def(Const(x)) => x
    case _ => delayInvoke
  }

  def def_unapply[T](e: Rep[T]): Option[Def[T]] = findDefinition(e).map(_.rhs)

  //  override def repDef_getElem[T <: Def[_]](x: Rep[T]): Elem[T] = x.elem
  //  override def rep_getElem[T](x: Rep[T]): Elem[T] = x.elem

  object Var {
    def unapply[T](e: Rep[T]): Option[Rep[T]] = e match {
      case Def(_) => None
      case _ => Some(e)
    }
  }

  object ExpWithElem {
    def unapply[T](s: Rep[T]): Option[(Rep[T],Elem[T])] = Some((s, s.elem))
  }

  /**
    * Used for staged methods which can't be implemented based on other methods.
    * This just returns a value of the desired type.
    */
  def defaultImpl[T](implicit elem: Elem[T]): Rep[T] = elem.defaultRepValue

  abstract class Stm // statement (links syms and definitions)

  implicit class StmOps(stm: Stm) {
    def lhs: List[Rep[Any]] = stm match {
      case TableEntry(sym, rhs) => sym :: Nil
    }

    def defines[A](sym: Rep[A]): Option[Def[A]] = stm match {
      case TableEntry(`sym`, rhs: Def[A] @unchecked) => Some(rhs)
      case _ => None
    }

    def defines[A](rhs: Def[A]): Option[Rep[A]] = stm match {
      case TableEntry(sym: Rep[A] @unchecked, `rhs`) => Some(sym)
      case _ => None
    }
  }

  trait TableEntry[+T] extends Stm {
    def sym: Rep[T]
    def lambda: Option[Rep[_]]
    def rhs: Def[T]
    def isLambda = rhs.isInstanceOf[Lambda[_, _]]
  }

  trait TableEntryCompanion {
    def apply[T](sym: Rep[T], rhs: Def[T]): TableEntry[T]
    def apply[T](sym: Rep[T], rhs: Def[T], lam: Rep[_]): TableEntry[T]
    def unapply[T](tp: TableEntry[T]): Option[(Rep[T], Def[T])]
  }

  object DefTableEntry {
    def unapply[T](e: Rep[T]): Option[TableEntry[T]] = findDefinition(e)
  }

  def decompose[T](d: Def[T]): Option[Rep[T]] = None

  def flatMapWithBuffer[A, T](iter: Iterator[A], f: A => TraversableOnce[T]): List[T] = {
    // performance hotspot: this is the same as
    // iter.toList.flatMap(f(_)) but faster
    val out = new ListBuffer[T]
    while (iter.hasNext) {
      val e = iter.next()
      out ++= f(e)
    }
    out.result()
  }

  def flatMapIterable[A, T](iterable: Iterable[A], f: A => TraversableOnce[T]) =
    flatMapWithBuffer(iterable.iterator, f)

  def flatMapProduct[T](p: Product, f: Any => TraversableOnce[T]): List[T] = {
    val iter = p.productIterator
    flatMapWithBuffer(iter, f)
  }

  // regular data (and effect) dependencies
  def syms(e: Any): List[Rep[_]] = e match {
    case s: Rep[_] => List(s)
    case s: Iterable[_] =>
      flatMapWithBuffer(s.iterator, syms)
    // All case classes extend Product!
    case p: Product =>
      flatMapProduct(p, syms)
    case _ => Nil
  }
  def dep(e: Rep[_]): List[Rep[_]] = e match {
    case Def(d) => syms(d)
    case _ => Nil
  }
  def dep(d: Def[_]): List[Rep[_]] = syms(d)

  // symbols which are bound in a definition
  def boundSyms(e: Any): List[Rep[Any]] = e match {
    case ss: Iterable[Any] => flatMapWithBuffer(ss.iterator, boundSyms)
    case p: Product => flatMapProduct(p, boundSyms)
    case _ => Nil
  }

  // symbols which are bound in a definition, but also defined elsewhere
  def tunnelSyms(e: Any): List[Rep[Any]] = e match {
    case ss: Iterable[Any] => flatMapWithBuffer(ss.iterator, tunnelSyms)
    case p: Product => flatMapProduct(p, tunnelSyms)
    case _ => Nil
  }

  // symbols of effectful components of a definition
  def effectSyms(x: Any): List[Rep[Any]] = x match {
    case ss: Iterable[Any] => flatMapWithBuffer(ss.iterator, effectSyms)
    case p: Product => flatMapProduct(p, effectSyms)
    case _ => Nil
  }

  // soft dependencies: they are not required but if they occur,
  // they must be scheduled before
  def softSyms(e: Any): List[Rep[Any]] = e match {
    // empty by default
    //case s: Rep[Any] => List(s)
    case ss: Iterable[Any] => flatMapWithBuffer(ss.iterator, softSyms)
    case p: Product => flatMapProduct(p, softSyms)
    case _ => Nil
  }

  // generic symbol traversal: f is expected to call rsyms again
  def rsyms[T](e: Any)(f: Any=>List[T]): List[T] = e match {
    case s: Rep[Any] => f(s)
    case ss: Iterable[Any] => flatMapWithBuffer(ss.iterator, f)
    case p: Product => flatMapProduct(p, f)
    case _ => Nil
  }

  // frequency information for dependencies: used/computed
  // often (hot) or not often (cold). used to drive code motion.
  def symsFreq(e: Any): List[(Rep[Any], Double)] = e match {
    case s: Rep[Any] => List((s,1.0))
    case ss: Iterable[Any] => flatMapWithBuffer(ss.iterator, symsFreq)
    case p: Product => flatMapProduct(p, symsFreq)
    //case _ => rsyms(e)(symsFreq)
    case _ => Nil
  }

  def freqNormal(e: Any) = symsFreq(e)
  def freqHot(e: Any) = symsFreq(e).map(p=>(p._1,p._2*1000.0))
  def freqCold(e: Any) = symsFreq(e).map(p=>(p._1,p._2*0.5))

  implicit class ExpForSomeOps(symbol: Rep[_]) {
    def inputs: List[Rep[Any]] = dep(symbol)
    def getDeps: List[Rep[_]] = symbol match {
      case Def(g: AstGraph) => g.freeVars.toList
      case _ => this.inputs
    }

    /** Shallow dependencies don't look into branches of IfThenElse  */
    def getShallowDeps: List[Sym] = symbol match {
      case Def(IfThenElse(c, _, _)) => List(c)
      case _ => getDeps
    }

    def isLambda: Boolean = symbol match {
      case Def(_: Lambda[_, _]) => true
      case _ => false
    }
    def tp: TableEntry[_] = findDefinition(symbol).getOrElse {
      !!!(s"No definition found for $symbol", symbol)
    }
    def sameScopeAs(other: Rep[_]): Boolean = this.tp.lambda == other.tp.lambda
  }

  implicit class DefForSomeOps(d: Def[_]) {
    def getDeps: List[Rep[_]] = d match {
      case g: AstGraph => g.freeVars.toList
      case _ => syms(d)
    }

    def asDef[T] = d.asInstanceOf[Def[T]]
  }

  case class HasArg(predicate: Rep[_] => Boolean) {
    def unapply[T](d: Def[T]): Option[Def[T]] = {
      val args = dep(d)
      if (args.exists(predicate)) Some(d) else None
    }
  }

  case class FindArg(predicate: Rep[_] => Boolean) {
    def unapply[T](d: Def[T]): Option[Rep[_]] = {
      val args = dep(d)
      for { a <- args.find(predicate) } yield a
    }
  }

  def rewrite[T](s: Rep[T]): Rep[_] = s match {
    case Def(d) => rewriteDef(d)
    case _ => rewriteVar(s)
  }

  def rewriteDef[T](d: Def[T]): Rep[_] = null

  def rewriteVar[T](s: Rep[T]): Rep[_] = null

  /**
    * A Sym is a symbolic reference used internally to refer to expressions.
    */
  object SingleSym {
    private var currId = 0
    def fresh[T: LElem]: Rep[T] = {
      currId += 1
      SingleSym(currId)
    }
  }
  case class SingleSym[+T](id: Int)(implicit private val eT: LElem[T @uncheckedVariance]) extends Rep[T] {
    override def elem: Elem[T @uncheckedVariance] = this match {
      case Def(d) => d.selfType
      case _ => eT.value
    }
    def varName = "s" + id
    override def toString = varName

    lazy val definition = findDefinition(this).map(_.rhs)
    def toStringWithDefinition = toStringWithType + definition.map(d => s" = $d").getOrElse("")
  }

  def fresh[T: LElem]: Rep[T] = SingleSym.fresh[T]

  case class TableEntrySingle[T](sym: Rep[T], rhs: Def[T], lambda: Option[Rep[_]]) extends TableEntry[T]

  val TableEntry: TableEntryCompanion = new TableEntryCompanion {
    def apply[T](sym: Rep[T], rhs: Def[T]) = new TableEntrySingle(sym, rhs, None)
    def apply[T](sym: Rep[T], rhs: Def[T], lam: Rep[_]) = new TableEntrySingle(sym, rhs, Some(lam))
    def unapply[T](tp: TableEntry[T]): Option[(Rep[T], Def[T])] = Some((tp.sym, tp.rhs))
    //def unapply[T](s: Rep[T]): Option[TableEntry[T]] = findDefinition(s)
  }
  protected val globalThunkSym: Rep[_] = fresh[Int] // we could use any type here
  private[this] val expToGlobalDefs: mutable.Map[Rep[_], TableEntry[_]] = mutable.HashMap.empty
  private[this] val defToGlobalDefs: mutable.Map[(Rep[_], Def[_]), TableEntry[_]] = mutable.HashMap.empty

  def findDefinition[T](s: Rep[T]): Option[TableEntry[T]] =
    expToGlobalDefs.get(s).asInstanceOf[Option[TableEntry[T]]]

  def findDefinition[T](thunk: Rep[_], d: Def[T]): Option[TableEntry[T]] =
    defToGlobalDefs.get((thunk,d)).asInstanceOf[Option[TableEntry[T]]]

  def findOrCreateDefinition[T](d: Def[T], newSym: => Rep[T]): Rep[T] = {
    val optScope = thunkStack.top
    val optFound = optScope match {
      case Some(scope) =>
        scope.findDef(d)
      case None =>
        findDefinition(globalThunkSym, d)
    }
    val te = optFound.getOrElse {
      createDefinition(optScope, newSym, d)
    }
    assert(te.rhs == d, s"${if (optFound.isDefined) "Found" else "Created"} unequal definition ${te.rhs} with symbol ${te.sym.toStringWithType} for $d")
    te.sym

  }

  def createDefinition[T](s: Rep[T], d: Def[T]): TableEntry[T] =
    createDefinition(thunkStack.top, s, d)

  private def createDefinition[T](optScope: Option[ThunkScope], s: Rep[T], d: Def[T]): TableEntry[T] = {
    val te = lambdaStack.top match {
      case Some(fSym) => TableEntry(s, d, fSym)
      case _ => TableEntry(s, d)
    }
    optScope match {
      case Some(scope) =>
        defToGlobalDefs += (scope.thunkSym, te.rhs) -> te
        scope += te
      case None =>
        defToGlobalDefs += (globalThunkSym, te.rhs) -> te
    }

    expToGlobalDefs += te.sym -> te
    te
  }

  /**
    * Updates the universe of symbols and definitions, then rewrites until fix-point
    * @param d A new graph node to add to the universe
    * @param newSym A symbol that will be used if d doesn't exist in the universe
    * @tparam T
    * @return The symbol of the graph which is semantically(up to rewrites) equivalent to d
    */
  protected[scalan] def toExp[T](d: Def[T], newSym: => Rep[T]): Rep[T] = {
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

  object IdSupply {
    private var _nextId = 0

    def nextId = {
      _nextId += 1
      _nextId
    }
  }
}
object Base {
  // Hacky way to make plugin config avaialable here. It probably shouldn't be, but
  // for now Gcc's initialization fails without it. If we decide it is, move logic from
  // Plugins to here.
  private[scalan] val config0 = ConfigFactory.load().getConfig("scalan")
  val config = Plugins.configWithPlugins
}

